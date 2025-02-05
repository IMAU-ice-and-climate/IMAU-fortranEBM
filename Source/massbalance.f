!===============================================================================
!
! File with routines and functions related to using sonic altimeter data to limit
! the accumulation to observed values 
! and use it for evaluation of the modelled snow and ice melt. 
!
!===============================================================================
SUBROUTINE GETACCUMANDMELT(iyr)
!===============================================================================
! Determines accumulation on ice and ice melt based in input sonic altimeter time series (17)
! and puts them in columns 19 = accum, 20 = melt , 21 = drift
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast , buf 
USE SNOW_EBM , ONLY : racc_old , acclevel , meltlevel 
USE INPUT_EBM , ONLY : ibyear
USE CONSTANTS_EBM , ONLY : errorval 

IMPLICIT NONE

! input
INTEGER :: iyr
! Local
INTEGER :: ii
INTEGER :: maxcnt , meltcnt
REAL :: rserie , racc , rdrift , rsummelt , rmelt

maxcnt = 30*24		!max number of samples delay with reset of meltlevel, prevents jumping behaviour		
meltcnt = 0

IF (iyr == ibyear) THEN
  racc_old = buf(1,16)
  acclevel = 0		! raccm1
  meltlevel = 0		! rmelt1
ENDIF

DO ii = 1,ilast		! per calendar year
  
  rserie = buf(ii,16)
  
  IF (rserie > errorval) THEN
    IF (rserie > acclevel) THEN
      rsummelt = acclevel
      racc = rserie - acclevel      
      meltcnt = meltcnt + 1
      IF (meltcnt > maxcnt) meltlevel=acclevel
    ELSE
      rsummelt = rserie
      acclevel = rserie
      racc = 0.
      meltcnt = 0
    ENDIF
    rdrift = MIN(0.,(racc - racc_old))
    racc_old = racc
!    rmelt = rsummelt-meltlevel
  ENDIF

  buf(ii,18) = racc
  buf(ii,19) = rsummelt		!rmelt
  buf(ii,20) = rdrift

  ENDDO

END SUBROUTINE GETACCUMANDMELT

!===============================================================================
SUBROUTINE CLIMACCUMANDMELT(iyr)
!===============================================================================
! Determines accumulation on ice and ice melt based in input sonic altimeter time series (17)
! and puts them in columns 19 = accum, 20 = melt , 21 = drift
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast , buf 
USE SNOW_EBM , ONLY : rhosn , climserie , climracc_old , climacclevel , climmeltlevel 
USE INPUT_EBM , ONLY : ibyear, lclimtemp , trhoprec , rhosnprec , lclimprec , climprec
USE CONSTANTS_EBM , ONLY : errorval , Tkel 

IMPLICIT NONE

! input
INTEGER :: iyr
! Local
INTEGER :: ii
INTEGER :: maxcnt , meltcnt
REAL :: rserie , racc , rdrift , rsummelt , rmelt , rprecip
REAL :: dens , densprec
REAL :: sum

maxcnt = 30*24		!max number of samples delay with reset of meltlevel, prevents jumping behaviour		
meltcnt = 0

IF (iyr == ibyear) THEN
  climracc_old = buf(1,16)
  climserie = buf(1,16)
  climacclevel = 0		! raccm1
  climmeltlevel = 0		! rmelt1
ENDIF

sum = climserie

DO ii = 1,ilast		! per calendar year
!  rserie = buf(ii,16)
!  racc = buf(ii,18)
  rsummelt = buf(ii,19)
  rdrift = buf(ii,20)

! modify serie, acc and drift in case climate sensitivity tests depending on temperature
  IF (trhoprec == 0) THEN		! constant value set in input
    dens = rhosnprec
  ELSE IF (trhoprec == 1) THEN	! value depends on wind speed and surface temperature
    dens = densprec(buf(ii,6),buf(ii,4))
  ENDIF
  rprecip = buf(ii,17)/dens		! 17 = precip

!  IF (lclimtemp == 1) THEN 
    IF (buf(ii,4) <= Tkel + 0.5) THEN
!     nothing happens
    ELSE IF (buf(ii,4) >= Tkel + 2.5) THEN		! precip turned into water
      rprecip = 0.
    ELSE
      rprecip = 0.5*(Tkel + 2.5 - buf(ii,4))*rprecip
    ENDIF
!  ENDIF
  
  rprecip = rprecip*(1.+lclimprec*climprec*0.01)
  
  sum = sum + rprecip + rdrift
  climserie = sum + rsummelt
  IF (climserie > errorval) THEN
    IF (climserie > climacclevel) THEN
      rsummelt = climacclevel
      racc = climserie - climacclevel      
      meltcnt = meltcnt + 1
      IF (meltcnt > maxcnt) climmeltlevel=climacclevel
    ELSE
      rsummelt = climserie
      climacclevel = climserie
      racc = 0.
      meltcnt = 0
    ENDIF
    rdrift = MIN(0.,(racc - climracc_old))
    climracc_old = racc
!    rmelt = rsummelt-meltlevel
  ENDIF

  buf(ii,16) = climserie
  buf(ii,18) = racc
  buf(ii,17) = buf(ii,17)*(1.+lclimprec*climprec*0.01)
  buf(ii,19) = rsummelt		!rmelt
  buf(ii,20) = rdrift

!    WRITE(99,*) ii,rserie,racc,rsummelt,rdrift,rprecip,climserie
ENDDO

END SUBROUTINE CLIMACCUMANDMELT

!===============================================================================
SUBROUTINE SET_PRECIP
!===============================================================================
! Routine that sets height of sensors -> station dependend in some cases !!!!!
!===============================================================================
USE GLOBALS_EBM , ONLY : sbuf ,  t0 , t2m
USE SNOW_EBM , ONLY : precip, rhosn, water, freshfrac!, precipsum
USE INPUT_EBM , ONLY : tpdens , rhosnprec, trhoprec , lclimtemp
USE CONSTANTS_EBM , ONLY : Tkel , errorval

IMPLICIT NONE

!local
REAL :: densprec 

! determine density of snow fall in order to convert precip in mm we to m snow
IF (tpdens > 0) THEN
  IF (trhoprec == 0) THEN		! constant value set in input
    rhosn = rhosnprec
  ELSE IF (trhoprec == 1) THEN	! value depends on wind speed and surface temperature
    rhosn = densprec(sbuf(6),t0)		! based on surf temp or 2m temp, or obs temp?
!    rhosn = densprec(sbuf(6),t2m)
!    rhosn = densprec(sbuf(6),sbuf(4))
  ENDIF
!ELSE
! rhosn set at initialisation when dens profile is set 
ENDIF

precip = sbuf(17)/rhosn		! convert to m snow

! modify precipitation in case climate sensitivity tests depending on temperature
IF (lclimtemp == 1) THEN  
  IF (sbuf(4) <= Tkel + 0.5) THEN
!   nothing happens
  ELSE IF (sbuf(4) >= Tkel + 2.5) THEN		! precip turned into water added to first layer water content
    water(1) = water(1) + precip*rhosn
    precip = 0.
  ELSE
    water(1) = water(1) + (1 - 0.5*(Tkel + 2.5 - sbuf(4)))*precip*rhosn    
    precip = 0.5*(Tkel + 2.5 - sbuf(4))*precip
  ENDIF
ENDIF

!precipsum = precipsum + precip*rhosn		!again in mm we


END SUBROUTINE SET_PRECIP

!===============================================================================
SUBROUTINE SET_HEIGHT
!===============================================================================
! Routine that corrects accumulation height in case acc restricted to obs !!!!!
!===============================================================================
USE GLOBALS_EBM , ONLY : t0 , sbuf
USE SNOW_EBM , ONLY : acc , drift , dsnowh , dsnowr , precip , hsnow, corrsnow , &
&                     rhosn, icemelt, melt_l, & !, precipsum
&                     temp , water , mass , ice , dens , dz , z , lid , nl , nlsnow 
USE INPUT_EBM , ONLY : tstep , dz0 , luseacc , lcomment
USE CONSTANTS_EBM , ONLY : Tkel

IMPLICIT NONE

!local
INTEGER :: il , illost
REAL :: hu , llprec
REAL :: obsmelt
REAL :: corrdrift , corrmelt 

! sbuf(16) = (running average) sonic height measurement in serie
! sbuf(18) = (running average) acc measurement determined from sonic height
! dsnow = snow thickness at start, equals series sonic height ranger at start
! dsnowh = acc as moddeled
! acc = acc observed
! hsnow = sonic series observations, set in interp_data
! hsnowmod_l = snow depth previous time step model

hu = 0.01				! estimated uncertainty in 1 sonic height observations
llprec = hu*tstep/(24*3600.)		! equals hu in case tstep is 1 hour

acc = sbuf(18)
obsmelt = sbuf(19)
!drift = sbuf(20)	already defined in interp_data

IF (luseacc == 2) THEN
  ! luseacc = 2, acc is restricted to observed accumulation
   IF ((dsnowh + dsnowr) - acc >= 0.) THEN		! current model acc > observed acc, no use to add anymore snow
     IF (t0 < 270.) THEN
       drift = acc - (dsnowh + dsnowr)				! and later on remove the too much snow, assuming it to be drift
       precip = 0.     							! 17 = precip in mm we. 
     ELSE
       drift = 0.
       precip = 0.
     ENDIF
   ELSE IF ((dsnowh + dsnowr) - acc < 0.) THEN	! current model acc < observed acc, can add a bit more
      drift = acc - (dsnowh + dsnowr)		! tmp param > 0 because observed acc > modelled
      IF (precip >= drift) THEN
         precip = drift
         drift = 0.   ! drift - precip
      ELSE
         drift = 0.
      ENDIF
   ENDIF
  
  !! IF (dsnowh - acc > 0.01*dz0) THEN	!in case measured acc < modelled acc
  ! IF ((dsnowh + dsnowr) - acc >= 0.) THEN		! current model acc > observed acc, not allowed to add anymore snow
  !  IF (precip > 0.) THEN
  !    drift = acc - (dsnowh + dsnowr)				! and later on remove the too much snow, assuming it to be drift
  !    precip = 0.     							! 17 = precip in mm we. 
  !  ELSE IF (drift < -2.*llprec .and. t0 < 270.) THEN	!Only adjust in case there is no melt and the initial drift islarge
  !    drift = -0.5*(dsnowh-acc)
  !  ELSE 
  !    drift = 0.
  !  ENDIF
  ! ELSE
  !  drift = 0.
  ! ENDIF
  
  ELSE IF (luseacc == 3) THEN
  ! in case of luseacc = 3 , the snow accumulation is completely fixed by observed sonic ranger data
   IF ((dsnowh + dsnowr) - acc >= 0.) THEN		! current model acc > observed acc, no use to add anymore snow
      drift = acc - (dsnowh + dsnowr)				! and later on in snowmodel remove the too much snow, assuming it to be drift
      precip = 0.     							! 17 = precip in mm we. 
   ELSE IF ((dsnowh + dsnowr) - acc < 0.) THEN	! current model acc < observed acc, can add a bit more
      drift = acc - (dsnowh + dsnowr)		! tmp param > 0 because observed acc > modelled
      IF (precip >= drift) THEN
         precip = drift
         drift = drift - precip
      ELSE IF (precip < drift .and. drift > 2.*llprec) THEN
         drift = 0.5*(acc - dsnowh)
      ENDIF
  !    IF (precip >= drift) THEN     
  !      precip = drift
  !      drift = 0
  !    ELSE 
  !      drift = 0.
  !    ENDIF     
   ENDIF
  ENDIF


! CORRECTION of hsnowmod after period with no data 
IF (corrsnow /= 0.) THEN
  corrdrift = dsnowh-acc
  corrmelt = obsmelt - melt_l		!> 0 then ice melt has occuredRetMIP_surface_forcing_KANU
  IF (corrdrift < 0.) precip = precip - corrdrift
  
  IF (corrmelt < 0.) icemelt = icemelt + corrmelt
  IF (corrsnow > 0.) dsnowh = dsnowh + corrsnow 

  corrdrift = -corrdrift*rhosn
  il = 1
  DO WHILE (corrdrift < 0.)
   mass(il) = mass(il) + corrdrift
   IF (mass(il) < 0.) THEN
    corrdrift = mass(il)
    il = il+1
   ELSE
    corrdrift = 0.
   ENDIF
  ENDDO 
  IF (il > 1) THEN
   illost = il-1
   IF (lcomment == 1) write(*,*) il-1,' Layer(s) removed to correct period with no observations'
   DO il=1,nl-illost
    temp(il) = temp(il+illost)
    water(il) = water(il+illost)
    ice(il) = ice(il+illost)
    mass(il) = mass(il+illost)
    dens(il) = dens(il+illost)
    dz(il) = dz(il+illost)
    IF (il == 1) z(il) = 0.5*dz(il)
    IF (il > 1) z(il) = 0.5*dz(il) + z(il-1)
    lid(il) = lid(il+illost)
   ENDDO
   nl = nl-illost
   nlsnow = nlsnow - illost
   IF (nlsnow < 0) nlsnow = 0
  ENDIF
  corrsnow = 0.
ENDIF

melt_l = obsmelt

END SUBROUTINE SET_HEIGHT

!===============================================================================
FUNCTION averdens(zz)
! calculate density over layer with snow for mass balance calculation
!===============================================================================
USE SNOW_EBM , ONLY : nl , lid , dens , z , dz , rhosn

!input
REAL :: zz
!local
REAL :: cumdens 
REAL :: averdens

il = 1
 cumdens = 0
averdens = 0.
DO WHILE ((z(il)+0.5*dz(il) < zz) .and. (lid(il) > 0))
  cumdens = cumdens + dens(il) * dz(il)
  il = il + 1
ENDDO
IF (lid(il) > 0 .and. zz > 0.) THEN
  cumdens = cumdens + dens(il) * (zz - (z(il)-0.5*dz(il)))
  averdens = cumdens / zz
ELSE IF (il > 1) THEN
  averdens = cumdens / (z(il-1)+0.5*dz(il-1))
ELSE IF (il == 1) THEN
  averdens = rhosn
ELSE 
  averdens = rhosn
ENDIF

!WRITE(*,*) rhosn,averdens,il

END FUNCTION averdens

!===============================================================================
SUBROUTINE MBYEAR(ii,jj,iy)
!===============================================================================
! subroutine that resets certain arrays when mass balance year ends
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift determined from 16
!===============================================================================
USE SNOW_EBM , ONLY : nl , lid , lidmax , icemeltout , icemeltmin , hmass , rhosn , &
&                     annualbal , summerbal , winterbal , &
&                     winterbalmsn, summerbalmsn, annualbalmsn , &
&                     winterbalmod , summerbalmod , annualbalmod ,  &
&                     wintermelt, summermelt, annualmelt, &
&                     melt , startmelt , endmelt , stmeltout , ndmeltout , mbout
USE GLOBALS_EBM , ONLY : sbuf , buf , ilast , icemeltcorrmod , icemeltcorrobs , snowcorrobs , errorflag
USE CONSTANTS_EBM , ONLY : errorval
USE INPUT_EBM , ONLY : ilyear , densice , mbsumdy , mbwindy , luseacc , lerrorgap

IMPLICIT NONE

!input
INTEGER :: ii,jj,iy
!local
INTEGER :: lid_old
INTEGER :: il,iyr
REAL :: averdens,dens

IF (melt > 0.) THEN
  IF ((mbsumdy < mbwindy) .and. (buf(ii,2) < mbwindy) .and. (buf(ii,2) > mbsumdy)) THEN		!NH 150 300
    IF (startmelt > buf(ii,2)) startmelt = buf(ii,2)
    IF (endmelt < buf(ii,2)) endmelt = buf(ii,2)
  ELSE IF ((mbsumdy > mbwindy) .and. ((buf(ii,2) < mbwindy) .or. (buf(ii,2) > mbsumdy))) THEN		!SH 335 30
    IF (buf(ii,2) > mbsumdy) THEN
      IF (startmelt > buf(ii,2)) startmelt = buf(ii,2)  
      IF (endmelt < buf(ii,2)) endmelt = buf(ii,2)
    ELSE IF (buf(ii,2) < mbwindy) THEN
      IF (startmelt > 367) startmelt = buf(ii,2)
      IF (endmelt > mbsumdy .or. endmelt < buf(ii,2)) endmelt = buf(ii,2)
    ENDIF
  ENDIF
ENDIF


! ------
! mb based on obs if available to compare with modelled
IF (luseacc > 0) THEN	 

  IF (buf(ii,2) == mbsumdy .and. jj == 0) THEN		! start summer, end winter

    IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN 		! observed Acc and icemelt
      dens = averdens(sbuf(18)-snowcorrobs)
      winterbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(19)-icemeltcorrobs))
      winterbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(19)-icemeltcorrobs)*densice)*0.001
    ENDIF
    
  ELSE IF (buf(ii,2) == mbwindy .and. jj == 0) THEN		! end summer, start winter, end mb year

    IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN
      dens = averdens(sbuf(18)-snowcorrobs)
      annualbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(19)-icemeltcorrobs))
      annualbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(19)-icemeltcorrobs)*densice)*0.001
    ENDIF
    IF (annualbal > errorval .and. winterbal > errorval) THEN
      summerbalmsn = annualbalmsn - winterbalmsn
      summerbal = annualbal - winterbal
    ENDIF

    mbout(1,1) = winterbal	!observations
    mbout(1,2) = summerbal
    mbout(1,3) = annualbal
    mbout(1,4) = winterbalmsn	!observations in m snow/ice
    mbout(1,5) = summerbalmsn
    mbout(1,6) = annualbalmsn

    icemeltcorrobs = sbuf(19)
    IF (icemeltcorrobs.LE.0) THEN
      snowcorrobs = 0
    ELSE
      snowcorrobs = sbuf(18)
    ENDIF

    winterbal = -999.
    summerbal = -999.
    annualbal = -999.
    winterbalmsn = -999.
    summerbalmsn = -999.
    annualbalmsn = -999.

  ENDIF
  
  IF (iy == ilyear .and. ii == ilast .and. jj == 0) THEN
!    WRITE(*,*) buf(ii,2),sbuf(18),snowcorrobs,sbuf(19),icemeltcorrobs
    IF (mbsumdy < mbwindy) THEN		!NH 150 300
      IF ((buf(ii,2) < mbwindy) .and. (buf(ii,2) > mbsumdy)) THEN		! determine a summer balance and end annual balance
        IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN
           dens = averdens(sbuf(18)-snowcorrobs)
           annualbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(19)-icemeltcorrobs))
           annualbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(19)-icemeltcorrobs)*densice)*0.001
        ENDIF
        IF (annualbal > errorval .and. winterbal > errorval) THEN
          summerbalmsn = annualbalmsn - winterbalmsn
          summerbal = annualbal - winterbal
        ENDIF
      ELSE IF ((buf(ii,2) > mbwindy) .or. (buf(ii,2) < mbsumdy)) THEN		! determine a winter balance no total banlace    
        IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN
           dens = averdens(sbuf(18)-snowcorrobs)
           winterbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(19)-icemeltcorrobs))
           winterbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(19)-icemeltcorrobs)*densice)*0.001
        ENDIF
      ENDIF
    ELSE IF (mbsumdy > mbwindy) THEN		!SH 335 30
      IF ((buf(ii,2) < mbwindy) .or. (buf(ii,2) > mbsumdy)) THEN		! determine a summer balance and end annual balance
        IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN
           dens = averdens(sbuf(18)-snowcorrobs)
           annualbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(19)-icemeltcorrobs))
           annualbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(19)-icemeltcorrobs)*densice)*0.001
        ENDIF
      ELSE IF ((buf(ii,2) > mbwindy) .and. (buf(ii,2) < mbsumdy)) THEN		! determine a winter balance no total banlace 
        IF (sbuf(18) > errorval .and. sbuf(19) > errorval) THEN
           dens = averdens(sbuf(18)-snowcorrobs)
           winterbalmsn = ((sbuf(18)-snowcorrobs) + (sbuf(20)-icemeltcorrobs))
           winterbal = ((sbuf(18)-snowcorrobs)*dens + (sbuf(20)-icemeltcorrobs)*densice)*0.001
        ENDIF
        IF (annualbal > errorval .and. winterbal > errorval) THEN
          summerbalmsn = annualbalmsn - winterbalmsn
          summerbal = annualbal - winterbal
        ENDIF
      ENDIF
    ENDIF
    iyr = 1
    IF (buf(ii,2) > mbwindy) iyr = 3
    mbout(iyr,1) = winterbal		!observations
    mbout(iyr,2) = summerbal
    mbout(iyr,3) = annualbal
    mbout(iyr,4) = winterbalmsn		!observations
    mbout(iyr,5) = summerbalmsn
    mbout(iyr,6) = annualbalmsn
  ENDIF

ELSE
  snowcorrobs = 0
ENDIF	! luseacc > 0

! ------
! mb based on model results 
IF (buf(ii,2) == mbsumdy .and. jj == 0) THEN		! start summer, end winter

    IF (hmass > errorval .and. icemeltout > errorval) &
&      winterbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
  
ELSE IF (buf(ii,2) == mbwindy .and. jj == 0) THEN		! start winter, end summer, end mb year
! increase id level of snowpack
  IF (icemeltcorrmod <= icemeltout) THEN
    IF (lid(1) == 1) lidmax = lidmax + 1
    DO il=1,nl
      IF (lid(il) > 0) lid(il) = lid(il) + 1
    ENDDO
  ENDIF

  IF (hmass > errorval .and. icemeltout > errorval) &
&    annualbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
  IF (annualbalmod > errorval .and. winterbalmod > errorval) summerbalmod = annualbalmod - winterbalmod

  mbout(2,1) = winterbalmod	!model
  mbout(2,2) = summerbalmod
  mbout(2,3) = annualbalmod
  mbout(2,4) = wintermelt*0.001	!model
  mbout(2,5) = summermelt*0.001
  mbout(2,6) = annualmelt*0.001

  icemeltcorrmod = icemeltout
  IF (icemeltout < errorval) icemeltcorrmod = icemeltmin

  stmeltout = startmelt
  ndmeltout = endmelt
  startmelt = 999. 
  endmelt = -999.
  winterbalmod = -999.
  summerbalmod = -999.
  annualbalmod = -999.
  wintermelt = 0.
  summermelt = 0.
  annualmelt = 0.
ENDIF

IF (iy == ilyear .and. ii == ilast .and. jj == 0) THEN
  IF (mbsumdy < mbwindy) THEN		!NH 150 300
    IF ((buf(ii,2) < mbwindy) .and. (buf(ii,2) > mbsumdy)) THEN		! determine a summer balance and end annual balance
      IF (hmass > errorval .and. icemeltout > errorval) annualbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
      IF (annualbalmod > errorval .and. winterbalmod > errorval) summerbalmod = annualbalmod - winterbalmod
    ELSE IF ((buf(ii,2) > mbwindy) .or. (buf(ii,2) < mbsumdy)) THEN		! determine a winter balance no total balance    
      IF (hmass > errorval .and. icemeltout > errorval) winterbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
    ENDIF
  ELSE IF (mbsumdy > mbwindy) THEN		!SH 335 30
    IF ((buf(ii,2) < mbwindy) .or. (buf(ii,2) > mbsumdy)) THEN		! determine a summer balance and end annual balance 
      IF (hmass > errorval .and. icemeltout > errorval) annualbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
      IF (annualbalmod > errorval .and. winterbalmod > errorval) summerbalmod = annualbalmod - winterbalmod    
    ELSE IF ((buf(ii,2) > mbwindy) .and. (buf(ii,2) < mbsumdy)) THEN		! determine a winter balance no total balance 
      IF (hmass > errorval .and. icemeltout > errorval) winterbalmod = (hmass + (icemeltout-icemeltcorrmod)*densice)*0.001
    ENDIF
  ENDIF
  iyr = 2
  IF (buf(ii,2) > mbwindy) iyr = 4
  stmeltout = startmelt
  ndmeltout = endmelt
  mbout(iyr,1) = winterbalmod	!model
  mbout(iyr,2) = summerbalmod
  mbout(iyr,3) = annualbalmod
  mbout(iyr,4) = wintermelt*0.001	!model
  mbout(iyr,5) = summermelt*0.001
  mbout(iyr,6) = annualmelt*0.001
ENDIF

END SUBROUTINE MBYEAR
!===============================================================================
