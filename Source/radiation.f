!===============================================================================
!
! All routines related to the correction and calculation of radiation 
! components of the energy balance
!
!===============================================================================
SUBROUTINE CALCALBEDO
!===============================================================================
! Routine that calculates the surface albedo based on:
! 1= Oerlemans and Knap, 1998, J. Glaciol. 
! 2= Douville et al 1995. 
! 3= combination of Oerlemans and Knap, 1998, J. Glaciol. and Zuo and Oerlemans, 1996, J. Glaciol. taken from Bougamont et al 2005
! including a dependence on snowdepth following Oerlemans and Knap, 1998 
! including solar zenith angle dependency: Segal et al., 1991 J. Atm. Sci., 48(8), 1024-1042. 
!===============================================================================
USE INPUT_EBM , ONLY : tstep , lalbedo , albsnow , albice , albfirn , snowstar , &
&               tstardry0 , tstardry10  , tstarwet , solzenyes 
USE GLOBALS_EBM , ONLY : snowdays , t0 , szenith , alb_old , albedo
USE SNOW_EBM , ONLY : lid , dsnowh , water, nl , precip , dsnowr , hmass , rhosn
USE CONSTANTS_EBM , ONLY : Tkel , secday , pi , errorval

IMPLICIT NONE

REAL , PARAMETER :: efoldcoef = 0.24			! Douville: exponential e-folding coefficient
REAL , PARAMETER :: reducrate=0.008		! Douville: constant reduction ratio
INTEGER :: il , firnrest
REAL :: snowrest, snowwet
REAL :: albmin, albmax
REAL :: snowalb,tstar,constant
REAL :: coszen, factor

!solzenyes  1 = yes, 0 = no correction albedo for low solar zenith angle
IF (solzenyes == 1) THEN	
  coszen = COS(szenith)
  IF (COS(szenith) < COS(80.*pi/180.)) coszen = COS(80.*pi/180.)
  factor = 0.32*0.5*((3./(1.+4.*coszen))-1.)
  IF (factor < 0.) factor = 0.
ELSE
  factor = 0.
ENDIF

il = 1
DO WHILE (lid(il) == 1) 		!snow cover
 il = il + 1
ENDDO
firnrest = 0
IF (lid(il) >= 1) firnrest = 1	!snow or firn

albmax = albsnow
snowrest = hmass		!snowrest in mm w.e.
IF ((snowrest <=0.).and.(dsnowr > 0.)) snowrest = dsnowr*rhosn		!snowrest in mm w.e.
IF (snowrest < 0.) snowrest = 0.
IF (lid(1) >= 2) snowrest = 0.001		! firn / snow more than 1 year old
snowwet = water(1)
 constant = (tstardry10-tstardry0)/10.

IF (precip <= 0.) THEN		! in case of no precipitation snow albedo decreases in time due to aging of snow
 snowdays = snowdays + tstep/secday				! time since last snowfall in days.
 albmin = albfirn

 IF (snowrest > 0.) THEN		! there is still snow on the surface 

   IF (lalbedo <= 1) THEN					! Oerlemans and Knap
     snowalb = albmin + (albmax - albmin) * exp((-snowdays)/tstarwet);

   ELSE IF (lalbedo == 2) THEN		! in Bougamont et al., 2005 using Oerlemans and Knap, 1998, Zuo and Oerlemans, 1996
     IF (snowwet > 0.) THEN
       tstar = tstarwet
     ELSE IF ((t0 <= Tkel).and.(t0 >= Tkel-10.)) THEN
       tstar = ABS(t0-Tkel)*constant+tstardry0
     ELSE
       tstar = tstardry10
     ENDIF
     snowalb = alb_old - (alb_old - albmin)*(tstep/(tstar*secday))

   ELSE IF (lalbedo == 3) THEN		! Douville et al, 1995 modified by VdHurk and Viterbo 2003
     IF (t0 >= Tkel) THEN
       snowalb = albmin + (alb_old - albmin) *exp(-efoldcoef*tstep/secday)
     ELSE
       snowalb = alb_old - reducrate*(tstep/(secday*(1+0.1*(t0-Tkel)**4.)))		! Modified by vdHurk and Viterbo 2003
     ENDIF
   ENDIF
 
! Correction albedo for low solar zenith angles, Segal et al, 1991
   IF (solzenyes == 1) THEN
     snowalb = snowalb + factor
     IF (snowalb > albmax) snowalb = albmax
   ENDIF
 
   albmin = albice
   IF (firnrest == 1) albmin=albfirn
   albedo = snowalb + (albmin - snowalb) * exp(-snowrest/snowstar)
 
 ELSE		! in case of no precipitation and ice at the surface
   albedo = albice		
 ENDIF

ELSE 		! in case of fresh snowfall
  albedo = albsnow
  snowdays = 0.
ENDIF

alb_old = albedo
IF ((solzenyes == 1).and.(precip == 0).and.(snowrest /= 0.)) alb_old = alb_old - factor

END SUBROUTINE CALCALBEDO

!===============================================================================
FUNCTION solarspectrum (cc,pres,icount)
!===============================================================================
USE RADPEN_EBM , ONLY : SolarPlateauClear, SolarPlateauCloudy, SolarSeaClear
IMPLICIT NONE
!Input
INTEGER, INTENT(IN)	:: icount
REAL, INTENT(IN)	:: cc, pres

!Output
REAL	:: solarspectrum

! local
REAL :: presplat, pressea
REAL :: cloudf,dsdp,SolarawsClear


presplat = 60900.0		! pressure plateau for standard solar spectrum
pressea = 98000.0		! presure sea level for standard solar spectrum

IF (SolarPlateauClear(icount).gt.0.) THEN
  cloudf = 1.0 - cc * (1.0 - SolarPlateauCloudy(icount)/SolarPlateauClear(icount))
ELSE
  cloudf = 0.
ENDIF

dsdp = (SolarSeaClear(icount) - SolarPlateauClear(icount))/( pressea - presplat)
SolarawsClear = SolarPlateauClear(icount) + dsdp * (pres - presplat)
solarspectrum = SolarawsClear*cloudf

END FUNCTION solarspectrum

!===============================================================================
FUNCTION cloudcover(temp,lwin)
!===============================================================================
USE INPUT_EBM, ONLY		: chstation, lwmax, lwmin
USE CONSTANTS_EBM, ONLY	: Tkel

IMPLICIT NONE

!Input
REAL,INTENT(IN) :: temp, lwin
REAL :: cloudcover
!local
REAL :: max, min, reltemp

reltemp = temp-Tkel

max = lwmax(1) + lwmax(2)*reltemp + lwmax(3)*reltemp**2.
min = lwmin(1) + lwmin(2)*reltemp + lwmin(3)*reltemp**2.

 cloudcover = (lwin - min)/(max - min)
IF (cloudcover.gt.1.) cloudcover = 1.
IF (cloudcover.lt.0.) cloudcover = 0.

END FUNCTION cloudcover

!===============================================================================
SUBROUTINE RadiationPenetration
!===============================================================================
! This routine calculates the fluxes of shortwave radiation that penetrate into
! the snow layer following Brand and Warren J. Glac. 1993
! calculations are done in terms of the radiation flux divergence dqdz
!===============================================================================
USE CONSTANTS_EBM, ONLY: nlmax
USE GLOBALS_EBM, ONLY: cloud, sbuf, Snet
USE INPUT_EBM, ONLY: densice, dzrad, lcomment, lwcloud, radiusice, radiussn, zradmax, lalbedo
USE RADPEN_EBM, ONLY:	asymice, asymsn, bandmax, cosinglescatice, cosinglescatsn, dlambda,&
&						dsdz, qextice, qextsn, sumdivs,&
&						asymAll, cosinglescatAll, qextAll, dlambdaAll
USE SNOW_EBM, ONLY: dens, dz, lid, nlsnow, z, grainsize
USE FILES, ONLY: uo1

IMPLICIT NONE

!Function
REAL :: solarspectrum

!Local
INTEGER :: i, il, k
INTEGER :: kmax, kmin, nlradnd, nlradst
REAL :: aext, radius, rcoef, rhorad, scaling, sumdo, sumup, sz0, sigext
REAL, DIMENSION(bandmax) :: asym, cosinglescat, klam, mlam, qext, solarscaled
REAL, DIMENSION(nlmax)  :: sz

REAL :: aext1, aext2, sigext1, sigext2, rcoef1, rcoef2, sz01, sz02, sumup1, sumup2,&
& sumdo1, sumdo2, sz1, sz2, scaling1, scaling2
REAL, DIMENSION(bandmax) :: asym1, asym2, cosinglescat1, cosinglescat2, dlambda1, dlambda2,&
& klam1, klam2, mlam1, mlam2, qext1, qext2
REAL :: fraction

INTEGER :: igrainbnds
REAL , DIMENSION(7) :: grainbnds

DATA grainbnds/0.00005,0.0001,0.0002,0.00035,0.0005,0.001,0.0025/

sz = 0.
dsdz = 0.
sumdivs = 0.

IF(lalbedo .eq. 4 .or. lalbedo .eq. 5) THEN	! Kuipers Munneke albedo scheme with snow grain size calculation
 DO il=1,nlmax
  IF((z(il)+0.5*dz(il))>zradmax) THEN
   nlradnd = il
   GOTO 100
  ENDIF 
 ENDDO 
 100 CONTINUE
 
 DO il = 1,nlradnd
  igrainbnds = 8
  DO i=7,1,-1
    IF (grainsize(il) <= grainbnds(i)) igrainbnds = i
  ENDDO
  IF (grainsize(il) == grainbnds(7)) igrainbnds = 8

  IF (igrainbnds == 8 .or. igrainbnds == 1) THEN
   IF (igrainbnds == 8) igrainbnds = 7
   dlambda = dlambdaAll(igrainbnds,:)
   asym = asymAll(igrainbnds,:)
   cosinglescat = cosinglescatAll(igrainbnds,:)
   qext = qextAll(igrainbnds,:)

   DO i = 1,bandmax
    sigext = (qext(i)*3.*dens(il))/(4.*densice*grainsize(il))
    aext = sigext * cosinglescat(i)
    rcoef = 0.5*sigext*(1.0 - asym(i))*(1.0 - cosinglescat(i))
    klam(i) = SQRT(aext*aext + 2.0*aext*rcoef)
    mlam(i) = (aext + rcoef - klam(i))/rcoef		! note density dependence falls out of mlam
   ENDDO
   
   sz0 = 0.
   DO  i = 1, bandmax
    IF (lwcloud.ne.1) THEN
     solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(0.0,sbuf(5),i)		!413 = ±sum of spectrum
    ELSE IF (lwcloud == 1) THEN
     solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(cloud,sbuf(5),i)		!413 = ±sum of spectrum
    ENDIF
    sz0 = sz0 + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0 - mlam(i))
   ENDDO
 
   scaling = (Snet-sumdivs)/sz0
   
   IF(il.eq.1) THEN
    sumup = 0.0
    DO i=1,bandmax
     sumup = sumup + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0-mlam(i)) * exp(-klam(i) * dzrad)
    ENDDO
   ENDIF
   sumdo = 0.0
   DO i=1,bandmax
    sumdo = sumdo + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0-mlam(i)) * exp(-klam(i) * (z(il)+0.5*dz(il)))
   ENDDO
   sz(il) = -ABS(scaling*(sumup-sumdo))
   sumup = sumdo
   dsdz(il) = sz(il)/dz(il)
   sumdivs = sumdivs - sz(il)
   IF ( sumdo*scaling < 0.01 ) goto 200
  ELSE

   dlambda1 = dlambdaAll(igrainbnds,:)
   asym1 = asymAll(igrainbnds,:)
   cosinglescat1 = cosinglescatAll(igrainbnds,:)
   qext1 = qextAll(igrainbnds,:)
   
   dlambda2 = dlambdaAll(igrainbnds-1,:)
   asym2 = asymAll(igrainbnds-1,:)
   cosinglescat2 = cosinglescatAll(igrainbnds-1,:)
   qext2 = qextAll(6,:)
   
   fraction = (grainsize(il)-grainbnds(igrainbnds-1))/(grainbnds(igrainbnds)-grainbnds(igrainbnds-1))
   
   DO i = 1,bandmax
    sigext1 = (qext1(i)*3.*dens(il))/(4.*densice*grainsize(il))
    aext1 = sigext1 * cosinglescat1(i)
    rcoef1 = 0.5*sigext1*(1.0 - asym1(i))*(1.0 - cosinglescat1(i))
    klam1(i) = SQRT(aext1*aext1 + 2.0*aext1*rcoef1)
    mlam1(i) = (aext1 + rcoef1 - klam1(i))/rcoef1		! note density dependence falls out of mlam
    
    sigext2 = (qext2(i)*3.*dens(il))/(4.*densice*grainsize(il))
    aext2 = sigext2 * cosinglescat2(i)
    rcoef2 = 0.5*sigext2*(1.0 - asym2(i))*(1.0 - cosinglescat2(i))
    klam2(i) = SQRT(aext2*aext2 + 2.0*aext2*rcoef2)
    mlam2(i) = (aext2 + rcoef2 - klam2(i))/rcoef2		! note density dependence falls out of mlam
   ENDDO
    
   sz01 = 0.
   sz02 = 0.
   DO  i = 1, bandmax
    IF (lwcloud.ne.1) THEN
     solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(0.0,sbuf(5),i)		!413 = ±sum of spectrum
    ELSE IF (lwcloud == 1) THEN
     solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(cloud,sbuf(5),i)		!413 = ±sum of spectrum
    ENDIF
    sz01 = sz01 + 1.0E-3 * dlambda1(i) * solarscaled(i) * (1.0 - mlam1(i))
    sz02 = sz02 + 1.0E-3 * dlambda2(i) * solarscaled(i) * (1.0 - mlam2(i))
   ENDDO
 
   scaling1 = (Snet-sumdivs)/sz01
   scaling2 = (Snet-sumdivs)/sz02
   
   IF(il.eq.1) THEN
    sumup1 = 0.0
    sumup2 = 0.0
    DO i=1,bandmax
     sumup1 = sumup1 + 1.0E-3 * dlambda1(i) * solarscaled(i) * (1.0-mlam1(i)) * exp(-klam1(i) * dzrad)
     sumup2 = sumup2 + 1.0E-3 * dlambda2(i) * solarscaled(i) * (1.0-mlam2(i)) * exp(-klam2(i) * dzrad)
    ENDDO
   ENDIF
   sumdo1 = 0.0
   sumdo2 = 0.0
   DO i=1,bandmax
    sumdo1 = sumdo1 + 1.0E-3 * dlambda1(i) * solarscaled(i) * (1.0-mlam1(i)) * exp(-klam1(i) * (z(il)+0.5*dz(il)))
    sumdo2 = sumdo2 + 1.0E-3 * dlambda2(i) * solarscaled(i) * (1.0-mlam2(i)) * exp(-klam2(i) * (z(il)+0.5*dz(il)))
   ENDDO
   sz1 = ABS(scaling1*(sumup1-sumdo1))
   sz2 = ABS(scaling2*(sumup2-sumdo2))
   sz(il) = -ABS(MIN(sz1,sz2) + ABS(sz1-sz2)*fraction)
 
   sumup1 = sumdo1
   sumup2 = sumdo2
   dsdz(il) = sz(il)/dz(il)
   sumdivs = sumdivs - sz(il)
   IF ( sumdo1*scaling1 < 0.01 .or. sumdo2*scaling2 < 0.01 ) goto 200
  ENDIF
 ENDDO
 200 CONTINUE
ELSE
 DO il=1,nlmax
  IF((z(il)+0.5*dz(il))>zradmax) THEN
   nlradnd = il
   GOTO 300
  ENDIF 
 ENDDO 
 300 CONTINUE
 
 DO il=1,nlradnd
  !Here read snow/ice values into the appropriate arrays
  IF(lid(il).eq.0) THEN
   radius = radiusice
   asym = asymice
   qext = qextice
   cosinglescat = cosinglescatice
  ELSE
   radius = radiussn
   asym = asymsn
   qext = qextsn
   cosinglescat = cosinglescatsn
  ENDIF
 
  DO i = 1,bandmax
   sigext = (qext(i)*3.*dens(il))/(4.*densice*radius)
   aext = sigext * cosinglescat(i)
   rcoef = 0.5*sigext*(1.0 - asym(i))*(1.0 - cosinglescat(i))
   klam(i) = SQRT(aext*aext + 2.0*aext*rcoef)
   mlam(i) = (aext + rcoef - klam(i))/rcoef		! note density dependence falls out of mlam
  ENDDO
    
  sz0 = 0.
  DO  i = 1, bandmax
   IF (lwcloud.ne.1) THEN
    solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(0.0,sbuf(5),i)		!413 = ±sum of spectrum
   ELSE IF (lwcloud == 1) THEN
    solarscaled(i) = (sbuf(7)/413.8)*solarspectrum(cloud,sbuf(5),i)		!413 = ±sum of spectrum
   ENDIF
   sz0 = sz0 + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0 - mlam(i))
  ENDDO
  
  scaling = (Snet-sumdivs)/sz0
  IF(il.eq.1) THEN
   sumup = 0.0
   DO i=1,bandmax
    sumup = sumup + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0-mlam(i)) * exp(-klam(i) * dzrad)
   ENDDO
  ENDIF
  sumdo = 0.0
  DO i=1,bandmax
   sumdo = sumdo + 1.0E-3 * dlambda(i) * solarscaled(i) * (1.0-mlam(i)) * exp(-klam(i) * (z(il)+0.5*dz(il)))
  ENDDO
  sz(il) = -ABS(scaling*(sumup-sumdo))
  sumup = sumdo
  dsdz(il) = sz(il)/dz(il)
  sumdivs = sumdivs - sz(il)
  IF ( sumdo*scaling < 0.01 ) goto 400
 ENDDO ! k=kmin,kmax
 400 CONTINUE
ENDIF

IF((il-1).eq.nlradnd .and. (k-1).eq.kmax) THEN
 WRITE(uo1,*) "Warning: shortwave radiation penetration has reached zradmax, remaining energy: ", sumdo*scaling
 IF(lcomment.eq.1) WRITE(*,*) "Warning: shortwave radiation penetration has reached zradmax, remaining energy: ", sumdo*scaling
ENDIF

END SUBROUTINE RadiationPenetration
!===============================================================================