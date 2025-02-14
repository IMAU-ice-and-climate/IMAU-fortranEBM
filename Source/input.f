!===============================================================================
!
! File with all routines related to the input of data and information
!
!===============================================================================
SUBROUTINE INFO
!===============================================================================
! Reads location and time period specific information
!===============================================================================
USE INPUT_EBM, ONLY:	ibyear, ilyear, tstep, tcalc, extrapolation, penetration, &
&						dz0, dzdeep, zdeep, densice, densfirn, densclosure, rhosninit, rhosnprec, trhoprec, ltempprec, &
&						tpcond, lwcloud, lwmax, lwmin, depthin, odepthin, tpdens, accyear, &
&						mbsumdy, mbwindy, radiussn, radiusice, lmc, SSAfresh, radrefr, &
&						T10m, dsnow, dfirn, z0msn, z0mice, lcomment, chstation, lrefr, soot, &
&						albmin, albmax, lerrorgap, luseacc, lz0m, lsnet, zll, zul, Hmax, Hmax_month, lz0h, lhourlysnowout, &
&						lclimtemp, lclimprec, lclimrad, lclimws, climtemp, climprec, climrad, climws, &
&						tpirre, cirre, lslush, surfangle, tausteep, tauhor, tau1, slfact, solzenyes, &
&						lalbedo, albsnow, albice, albfirn, tstarwet, tstardry0, tstardry10, snowstar, dzrad, zradmax
USE CONSTANTS_EBM, ONLY: Tkel, emis
USE GLOBALS_EBM , ONLY : Hice
USE FILES, ONLY:		ui, uo1
IMPLICIT NONE
!Local
INTEGER :: i,istep,ihour
REAL :: rhour
REAL :: rndnr(2)

OPEN(unit=uo1,file='runinfo.txt',status='unknown')

OPEN(unit=ui,file='awsname.txt',status='old')
READ(ui,*) chstation
WRITE(uo1,'(/,2A)') 'region_awsnr: ', chstation
CLOSE(ui)

OPEN(unit=ui,file='infofile.txt',status='old')

READ(ui,*) lcomment		!integer 1=yes or 0=no comments to screen

IF (lcomment == 1) WRITE(*,'(/,A)') 'Read some useful coefficients '
WRITE(uo1,'(/,A)') 'Read some useful coefficients '

READ(ui,*) lerrorgap	!jump over data gaps = 0 or interpolate = 1 (errorflag)
WRITE(uo1,*) lerrorgap,'0 = jump over data gaps, 1 = interpolate, 2 = interpolate and use all data for averages (errorflag), '
READ(ui,*) lmc			!disturb measurements with random errors
WRITE(uo1,*) lmc, 'disturb measurements with random errors'
READ(ui,*) lhourlysnowout	!Write snow characteristics every [0] day or [1] hour, default is 0 because it slows down enormously, use only when hourly values are needed for evaluation
WRITE(uo1,*) lhourlysnowout, 'Write snow characteristics every [0] day or [1] hour, default is 0 because it slows'//&
&							' down enormously, use only when hourly values are needed for evaluation'
READ(ui,*) ibyear		!Year at beginning of calculations
READ(ui,*) ilyear		!Year at end of calculations
WRITE(uo1,*) ibyear,ilyear, 'Start and end year of the calculations'
READ(ui,*) tstep		!Time step in seconds, mainly to keep subsurface calculations stable
WRITE(uo1,*) tstep,' Time step in seconds'
READ(ui,*) mbsumdy,mbwindy		! day of year on which mb year ends
WRITE(uo1,*) mbsumdy,mbwindy,' Day of year on which mb summer and winter start, mbwindy equals end mb year,'//&
&                           ' then snow (id1) is set to firn (id2), and firn increases +1'
READ(ui,*) dz0			!Size of uppermost grid cell in m
READ(ui,*) dzdeep		!Size of lowermost grid cell in m
READ(ui,*) zdeep		!Depth of grid in m
WRITE(uo1,*) dz0,dzdeep,zdeep,' Size of uppermost and lowemost grid cell and depth of grid in m'
READ(ui,*) densice,densfirn,densclosure		!ice density
WRITE(uo1,*) densice,densfirn,densclosure,'Ice and firn density'
READ(ui,*) rhosninit,rhosnprec,trhoprec	!Density of initial snowpack and fresh snowfall and type of determination of snow fall density 0 = constant, 1 is f(ws,t)
WRITE(uo1,*) rhosninit,rhosnprec,trhoprec,' Density of initial snow pack and fresh snowfall(if no profile data is available)'//&
&                           ' and type of determination of snow fall density 0 = constant, 1 is f(ws,t)'//&
&                           ' (in case 1, rhosnprec is used as lower limit)'
READ(ui,*) ltempprec
WRITE(uo1,*) ltempprec,' ltempprec'
READ(ui,*) tpcond		!Type of effective conductivity equation, 1= Von Dussen, 2=Sturm
WRITE(uo1,*) tpcond,' Type of effective conductivity: 1=Von Dussen, 2=Sturm'
READ(ui,*) T10m			!Measured 10 m snow temperature
T10m = T10m + Tkel
WRITE(uo1,*) T10m,' Observed 10 m snow temperature'
READ(ui,*) dsnow,dfirn		! Thickness of snow/firn layer on top of glacier ice in m
WRITE(uo1,*) dsnow,dfirn,' Thickness of snow/firn layer on top of glacier ice in m'
READ(ui,*) luseacc		! use sonic data for: 0= not at all, 1=only for evaluation of ice melt and accumulation, 2= 1 + restrict accum based on precip with measured sonic altim.
WRITE(uo1,*) luseacc, 'use sonic data for: 0 = not at all, 1 =only for evaluation of ice melt and accumulation, '//&
&                     '2 = 1 + restrict accum based on precip with measured sonic altim.'
READ(ui,*) tpdens, lrefr	!Type of densification routine, lrefr:1/0 yes/no refreezing, (tpdens: 0 = no densification, 2 = Herron and Langway 1980, 3 = Li and Zwally 2004, 4 = Li and Zwally plus vapor transport, 5 = Helsen 2008, 6 = Arthern 2010, 7 = Ligtenberg 2011, -1 = no dry densification, density profile reset after each time step, snow height folows observations)
WRITE(uo1,*) tpdens, lrefr, ' Type of densification routine, lrefr:1/0 yes/no refreezing, (tpdens: 0 = no densification, '//&
&					        '2 = Herron and Langway 1980, 3 = Li and Zwally 2004, 4 = Li and Zwally plus vapor transport, '//&
&					        '5 = Helsen 2008, 6 = Arthern 2010, 7 = Ligtenberg 2011, -1 = no dry densification, density '//&
&					        'profile reset after each time step, snow height folows observations)'
READ(ui,*) tpirre,cirre	!Type of irreducible water content routine 0 = no water percolation, 1 = constant value cirre, 2 = Coleou and Lesaffre 1998, 3 = Schneider and Jansson 2004
WRITE(uo1,*) tpirre,cirre,'Type of irreducible water content routine 0 = no water in de snow, 1 = constant value cirre, '//&
&                  '2 = Coleou and Lesaffre 1998, 3 = Schneider and Jansson 2004'
READ(ui,*) lslush		! yes (1) or no (0) slush formation possible based on Zuo and Oerlemans 1996.
READ(ui,*) surfangle, tausteep, tauhor,tau1,slfact		! parameters in slush formation routine resp: surface angle, runoff time scale of steep slopes > 5, runoff time scale od horizontal slopes, runoff time scales on 1deg slopes, factor difference between surface runoff and within snowpack, 
WRITE(uo1,*) lslush, 'yes (1) or no (0) slush formation possible based on Zuo and Oerlemans 1996.'
WRITE(uo1,*) surfangle, tausteep, tauhor,tau1,slfact,' parameters in slush formation routine resp: surface angle, '//&
&                             ' runoff time scale of steep slopes > 5, runoff time scale od horizontal slopes, '// &
&                             ' runoff time scales on 1deg slopes, factor difference between surface runoff and within snowpack'

IF (tpirre == 0) THEN
  WRITE(uo1,*) 'Water percolation and refreezing is turned off, all water immediately runs of'  
  IF (lcomment == 1) WRITE(*,*) 'Water percolation and refreezing is turned off, all water immediately runs of'  
  cirre = 0.1
  tpirre = 1
  IF (lcomment == 1) WRITE(uo1,*) tpirre,cirre,'Type of irreducible water content routine 0 = no water in de snow, '//&
&                  '1 = constant value cirre, 2 = Coleou and Lesaffre 1998, 3 = Schneider and Jansson 2004'
  IF (lcomment == 1) WRITE(*,*) tpirre,cirre,'Type of irreducible water content routine 0 = no water in de snow, '//&
&                  '1 = constant value cirre, 2 = Coleou and Lesaffre 1998, 3 = Schneider and Jansson 2004' 
  lslush = 0
  WRITE(uo1,*) lslush, 'yes (1) or no (0) slush formation possible based on Zuo and Oerlemans 1996.'
  IF (lcomment == 1) WRITE(*,*) lslush, 'yes (1) or no (0) slush formation possible based on Zuo and Oerlemans 1996.'  
ENDIF

READ(ui,*) accyear		! Annual average accumulation for densification calculation
WRITE(uo1,*) accyear,' Annual average accumulation for densification calculation'
READ(ui,*) lz0m, zll, zul, Hmax, Hmax_month			!Switch roughness length for momentum from file(0), set below (1) or in case of snow a random value between zll and zul (2) or ice (3),or parameterized using Hmax based on Van Tiggelen et al (2021) (4)
WRITE(uo1,*) lz0m, zll, zul, Hmax, Hmax_month, 'Switch roughness length for momentum from file (0), set below (1)'//&
&							' or in case of snow a random value between zll and zul (2) or ice (3)'//&
&             ', or parameterized using Hmax based on Van Tiggelen et al (2021) (4)'
READ(ui,*) z0msn,z0mice		!Roughness length for momentum for snow and ice when lz0m = 1
WRITE(uo1,*) z0msn,z0mice,'Values roughness length for momentum for snow and ice resp. in case lz0m = 1'
READ(ui,*) lz0h   !'lz0h Switch roughness length for heat and moisture, 0.1*z0m (0), using Andreas 1987 parameterization (1), using Smeets and van den Broeke 2008 parameterization (2) or using Van Tiggelen et al (2021) parameterization (3). For smooth surfaces (z0m < 1mm) and lz0m > 0 Andreas 1987 is used.'
WRITE(uo1,*) lz0h, 'lz0h Switch roughness length for heat and moisture, 0.1xz0m (0),'//&
&             ' using Andreas 1987 parameterization (1),'//&
&             ' using Smeets and van den Broeke 2008 parameterization (2),'//&
&             ' or using Van Tiggelen et al (2021) parameterization (3).'//&
&             ' For smooth surfaces (z0m lt 1mm) and lz0m gt 0 Andreas 1987 is used.'
READ(ui,*) tcalc		!formulation calculation surface temperature 1 = from Lout observations, 2 = extrapolated from upper most layers, 3 is skin layer formulation
WRITE(uo1,*) tcalc,' formulation calculation surface temperature 1 = from Lout observations,'//&
&                  ' 2 = equal to temp uppermost layer, 3 = extrapolated from upper most layers,'//&
&                  ' 4 = skin layer formulation'
READ(ui,*) extrapolation		!extrapolation to surface temp 1 = based on only upper layer, 2 = based on upper 2 layers
WRITE(uo1,*) extrapolation,'extrapolation to surface temp 1 = based on only upper layer, 2 = based on upper 2 layers'
READ(ui,*) lsnet
WRITE(uo1,*) lsnet,'switch calculate Snet from Sin and Sout (0), '//&
&            'or calculate Snet and Sin based on Sout and running mean albedo (1)'
READ(ui,*) albmin,albmax
WRITE(uo1,*) albmin,albmax,'minimum, maximum albedo, used to set error values to and prevent unrealistic values'

READ(ui,*) emis
WRITE(uo1,*) emis,'Emissivity of the snow/ice surface'
READ(ui,*) lalbedo,solzenyes, SSAfresh, radrefr	! albedo from 0 = observations,  or parameterised: 1 = Oerlemans and Knap (1998), 2 = Bougamont et al. (2005), 3 = Douville et al. (1995), 4 = Kuipers Munneke et al. (2011), yes(1) or no(0) correction on albedo for zenith angle, fresh snow specific surface area and refrozen snow grain radius (for lalbedo option 4)
WRITE(uo1,*) lalbedo,solzenyes, SSAfresh, radrefr, ' albedo from 0 = observations,  or parameterised:'//&
&			' 1 = Oerlemans and Knap (1998), 2 = Bougamont et al. (2005), 3 = Douville et al. (1995),'//&
&			' 4 = Kuipers Munneke et al. (2011), yes(1) or no(0) correction on albedo for zenith angle,'//&
&			' fresh snow specific surface area and refrozen snow grain radius (for lalbedo option 4)'//&
&           ' 5 = use Kuipers Munneke et al. (2011) for grain size but observations for albedo'
IF(SSAfresh.ne.60.and.SSAfresh.ne.80.and.SSAfresh.ne.100) THEN
 WRITE(*,*) "Invalid value for refrozen grain specific surface area: ", SSAfresh
 WRITE(*,*) "Possible values: 60, 80, 100"
 WRITE(*,*) "Defaulting to SSA = 60"
 SSAfresh = 60
ENDIF
READ(ui,*) albsnow,albice,albfirn,soot		! albedo snow, albedo ice, albedo firn/old snow (used by lalbedo options 1, 2, 3), soot concentration in ppmw (used by lalbedo option 4)
WRITE(uo1,*) albsnow,albice,albfirn,soot, ' albedo snow, albedo ice, albedo firn/old snow (used by lalbedo options 1, 2, 3),'//&
&			' soot concentration in ppmw (used by lalbedo option 4)'
READ(ui,*) snowstar,tstarwet,tstardry0,tstardry10		! characteristic depth scale snow, time scales of decay for wet snow, dry surface at 0C, dry surface at -10C, respectively
WRITE(uo1,*) snowstar,tstarwet,tstardry0,tstardry10,' characteristic depth scale snow (used by lalbedo options 1, 2, 3),'//&
&                 ' time scales of decay for wet snow (used by lalbedo options 1, 2)'//&
&                 ', dry surface at 0C (used by lalbedo option 2), dry surface at -10C (used by lalbedo option 2), respectively'
READ(ui,*) penetration	! radiation penetration 0 = off, 1 = on
WRITE(uo1,*) penetration,'radiation penetration 0 = off, 1 = on'
READ(ui,*) dzrad, zradmax	! snow layer thickness for upper layer in shortwave penetration
WRITE(uo1,*) dzrad,zradmax,' snow layer thickness for upper layer in shortwave penetration'
IF (penetration == 1 .and. zradmax >= zdeep) THEN
 WRITE(uo1,*) 'Chosen max depth for radiation penetration (zradmax) is deeper than chosen grid depth (zdeep).'//&
&                   'zdeep set to zradmax + 1 ',zradmax , zdeep
 IF (lcomment == 1) WRITE(*,*) 'Chosen max depth for radiation penetration (zradmax) is deeper than chosen grid depth (zdeep).'//&
&                   'zdeep set to zradmax + 1 ',zradmax , zdeep
 zdeep = zradmax + 1.
ENDIF
IF(dzrad .gt. 0.5*dz0) THEN
 WRITE(uo1,*) 'Skin layer thickness (dzrad) changed to half of upper snow layer thickness'
 IF(lcomment == 1) WRITE(*,*) 'Skin layer thickness (dzrad) changed to half of upper snow layer thickness'
 dzrad = 0.5*dz0
ENDIF
READ(ui,*) radiussn,radiusice		! radius of snow/ice particles for radiation penetration routine
WRITE(uo1,*) radiussn,radiusice,'radius of snow/ice particles for radiation penetration routine, '//&
&             'in case lalbedo is 4 or 5 radius is calculated with Kuipers Munneke routine'
READ(ui,*) lwcloud		! cloud cover from lwin (1) or not (0), if not then set to 0.
WRITE(uo1,*) lwcloud,'cloud cover from lwin (1) or not (0), if not then set to 0.'
READ(ui,*) (lwmax(i),i=1,3)	! polynomial to describe upper limit of lwin as function of temperature, corresponding to max cloud cover
WRITE(uo1,*) (lwmax(i),i=1,3),'polynomial to describe upper limit of lwin as function'//&
&                                       ' of temperature, corresponding to max cloud cover'
READ(ui,*) (lwmin(i),i=1,3)	! polynomial to describe lower limit of lwin as function of temperature, corresponding to min cloud cover
WRITE(uo1,*) (lwmin(i),i=1,3),'polynomial to describe lower limit of lwin as function'//&
&                                       ' of temperature, corresponding to min cloud cover'
READ(ui,*) (depthin(i),i=1,5)	! depth of first 5 snow temperature sensors at start simulation for validation purposes
WRITE(uo1,*) (depthin(i),i=1,5),' Depth of first 5 snow temperature sensors at start simulation for validation purposes'
odepthin = depthin
READ(ui,*) lclimtemp,climtemp	!logical yes (1) or no (2) climate sensitivity test temperature change in K
WRITE(uo1,*) lclimtemp,climtemp, 'logical yes (1) or no (2) climate sensitivity test temperature change in K'
READ(ui,*) lclimprec,climprec		!logical yes (1) or no (2) climate sensitivity test precipitation change in %
WRITE(uo1,*) lclimprec,climprec, 'logical yes (1) or no (2) climate sensitivity test precipitation change in %'
READ(ui,*) lclimrad,climrad		!logical yes (1) or no (2) climate sensitivity test shortwave radiation change in %
WRITE(uo1,*) lclimrad,climrad, 'logical yes (1) or no (2) climate sensitivity test shortwave radiation change in %'
READ(ui,*) lclimws,climws		!logical yes (1) or no (2) climate sensitivity test wind speed change in %
WRITE(uo1,*) lclimws,climws, 'logical yes (1) or no (2) climate sensitivity test wind speed change in %'

! Information general: model by Peter/Michiel use the following settings:
! tpdens = 0		! fixed density profile, no densification or effect of refreezing on density
! dzdeep = dz0		! constant layer thickness snow/ice
! cirre = 0.1		! constant value of irreducible water content
! tpirre = 1		! type of routine for irreducible water content = constant value
! lslush = 0		! no slush formation included

IF (lz0m == 2) THEN		!test with z0m snow
 CALL RANDOM_NUMBER(rndnr)
 z0msn = (rndnr(1)*(LOG10(zul)-LOG10(zll)))+LOG10(zll) !Done logarithmically to get better coverage of the interval
 z0msn = 10**z0msn
 IF (lcomment == 1) WRITE(*,*) "Picking a random roughness length between ", zll, " and , ", zul, ": ", z0msn
 WRITE(uo1,*) "Picking a random roughness length between ", zll, " and , ", zul, ": ", z0msn 

 rhosnprec = (rndnr(2)*(500.-150.))+150.
! trhoprec = 0
 IF (lcomment == 1) WRITE(*,*) "Picking a random fresh snow density between 150 and 500: ", rhosnprec
 WRITE(uo1,*) "Picking a random fresh snow density between 150 and 500: ", rhosnprec  
ELSEIF (lz0m == 3) THEN
 CALL RANDOM_NUMBER(rndnr)
 z0mice = (rndnr(1)*(LOG10(zul)-LOG10(zll)))+LOG10(zll) !Done logarithmically to get better coverage of the interval
 z0mice = 10**z0mice
 IF (lcomment == 1) WRITE(*,*) "Picking a random roughness length between ", zll, " and , ", zul, ": ", z0mice
 WRITE(uo1,*) "Picking a random roughness length between ", zll, " and , ", zul, ": ", z0mice 

 albmin = (rndnr(2)*(0.5-0.1))+0.1
 IF (lcomment == 1) WRITE(*,*) "Picking a random lower limit ice albedo between 0.1 and 0.5: ", albmin
 WRITE(uo1,*) "Picking a random lower limit ice albedo between 0.1 and 0.5: ", albmin  

ENDIF

IF (((lmc > 0).or.(lz0m.eq.2).or.(lz0m.eq.3)) .and. (lcomment == 1)) THEN
  WRITE(uo1,*) "Turn off comments to screen in case of sensitivity testing"
  WRITE(*,*) "Turn off comments to screen in case of sensitivity testing"
  lcomment = 0
ENDIF

IF ((lclimtemp == 1).or.(lclimprec == 1).or.(lclimrad == 1).or.(lclimws == 1)) THEN
  IF (lalbedo == 0) lalbedo = 1
  WRITE(uo1,*) 'albedo parameterisation turned on to account for changing surface conditions',lalbedo
  lz0m = 10
  WRITE(uo1,*) 'roughness length momentum depends on modeled given surface type'
ENDIF

!check if tstep multiplys to 1 hour, else change it
istep = tstep
DO i=1,tstep-1
 ihour = INT(3600./istep)
 rhour = 3600./istep
 IF (ihour.eq.rhour) GOTO 1
 istep=istep-1
ENDDO
1 CONTINUE
IF (istep.ne.tstep) THEN
 WRITE(uo1,'(/,A)') 'Changed time step into: ',istep
 IF (lcomment == 1) WRITE(*,'(/,A)') 'Changed time step into: ',istep
ENDIF
tstep = istep

 CLOSE(ui)

END SUBROUTINE INFO

!===============================================================================
SUBROUTINE INPDATA(&
!Input
& iyr )
!===============================================================================
! Reads the input data 
!===============================================================================
USE INPUT_EBM , ONLY : ibyear, ilyear, ibday, ilday, lcomment, chstation, lmc, albmin, albmax, tcalc
USE GLOBALS_EBM , ONLY : ilast, ilasttot , ibuf , dhour , chdtg , awsid , &
&                        errorflag , zenith , paramerror
USE CONSTANTS_EBM , ONLY : rowmax , Tkel, loutmax , maxerror
USE FILES , ONLY : ui, uo1
IMPLICIT NONE
!Input
INTEGER,INTENT(IN) :: iyr
!Local
INTEGER :: parmax
INTEGER :: ichs,iche
INTEGER :: ip,ii
INTEGER :: iyear,imin,ihour,imth,idy,julday,isec
REAL , ALLOCATABLE :: tbuf(:)
REAL :: dum, dum2, dum3, Told, Pold
REAL :: jday1,jday2,ih,rands(9)
 CHARACTER :: chdate*10,chtime*8
 CHARACTER :: chyear*4,chmth*2,chdy*2,chhr*2,chmin*2,chsec*2

IF (lcomment == 1) WRITE(*,'(/,A)') 'Read input data '
WRITE(uo1,'(/,A)') 'Read input data '

IF (iyr.eq.ibyear) OPEN(unit=ui,file='inputdata.txt',status='old')

ih = 23.			! last hour of day
parmax = 15			! maximum number to be read - 1, starting at fourth column (first column with data, not time)

IF (chstation.eq."LF_0710") THEN
  ih = 23.75
ENDIF
ALLOCATE(tbuf(parmax))

!Read the data and reformat to desired format
errorflag = 0.
READ(ui,*)
ii=1
10 READ(ui,*,END=20) chdate,chtime,dum,(tbuf(ip),ip=1,parmax),awsid(ii),paramerror(ii)
 READ(chdate(1:4),'(i4)') iyear
 
 ibuf(ii,1)=iyear					! year

 IF (iyr.gt.iyear.and.iyear.lt.ilyear) THEN
  GOTO 10
 ELSE IF (iyr.eq.iyear) THEN

! awsid contains information about data source.
! paramerror contains a combination of information about missing data (decimals)
! 1ihgfedcba			(abcdefghi reverse)
! 1 = when 2 then rimed sample
! a = 1 = Windspeed
! b = 2 = Sin
! c = 3 = Sout
! d = 4 = Lin
! e = 5 = Lout
! f = 6 = Temperature
! g = 7 = humidity (error for relhum obs, but stored is q, based on rh, temp and pres)
! h = 8 = pressure
! i = 9 = altimeter

 IF (awsid(ii) == 0) errorflag(ii) = maxerror
 ! in this case errorflag will/should be 61

 ! First impact of rime = 1: 2  (MVT previously: negative) 
!  IF (paramerror(ii) < 0) errorflag(ii) = errorflag(ii) + 1.
!  paramerror(ii) = ABS(paramerror(ii))
 ip = 0
 dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 IF (NINT(dum) >= 2) errorflag(ii) = errorflag(ii) + 1.
 ! Second impact of temperature = 16: f = temperature = 6
 ip = 6
 dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 IF (NINT(dum) >= 1) errorflag(ii) = errorflag(ii) + 16.
 ! Third impact of Short wave in and out, and longwave in = 8: b = Sin = 2 and c = Sout = 3, d = Lin = 4
 ! was only Sin and Sout
 ip = 2
 dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 ip = 3
 dum2 = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 ip = 4
 dum3 = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 IF (NINT(dum) >= 1 .or. NINT(dum2) >= 1 .or. NINT(dum3) >= 1) errorflag(ii) = errorflag(ii) + 8.
 ! Fourth impact of wind speed = 4: a = wind speed = 1
 ip = 1
 dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 IF (NINT(dum) >= 1) errorflag(ii) = errorflag(ii) + 4.
 ! Last impact of air pressure and humidity = 2: g = RH = 7 h = P = 8
 ! was Lin
 IF (tcalc.eq.1) THEN ! no LWup so no SEB model
  ip = 5
  dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
  IF (NINT(dum) >= 1) errorflag(ii) = errorflag(ii) + 16.
 ENDIF
 ip = 7
 dum = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 ip = 8
 dum2 = INT(paramerror(ii) /(10**(ip-1))) - 10*INT(paramerror(ii) /(10**(ip)))
 IF (NINT(dum) >= 1 .or. NINT(dum2) >= 1) errorflag(ii) = errorflag(ii) + 2.
! all add up max is 30 + 1, if 30, than no data present
!  # ! 1ihgfedcba			(abcdefghi reverse)
!  # ! 1 = when 2 then rimed sample
!  # ! a = 1 = Windspeed
!  # ! b = 2 = Sin
!  # ! c = 3 = Sout
!  # ! d = 4 = Lin
!  # ! e = 5 = Lout
!  # ! f = 6 = Temperature
!  # ! g = 7 = humidity (error for relhum obs, but stored is q, based on rh, temp and pres)
!  # ! h = 8 = pressure
!  # ! i = 9 = altimeter

 ichs = index(chdate,'-') + 1
 iche = ichs + index(chdate(ichs:10),'-') - 2
 READ(chdate(ichs:iche),'(i2)') imth
 ichs = iche + index(chdate(iche:10),'-') 
 iche = 10
 READ(chdate(ichs:iche),'(i2)') idy
 iche = index(chtime,':') - 1
 READ(chtime(1:iche),'(i2)') ihour
 ichs = index(chtime,':') + 1
 iche = ichs + index(chtime(ichs:8),':') - 2
 READ(chtime(ichs:iche),'(i2)') imin
 ichs = iche+ index(chtime(iche:8),':')
 iche = 8
 READ(chtime(ichs:iche),'(i2)') isec
 IF (isec.gt.30) THEN
    isec = 0
    imin = imin+1
 ENDIF
 ibuf(ii,3)=ihour+imin/60.					! hour
 ibuf(ii,2)=julday(iyear,imth,idy) + ibuf(ii,3)/24.	! Julian day plus time

 WRITE(chyear,'(i4)') iyear
 WRITE(chmth,'(i2.2)') INT(imth)
 WRITE(chdy,'(i2.2)') INT(idy)
 WRITE(chhr,'(i2.2)') INT(ihour)
 WRITE(chmin,'(i2.2)') INT(imin)
! WRITE(chsec,'(i2.2)') INT(isec)
 WRITE(chsec,'(a2)') '00'
 chdtg(ii)=chyear//'-'//chmth//'-'//chdy//char(9)//chhr//':'//chmin//':'//chsec
 
 IF (ii.eq.1) jday2=ibuf(ii,2)+ibuf(ii,3)/24.
 IF (ii.eq.1) dhour(ii)=3600.
 IF (ii.gt.1) THEN
  jday1=ibuf(ii-1,2)		!+ibuf(ii-1,3)/24.
  jday2=ibuf(ii,2)			!+ibuf(ii,3)/24.
  dhour(ii-1) = NINT(((jday2-jday1)*24.*3600.)/100.)*100
  dhour(ii)=dhour(ii-1)
 ENDIF
 ih = 24-dhour(ii)/3600.
 
 IF(lmc.ne.0) THEN
  CALL RANDOM_NUMBER(rands) !0<rands(i)<1
  rands = (rands*2.)-1.		!-1<rands(i)<1
  
  IF (chstation.eq."ant_neuma") THEN
  !*******************************************************
  !Measurement errors for Neumayer  
  !*******************************************************
   Told = tbuf(6)														!Save old value for T as it is needed for RANDHUM
   Pold = tbuf(8)														!Idem
   tbuf(6) = tbuf(6)+0.1*rands(1)										!Temperature +- 0.1
   tbuf(8) = MAX(0.,tbuf(8)+0.5*rands(2))								!Pressure +- 0.5
   CALL RANDHUM(tbuf(7),tbuf(6),tbuf(8),Told,Pold)						!Measurement is relhum, data is spechum, so treat separately
   IF(tbuf(1) .gt. 10.) THEN
    tbuf(1) = tbuf(1)*(1.+0.05*rands(3))								!Windspeed +- 5%
   ELSE
    tbuf(1) = MAX(0.1,tbuf(1)+0.5*rands(3))								!Windspeed +- 0.5
   ENDIF
   
   tbuf(3) = MAX(0.,tbuf(3)+5.*rands(4))								!Shortwave +- 5
   tbuf(2) = MIN(tbuf(3)/albmax,MAX(tbuf(3)/albmin,tbuf(2)+5.*rands(5)))!Shortwave +- 5 (Sout/albmin<Sin<Sout/albmax)

   tbuf(5) = MAX(0.,MIN(loutmax,tbuf(5)+5.*rands(7)))					!Longwave +- 5 (Should be less than loutmax (Lout of T=Tkel))
   tbuf(4) = MAX(0.,tbuf(4)+5.*rands(6))								!Longwave +- 5
   
   !Disturbance in sonic height measurements
   tbuf(13) = MAX(0.,tbuf(13)+0.1*rands(8))								!zt +- 0.1
   tbuf(14) = MAX(0.,tbuf(14)+0.1*rands(8))								!zm +- 0.1
  ELSE
  !*******************************************************
  !Measurement errors for IMAU AWS
  !*******************************************************
   Told = tbuf(6)														!Save old value for T as it is needed for RANDHUM
   Pold = tbuf(8)														!Idem
   tbuf(6) = tbuf(6)+0.3*rands(1)										!Temperature +- 0.3
   tbuf(8) = MAX(0.,tbuf(8)+4.*rands(2))								!Pressure +- 4
   CALL RANDHUM(tbuf(7),tbuf(6)+Tkel,tbuf(8),Told+Tkel,Pold)			!Measurement is relhum, data is spechum, so treat separately
   tbuf(1) = MAX(0.1,tbuf(1)+0.3*rands(3))								!Windspeed +- 0.3
   tbuf(2) = MAX(0.,tbuf(2)*(1.+0.02*rands(4)))							!Shortwave +- 2%
   tbuf(3) = MAX(0.,tbuf(3)*(1.+0.02*rands(5)))							!Shortwave +- 2%
   tbuf(4) = MAX(0.,tbuf(4)+15.*rands(6))								!Longwave +- 15
   tbuf(5) = MAX(0.,tbuf(5)+15.*rands(7))								!Longwave +- 15
  
!Disturbance in sonic height measurements
!   tbuf(13) = MAX(0.,tbuf(13)+0.1*rands(8))								!zt +- 0.1
!   tbuf(14) = MAX(0.,tbuf(14)+0.1*rands(8))								!zm +- 0.1
  
!Wrong SW measurements because of tilt (+- 20%), same rands(i) for both as they are attached to the same mast
   tbuf(2) = tbuf(2)*(1.+0.2*rands(9))
   tbuf(3) = tbuf(3)*(1.+0.2*rands(9))
  ENDIF
 ELSE
  !Minimum wind speed
  IF (tbuf(1) .lt. 0.1) tbuf(1) = 0.1
  
  !Minimum pressure
  IF(tbuf(8) .lt. 0.0) tbuf(8) = 0.0
  
  !Minimum reflected shortwave radiation
  IF(tbuf(3) .lt. 0.0) tbuf(3) = 0.0
  
  !Incoming shortwave radiation should be between Sout/albmin and Sout/albmax
  IF(tbuf(2) .gt. tbuf(3)/albmax) THEN
   tbuf(2) = tbuf(3)/albmax
  ELSEIF(tbuf(2) .lt. tbuf(3)/albmin) THEN
   tbuf(2) = tbuf(3)/albmin
  ENDIF
  
  !Minimum longwave radiation
  IF(tbuf(4) .lt. 0.0) tbuf(4) = 0.0
  IF(tbuf(5) .lt. 0.0) tbuf(5) = 0.0
 ENDIF

!INPUT
!1=WS, 2=Sin, 3=Sout, 4=Lin, 5=Lout, 6=T, 7=RH/q, 8=P, 9=albedo, 10=albedo(run),
!11=zenith,12=Hserie(run), 13=Precip
!14=zt, 15=zm, 16=z0m, 17=AWSid, 18=valerror
!OUTPUT
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift determined from 16
 ibuf(ii,4)  = tbuf(6)		! temperature
 ibuf(ii,5)  = tbuf(8)		! pressure
 ibuf(ii,6)  = tbuf(1)		! windspeed
 ibuf(ii,7)  = tbuf(2)		! shortwave incoming radiation
 ibuf(ii,8)  = tbuf(3)		! shortwave reflected radiation
 ibuf(ii,9)  = tbuf(9) 		! albedo
 ibuf(ii,10) = tbuf(7)		! specific humidity
 ibuf(ii,11) = tbuf(4)		! long wave incoming radiation
 ibuf(ii,12) = tbuf(5)		! long wave outgoing radiation
 ibuf(ii,13) = tbuf(15)		! z0m = roughness length surface combi of snow and ice values.
 ibuf(ii,14) = tbuf(13)		! zt = height temperature sensor above surface
 ibuf(ii,15) = tbuf(14)		! zm = height wind speed sensor above surface
 ibuf(ii,16) = tbuf(11)		! sonic height obs in 1 series, preferably running mean height, at t = 0 equal dsnow, dsnow must be > 0
IF (chstation.eq."ant_neuma") THEN 
 ibuf(ii,17) = tbuf(12)*1.153	! Neumayer: precipitation taken from RACMO, scaled to fit stake observations
ELSE
 ibuf(ii,17) = tbuf(12)		! precipitation
ENDIF
 zenith(ii)  = tbuf(10)		! zenith angle

 IF ((chstation.eq."STGL_1999").or.(chstation.eq."SURE_2007")) THEN	!20-4-2012 CHECK!!!!!!
   WRITE(*,*) 'CHECK !!!!!!!!!!!!'
   ibuf(ii,16) = 100.-tbuf(11)		! sonic height obs in 1 series. at t = 0 tbuf(11) equals dsnow
 ENDIF
 
 IF (imth.eq.12.and.idy.eq.31.and.ihour.eq.ih) GOTO 30
 ii=ii+1
 GOTO 10

 ELSE IF (iyr.lt.iyear.and.ii.gt.1) THEN
   GOTO 20
 ELSE
  WRITE(*,'(/,A,2i5)') 'Your input data does not correspond with your input information',iyr,iyear
  WRITE(uo1,'(/,A,2i5)') 'Your input data does not correspond with your input information',iyr,iyear
  STOP 12
 ENDIF 
 
20 CONTINUE
 IF (ii.eq.1) THEN
  WRITE(*,'(/,A,2i5,/)') 'Your input data does not correspond with your input information',iyr,iyear
  WRITE(uo1,'(/,A,2i5,/)') 'Your input data does not correspond with your input information',iyr,iyear
  STOP 13
 ENDIF 
ii=ii-1
30 CONTINUE
ilast=ii
ilasttot = ilasttot + ii
ibday=INT(ibuf(1,2))
ilday=INT(ibuf(ilast,2))

errorflag(ilast+1) = errorflag(ilast)

if (lcomment == 1) write(*,'(/,4(A,I7))') 'ilast = ',ilast,', ilasttot = ',ilasttot,', jdaystart =', ibday,', jdaylast =', ilday
write(uo1,'(/,4(A,I7))') 'ilast = ',ilast,', ilasttot = ',ilasttot,', jdaystart =', ibday,', jdaylast =', ilday

REWIND(ui)
!Close input data file
IF (iyr.eq.ilyear) CLOSE(ui) 
DEALLOCATE(tbuf)

END SUBROUTINE INPDATA

!===============================================================================
SUBROUTINE CHECKDATA(&
!Input
& iyr ) 
!===============================================================================
! Routine that checks the input data and corrects when necessary
! linearly interpolates for missing values
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift determined from 16
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast, ibuf, buf , alb_old
USE SNOW_EBM , ONLY : hsnowmod, hsnowmod_l , dsnowh , hsnowstart , &
&                     dsnowr , dsnowice , corrsnow
USE INPUT_EBM , ONLY : dsnow , lcomment, ibyear, luseacc , lclimtemp, climtemp, &
&                      lclimrad, climrad, lclimprec, climprec, lclimws, climws, &
&                      albmin , albmax , trhoprec , rhosnprec, chstation
USE CONSTANTS_EBM , ONLY : colmax, Tkel, StefBoltz , loutmax

IMPLICIT NONE

! input
INTEGER,INTENT(IN) :: iyr
! Local
REAL , PARAMETER :: error = -990.
REAL :: rh, relhum, spechum
REAL :: emissivity
REAL :: dens , densprec
INTEGER :: ii,j,iv
INTEGER :: precipconvert

buf = -999.
buf = ibuf

precipconvert = 1
IF (chstation.eq."ant_neuma") precipconvert = 0
! 0 = nothing is done
! 1 = from AWS IMAU data it was first altimeter, then converted to precip in m snow 
! then with given density in convert converted to precip in m we. Here later on 
! converted again to m snow. in case precip from altimeter dens conversion should equal.
! trick here: convert in 'convert' with constant density of 400, than here converted 
! back to m snow and then again to mm we using dens given here. This ensures consistency.

! check all data points
DO ii = 1,ilast

! check to be sure that every year each value starts with a valid value! 
  IF (ii == 1 .or. ii == ilast) THEN
    DO iv=1,colmax-3		!17, 18-20 are snow acc, ice melt, drift
      IF (ibuf(ii,iv) < error .and. iyr == ibyear) THEN
        WRITE(*,*) 'Start or end with error value !!! STOP',ii,iv,iyr
        STOP 14
      ENDIF
    ENDDO
  ENDIF

  buf(ii,4) = ibuf(ii,4) + Tkel		! temperature in K
  buf(ii,5) = ibuf(ii,5) * 100.		! pressure in Pa
  buf(ii,10) = ibuf(ii,10) / 1000.	! Specific humidity in kg/kg
  
! set lower limit of wind speed
  IF (buf(ii,6) < 0.1) buf(ii,6) = 0.1

! set upper and lower limit albedo  
  IF (buf(ii,9) < albmin) buf(ii,9) = albmin
  IF (buf(ii,9) > albmax) buf(ii,9) = albmax
  
  IF (precipconvert == 1) THEN
    dens = 400.
    buf(ii,17) = buf(ii,17)/dens
    IF (trhoprec == 0) THEN		! constant value set in input
      dens = rhosnprec
    ELSE IF (trhoprec == 1) THEN	! value depends on wind speed and surface temperature
      dens = densprec(buf(ii,6),buf(ii,4))
    ENDIF
    buf(ii,17) = buf(ii,17)*dens
  ENDIF

  IF ( lclimtemp == 1 .and. climtemp /= 0. ) THEN
    rh = relhum(buf(ii,4),buf(ii,10),buf(ii,5)) 
    emissivity = buf(ii,11)/(StefBoltz*(buf(ii,4)**4))
    buf(ii,4) = buf(ii,4) + climtemp		! climate sensitivity
    buf(ii,10) = spechum(buf(ii,4),rh,buf(ii,5))		! change spec hum assuming rel hum remains constant
    buf(ii,11) = emissivity*StefBoltz*(buf(ii,4)**4)	! change Lin assuming constant emissivity
  ENDIF
  IF (lclimrad == 1 .and. climrad /= 0. ) THEN
    buf(ii,7) = buf(ii,7) * (1.+ climrad*0.01)		! climate sensitivity
    buf(ii,8) = buf(ii,8) * (1.+ climrad*0.01)		! climate sensitivity
  ENDIF
!  IF (lclimprec == 1 .and. climprec /= 0. ) THEN
!!    buf(ii,16) = ibuf(ii,16)*(1.+climprec*0.01)	! serie Climate sensitivity test with precip
!    buf(ii,17) = ibuf(ii,17)*(1.+climprec*0.01)	! precip Climate sensitivity test with precip
!  ENDIF
  IF (lclimws == 1 .and. climws /= 0. ) THEN
    buf(ii,6) = buf(ii,6) * (1.+ climws*0.01)		! climate sensitivity
  ENDIF

ENDDO	! ilast

! in case serie from sonic height ranger is available then calculate acc, melt and drift
IF (luseacc > 0) CALL GETACCUMANDMELT(iyr)
IF ((lclimtemp == 1) .or. (lclimprec == 1) .or. (lclimrad == 1) .or. (lclimws == 1) ) CALL CLIMACCUMANDMELT(iyr)


! initialise other necessary parameters
IF (iyr == ibyear) THEN
  dsnowr = MAX((buf(1,16)-dsnow),0.)		! initialise rest snow not yet put into a layer
  dsnow = buf(1,16)
  hsnowstart = dsnow
  hsnowmod = dsnow
  hsnowmod_l = dsnow
  dsnowh = dsnow
  corrsnow = 0.
  alb_old = buf(1,9)
ENDIF

IF (lcomment == 1) WRITE(*,*) 'END data checking, start EBM calculation'


END SUBROUTINE CHECKDATA
    
!===============================================================================
SUBROUTINE INTERP_DATA (&
! Input
& j,i, step, &
! Output
& nst)
!===============================================================================
! Routine that linearly interpolates the input data to the model timestep
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast, buf, sbuf, zm, zt , z0m , szenith, zenith
USE SNOW_EBM , ONLY : lid , hsnow , drift
USE CONSTANTS_EBM , ONLY : colmax
USE INPUT_EBM , ONLY : lz0m , z0msn , z0mice , luseacc

IMPLICIT NONE

! Input
INTEGER,INTENT(IN) :: i,j,step
! Output
REAL,INTENT(OUT) :: nst
! local
INTEGER :: iv , ivstart , ivend
REAL :: grad

IF (j.eq.0) nst=0

ivstart = 4
ivend = colmax
!CHR ADD IF- STATEMENT in case yes/no acc from sonic height ranger to set ivstart and ivend

IF (j == -1) THEN	! in case of error data continue to interpolate precip data
  ivstart = 17		! precip
  ivend = 20
  IF (luseacc == 0) ivend = 17		! 
ENDIF

sbuf = 0.

DO iv = ivstart,ivend
!    grad = (buf(i+1,iv) - buf(i,iv))/step
!    IF (i.eq.ilast) grad = (buf(i,iv) - buf(i-1,iv))/step
   if (i < ilast) then
    grad = (buf(i+1,iv) - buf(i,iv))/step
    sbuf(iv) = buf(i,iv) + nst*grad
    IF (iv == 17 .or. iv == 20) sbuf(iv) = buf(i+1,iv) / step 		! precip and drift
  else if (i.eq.ilast) then
    sbuf(iv) = buf(i,iv)
    IF (iv == 17 .or. iv == 20) sbuf(iv) = buf(i,iv) / step 		! precip and drift
  ENDIF
ENDDO

!Set other parameters per time step
z0m = sbuf(13)
zt = sbuf(14)
zm = sbuf(15)
! hsnow = depth snow layer, becomes negative after ice melt.
hsnow = sbuf(16)
drift = sbuf(20)
IF (luseacc <=1) drift = 0.

IF ((lz0m.eq.1).or.(lz0m.eq.2).or.(lz0m.eq.3)) THEN
  z0m = z0msn
  IF (luseacc >= 2) THEN
  	! z0m implicitly > 0
    IF (lz0m <= 3 .and. (sbuf(18) < 0.005)) THEN	! in this case observed surface type determines roughness
      z0m = z0mice
    ELSE IF (lz0m == 10 .and. (lid(1) == 0)) THEN	! in this case modelled surface type determines roughness
      z0m = z0mice
    ENDIF  
  ELSE IF (lid(1) == 0) THEN		! of lack of more information, must be based on model surface type
    z0m = z0mice
  ENDIF
ENDIF
! else value for z0m as a function of time is given in the input file, or parameterized

grad = (zenith(i+1) - zenith(i))/step
IF (i.eq.ilast) grad = zenith(i)	!(zenith(i) - zenith(i-1))/step
szenith = zenith(i) + nst*grad

nst=nst+1

END SUBROUTINE INTERP_DATA


!===============================================================================
SUBROUTINE INPUTRADPEN
!===============================================================================
! Routine that reads input information for the radiation penetration routine
!===============================================================================
USE RADPEN_EBM , ONLY : bandmax, lambda, SolarPlateauClear, SolarPlateauCloudy, SolarSeaClear, &
&                       dlambda, asymsn, qextsn, cosinglescatsn, asymice, qextice, cosinglescatice,&
&						asymAll, cosinglescatAll, qextAll, dlambdaAll, lambdaAll
USE INPUT_EBM , ONLY : radiussn,radiusice, lalbedo
USE FILES , ONLY : ui

IMPLICIT NONE

INTEGER :: k	
INTEGER :: ii
REAL :: radius

OPEN(unit=ui,file='IN_SolarSpectrum.txt',status='old')

READ(ui,*)
DO ii = 1,bandmax
  READ(ui,*) lambda(ii),SolarPlateauClear(ii),SolarPlateauCloudy(ii),SolarSeaClear(ii)
ENDDO
 CLOSE(ui)

IF(lalbedo .eq. 4 .or. lalbedo .eq. 5) THEN
 OPEN(ui,FILE='IN_Mie(r=0.05mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(1,ii), dlambdaAll(1,ii), asymAll(1,ii), qextAll(1,ii), cosinglescatAll(1,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=0.1mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(2,ii), dlambdaAll(2,ii), asymAll(2,ii), qextAll(2,ii), cosinglescatAll(2,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=0.2mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(3,ii), dlambdaAll(3,ii), asymAll(3,ii), qextAll(3,ii), cosinglescatAll(3,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=0.35mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(4,ii), dlambdaAll(4,ii), asymAll(4,ii), qextAll(4,ii), cosinglescatAll(4,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=0.5mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(5,ii), dlambdaAll(5,ii), asymAll(5,ii), qextAll(5,ii), cosinglescatAll(5,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=1.0mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(6,ii), dlambdaAll(6,ii), asymAll(6,ii), qextAll(6,ii), cosinglescatAll(6,ii)
 ENDDO 
 CLOSE(ui)
 OPEN(ui,FILE='IN_Mie(r=2.5mm).txt',status='old')
 DO ii=1,bandmax
  READ(ui,*) lambdaAll(7,ii), dlambdaAll(7,ii), asymAll(7,ii), qextAll(7,ii), cosinglescatAll(7,ii)
 ENDDO 
 CLOSE(ui)
ELSE
 DO k = 1,2
  IF (k == 1) THEN
   radius = radiussn
  ELSE
   radius = radiusice
  ENDIF

  IF (radius == 0.5e-4) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=0.05mm).txt',status='old')
  ELSEIF (radius == 1.0e-4) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=0.1mm).txt',status='old')
  ELSEIF (radius == 2.0e-4) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=0.2mm).txt',status='old')
  ELSEIF (radius == 3.5e-4) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=0.35mm).txt',status='old')
  ELSEIF (radius == 5.0e-4) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=0.5mm).txt',status='old') 
  ELSEIF (radius == 1.0e-3) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=1.0mm).txt',status='old')
  ELSEIF (radius == 2.5e-3) THEN
   OPEN(unit=ui,FILE='IN_Mie(r=2.5mm).txt',status='old')
  ELSE
   WRITE(*,*) 'Grain size is ', radius 
   WRITE(*,*) 'No Mie scattering file available for this grain size radius'
   STOP 15
  ENDIF

!!READ(ui,*)
! DO ii = 1,bandmax
!  READ(ui,*) lambda(ii),dlambda(ii),asym(ii),qext(ii),cosinglescat(ii)
! ENDDO

! CLOSE(ui)

  IF (k == 1) THEN
   DO ii = 1,bandmax
    READ(ui,*) lambda(ii),dlambda(ii),asymsn(ii),qextsn(ii),cosinglescatsn(ii)
   ENDDO
   CLOSE(ui)
  ELSE
   DO ii = 1,bandmax
    READ(ui,*) lambda(ii),dlambda(ii),asymice(ii),qextice(ii),cosinglescatice(ii)
   ENDDO
   CLOSE(ui)
  ENDIF
 ENDDO
ENDIF

END SUBROUTINE INPUTRADPEN

!===============================================================================
