!===============================================================================
!
! File with all routines related to the output of data and run information
!
!===============================================================================
SUBROUTINE OPENOUTPUT
!===============================================================================
! Opens files for output data
!===============================================================================
USE FILES, ONLY		: uo1, uo2, uo3, uo4, uo5, uo6, uo7, uo8, uo9, uo10 , uo11
USE INPUT_EBM, ONLY	: lz0m, chstation
IMPLICIT NONE
!Local
LOGICAL :: lexist

!WRITE(*,*) 'Open output files '
!OPEN(unit=uo1,file='runinfo.txt',status='unknown')

 IF(lz0m .ne. 2 .and. lz0m .ne. 3) THEN
  OPEN(unit=uo2,file='output1.txt',status='unknown')		!AWS/basic parameters
  OPEN(unit=uo3,file='output2.txt',status='unknown')		!Mass balance parameters
  OPEN(unit=uo4,file='output3.txt',status='unknown')		!snow characteristics
  OPEN(unit=uo5,file='output4.txt',status='unknown')		!snow first layer characteristics
  OPEN(unit=uo6,file='output5.txt',status='unknown')		!snow temperature vs obs
  OPEN(unit=uo7,file='output6.txt',status='unknown')		!month
  OPEN(unit=uo8,file='output7.txt',status='unknown')		!month climate
  OPEN(unit=uo9,file='output8.txt',status='unknown')		!year
  OPEN(unit=uo10,file='output9.txt',status='unknown')		!daily averages
  OPEN(unit=uo11,file='output10.txt',status='unknown')		!seasonal averages

!HOUR / AWS
  WRITE(uo2,*) ' date	hour year day'//&
&            ' Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) Cloud(-) '//&
&            ' Albedo(obs,-) Albedo(mod,-) Zenith(-)'//&
&            ' rest(W/m2) SUM(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) Spen/Sin(-)'//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' Ch(-) Cq(-) ustar(m/s) thstar(K) qstar(g/kg) psim(-) psih(-) psiq(-)'//&
&            ' zt(m) zm(m) z0m(m) z0h(m) z0q(m) Hice(m) dHice(m)'//&
&            ' error(-) awsid(-) paramerror(-)'
!Mass balance
WRITE(uo3,*) ' date	hour'//&
&            ' hsnow(obs,m) precip(obs,mmwe) drift(obs,m) acc(obs,m) icemelt(obs,m) cumicemelt(obs,m)'//&
&            ' hsnowmod(mod,m) precip(mod,mmwe) drift(mod,m) accsnow(mod,m) icemelt(mod,m) cumicemelt(mod,m)'//&
&            ' hmass(mmwe) totwater(mmwe) topwater(mmwe) topsnow(m) topmass(mmwe)'//&
&            ' cummelt(mmwe) melt(mmwe/dt) cumsurfmelt(mmwe) surfmelt(mmwe/dt)'//&
&            ' runoff(mmwe/dt) subl(mmwe/dt) slushdepth(m) surfwater(mmwe)'//&
&            ' error(-) errorobs(-)'
!SNOW (snow profiles)
  WRITE(uo4,*) 'date	hour Year.day il depth(m) thickness(m) temp(K) dens(kg/m3) mass(kg/m2) water(kg/m2) ice(kg/m2) '//&
&                   'Spenetration(W/m2) cp(J/Kkg) irrwater energy(J/m2) grainsize(m) id hsnowmod(m) hsnow(m) error'
  WRITE(uo5,*) ' date	hour year day thickness(m) temp(K) T0(K) dens(kg/m3) mass(kg/m2) water(kg/m2)'//&
&                   ' ice(kg/m2) Spenetration(W/m2) cp(J/Kkg)) K(J/Kms) irrwater energy(J/m2) grainsize(m) id'
  WRITE(uo6,*) ' date	hour  year day T1ob T2ob T3ob T4ob T5ob D1ob D2ob D3ob D4ob D5ob '//&
&            ' T1mo T2mo T3mo T4mo T5mo D1mo D2mo D3mo D4mo D5mo'
!MONTH 10+5+5+8+5+2+3+1 = 39
  WRITE(uo7,*) ' date Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&	
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) cloud(-) '//&
&            ' M(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) '//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' precip(mmwe/dt) melt(mmwe/dt) surfmelt(mmwe/dt) runoff(mmwe/dt) subl(mmwe/dt) acc(mmwe/dt)'//&
&            ' zt(m) zm(m) hsnow(obs,m) hsnowmod(mod,m) '//&
&            ' pdd(nr) sumpdd(deg) meltdys(nr)'//&
&            ' error(-)'
!MONTH CLIMATE 10+5+5+8+3+2 = 33 
  WRITE(uo8,*) ' month Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) cloud(-) '//&
&            ' M(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) '//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' pdd(nr) sumpdd(deg) meltdys(nr)'//&
&            ' error(-) count(-)'
!Seasonal 10+5+5+8+5+2+3+1 = 39
  WRITE(uo11,*) ' season year iseason Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&	
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) cloud(-) '//&
&            ' M(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) '//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' precip(mmwe/dt) melt(mmwe/dt) surfmelt(mmwe/dt) runoff(mmwe/dt) subl(mmwe/dt) acc(mmwe/dt)'//&
&            ' zt(m) zm(m) hsnow(obs,m) hsnowmod(mod,m)'//&
&            ' pdd(nr) sumpdd(deg) meltdys(nr)'//&
&            ' error(-)'
!YEAR 10+5+5+8+5+5+3+3+3+3+3+1 = 54
  WRITE(uo9,*) ' year Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) cloud(-) '//&
&            ' M(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) '//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' precip(mmwe/dt) melt(mmwe/dt) surfmelt(mmwe/dt) runoff(mmwe/dt) subl(mmwe/dt) acc(mmwe/dt)'//&
&            ' zt(m) zm(m) hsnow(obs,m) hsnowmod(mod,m) dystmelt(dys) dyndmelt(dys) lengthseas(dys)'//&
&            ' winterbal(obs,msnow) summerbal(obs,msnow) annualbal(obs,msnow)'//&
&            ' winterbal(obs,mwe) summerbal(obs,mwe) annualbal(obs,mwe)'//&
&            ' winterbal(mod,mwe) summerbal(mod,mwe) annualbal(mod,mwe)'//&
&            ' wintermelt(mod,mwe) summermelt(mod,mwe) annualmelt(mod,mwe)'//&
&            ' pdd(nr) sumpdd(deg) meltdys(nr)'//&
&            ' error(-)'
!DAY 10+5+5+8+3+4+3 = 38
  WRITE(uo10,*) ' date Temp(K) T2m(K) Tpot(K) T0(K) T0lout(K) dT0(k) Tinv(K) Q(g/kg) Q2m(g/kg) Q0(g/kg)'//&
&            ' RH2m(%) WS(m/s) WS10m(m/s) P(hPa) cloud(-) '//&
&            ' M(W/m2) Snet(W/m2) Sin(W/m2) Sout(W/m2) Spen(W/m2) '//&
&            ' Lnet(W/m2) Lin(W/m2) Lout(W/m2) LoutObs(W/m2) Rnet(W/m2) SH(W/m2) LE(W/m2) G(W/m2)'//&
&            ' alb_obs(-) alb_calc(-) precip(mmwe/dt)'//&
&            ' melt(mmwe/dt) surfmelt(mmwe/dt) runoff(mmwe/dt) subl(mmwe/dt) acc(mmwe/dt)'//&
&            ' zt(m) zm(m) hsnow(obs,m) hsnowmod(mod,m) error(-)'
ENDIF

IF(lz0m == 2 .or. lz0m == 3) THEN !If z0 is being changed, create output file if it doesn't exist yet
 INQUIRE(file='../Scripts/random_'//chstation//'.txt',EXIST=lexist)
 IF(lexist) THEN
  OPEN(uo2,file='../Scripts/random_'//chstation//'.txt',status='old',action='write',position='append')
 ELSE
  WRITE(*,*) "Creating outputfile for random test results: ../Scripts/random_"//chstation//".txt"
  OPEN(uo2,file='../Scripts/random_'//chstation//'.txt',status='new',action='write')
  WRITE(uo2,'(A193)') ' 2/3 z0msn/ice rhosnpr/albmin summelt meltjan meltfeb meltmar meltapr meltmay meltjun meltjul'//&
  &  '  meltaug meltsep meltoct meltnov meltdec meandt0 rmsddt0 stddt0 PearsonRdt0 meandacc/mel stddacc/mel'
 ENDIF
ENDIF

END SUBROUTINE OPENOUTPUT

!===============================================================================
SUBROUTINE CLOSEOUTPUT
!===============================================================================
! Opens files for output data
!===============================================================================
USE INPUT_EBM, ONLY: lz0m
USE FILES , ONLY : uo1, uo2, uo3, uo4, uo5 , uo6 , uo7 , uo8 , uo9 , uo10 , uo11
IMPLICIT NONE

 CLOSE(uo1)
 CLOSE(uo2)
 
 IF(lz0m .ne. 2 .and. lz0m .ne. 3) THEN
  CLOSE(uo3)
  CLOSE(uo4)
  CLOSE(uo5)
  CLOSE(uo6)
  CLOSE(uo7)
  CLOSE(uo8)
  CLOSE(uo9)
  CLOSE(uo10)
  CLOSE(uo11)
 ENDIF

END SUBROUTINE CLOSEOUTPUT

!===============================================================================
SUBROUTINE PREPAREOUTPUT(&
!Input
& iyr , icount, jcount )
!===============================================================================
! Prepare different arrays for output
!OUTPUT
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 
!===============================================================================
USE GLOBALS_EBM , ONLY : nstep , ilast , ilasttot , chdtg , dhour , sbuf , buf , errorflag , dbuf , dcount , &
&                 mbuf , mcount , ssbuf , scount , clbuf , clcount , clmth , ybuf , ycount , perbuf , percount , &
&                 t2m , t0 , q2m , q0 , ws10m , source, Snet, Lnet, SH, LE, GH , &
&                 t0obs , dt0 , tinv , tpot , Lout , Loutobs , Rnet , SpenOSin , &
&                 cloud , dcloud , mcloud , scloud , ycloud , clcloud , zt, zm, &
&                 albedo , dalbedo , dalbedo_obs , derror , merror , serror , clerror , yerror ,  &
&                 ldwrite , lmwrite , lclwrite , lyrwrite , lswrite , lreset_z0, &
&                 dt0buf , dt0sum , dt0stdsum , dt0sumcount , dt02sum , &
&                 t0sum, t0obssum, t0t0obssum, t02sum, t0obs2sum , &
&                 pearsonR , daccsum, dacc2sum , dmelsum, dmel2sum , &
&                 dsmbsum , dsmb2sum , dsmbsumcount , paramerror , &
&                 snowcorrobs , icemeltcorrmod , icemeltcorrobs, accobs , maxdays
USE SNOW_EBM , ONLY : hsnow , hsnowmod , hsnowout , runoff, surfmelt, melt , subl , precipsum , sumdrift , &
&              summelt, sumsurfmelt, sumrunoff , sumacc , precipsum , sumdrift , &
&              mpdd , mtsumpdd , mnrmeltdys , cpdd , ctsumpdd , cnrmeltdys , &
&              spdd , stsumpdd , snrmeltdys , ypdd , ytsumpdd , ynrmeltdys , &
&              wintermelt, summermelt, annualmelt, &
&              stmeltout , ndmeltout , lengthseas , &
&              cumdrift , cumdriftobs , precipobs , dsnowacc , icemeltout
USE RADPEN_EBM , ONLY : sumdivs
USE INPUT_EBM , ONLY : chstation , lerrorgap , ilyear , ibyear , &
                mbsumdy , mbwindy , luseacc, Hmax_month
USE CONSTANTS_EBM , ONLY : errorval , Tkel , emis , StefBoltz , rd , cp , mmax , maxerror

IMPLICIT NONE

!Input
INTEGER :: iyr,icount, jcount
!Functions
REAL	:: relhum
!Local
INTEGER :: i,j,ii,ip , idh
INTEGER :: maxdcount,maxmcount,maxclcount,maxycount,maxscount
INTEGER :: iyear,imth,idy,ihr,imin,iseas
INTEGER :: jaar(12),jaarl(12),year(12)
REAL :: fracmax
REAL :: hr,lhr,dyyear
REAL :: errobs(9)
!REAL :: accobs

jaar(1:12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
jaarl(1:12) = (/31,29,31,30,31,30,31,31,30,31,30,31/)

! first determine whether to write out data or not
READ(chdtg(icount)(1:4),'(i4)') iyear
READ(chdtg(icount)(6:7),'(i2)') imth
READ(chdtg(icount)(9:10),'(i2)') idy
READ(chdtg(icount)(12:13),'(i2)') ihr
READ(chdtg(icount)(15:16),'(i2)') imin
hr = ihr*1.+imin/60.
IF (chstation.eq."LF_0710") hr = hr - 0.25
lhr = 24.-dhour(icount)/3600.

! determine leapyear or not
IF(maxdays.eq.366) THEN
 year = jaarl
ELSE
 year = jaar
ENDIF

IF (imth == 12 .or. imth == 1 .or. imth == 2) iseas = 1
IF (imth == 3 .or. imth == 4 .or. imth == 5) iseas = 2
IF (imth == 6 .or. imth == 7 .or. imth == 8) iseas = 3
IF (imth == 9 .or. imth == 10 .or. imth == 11) iseas = 4

fracmax = 0.75
IF ( chstation(1:3) == 'ant' .and. chstation.ne.'ant_neuma' .and. iyear <= 2002 ) fracmax = 0.75*0.5
IF (lerrorgap .eq. 2) fracmax = 0.25

!logicals determining yes or no writing data to file
ldwrite = 0		!logical determining yes or no writing daily data to file
IF (hr == lhr) ldwrite = 1
IF (icount == ilast) ldwrite = 1
lmwrite = 0		!logical determining yes or no writing monthly data to file
IF ((idy == year(imth)).and.(hr == lhr)) lmwrite = 1
IF (icount == ilast) lmwrite = 1
lreset_z0 = 0		!logical determining yes or no resseting z0m paramweterization
IF ((imth == Hmax_month).and.(lmwrite == 1)) lreset_z0 = 1
lclwrite = 0	!logical determining yes or no writing monthly climatological data to file
IF (iyr == ilyear) lclwrite = 1
lswrite = 0		!logical determining yes or no writing seasonal data to file
IF ( (imth + 1)/3. == INT((imth + 1)/3.) .and. (lmwrite == 1)) lswrite = 1
IF ((iyr == ilyear).and.(icount == ilast)) lswrite = 1
lyrwrite = 0		!logical determining yes or no writing yearly data to file
IF ((imth == 12).and.(lmwrite == 1)) lyrwrite = 1
IF ((iyr == ilyear).and.(icount == ilast)) lyrwrite = 2

! max possible number of samples in average
idh = icount
IF ((iyr == ilyear).and.(icount > ilast-2)) idh = icount -2
maxdcount = 24./(dhour(idh)/3600.)				! day
maxmcount = year(imth)*24./(dhour(idh)/3600.)	! month
!maxclcount = maxmcount*(ilyear - ibyear)			! climatology
maxycount = maxdays*24./(dhour(idh)/3600.)		! year
maxscount = 90										! season
IF ((iseas == 1 .and. maxdays == 366) .or. (iseas == 4)) maxscount = maxscount + 1
IF (iseas == 2 .or. iseas == 3) maxscount = maxscount + 2
maxscount = maxscount*24/(dhour(idh)/3600.)				! season

! In paramerror: 
! 1 = Windspeed, 2 = Sin, 3 = Sout, 4 = Lin, 5 = Lout, 6 = Temperature
! 7 = humidity (error for relhum obs, but stored is q, based on rh, temp and pres)
! 8 = pressure, 9 = altimeter

DO ip = 1,9
  errobs(ip) = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
ENDDO

! Determine some diagnostic variables such as tpot and tinv
IF (jcount >= 0) THEN
 t0obs=(((sbuf(12)-(1.-emis)*sbuf(11))/(emis*StefBoltz))**0.25)
 dt0 = t0-t0obs
 tinv = t2m - t0
 tpot = t2m*((100000./sbuf(5))**(rd/cp))
 SpenOSin = -999.
 IF (sbuf(7) > 0.) SpenOSin = sumdivs/sbuf(7)
 Lout = emis*StefBoltz*(t0)**4
 Loutobs = sbuf(12)-(1.-emis)*sbuf(11)
 Rnet = Snet+Lnet
 IF (errobs(5) > 0) THEN
   t0obs=-999.9		!no valid lwout obs 
   Loutobs = -999.9
 ENDIF

ELSE 
 t0obs = errorval -90.
 dt0 = errorval -90.
 tinv = errorval -90.
 tpot = errorval -90.
 Loutobs = errorval -90.
 IF (errobs(5) < 1) THEN
   IF ((emis < 1. .and. errobs(4) < 1).or.(emis == 1.)) THEN
     t0obs=(((sbuf(12)-(1.-emis)*sbuf(11))/(emis*StefBoltz))**0.25)
     Loutobs = sbuf(12)-(1.-emis)*sbuf(11)
   ENDIF
 ENDIF
 SpenOSin = -999.
 Lout = errorval -90.
 Rnet = errorval -90.
ENDIF

IF (sbuf(6) > 0.1 .and. errorflag(icount) <= 0. .and. &
&   errobs(5) == 0 .and. errobs(4) == 0 .and. lyrwrite .ne. 2) THEN
!  IF ( .not.(sbuf(7) == 0 .and. t0obs > sbuf(4))) THEN ! takes into account that in case of no incoming radiation surface temp should be smaller than air temp
    dt0sum = dt0sum + dt0
    dt0sumcount = dt0sumcount +1
    dt02sum = dt02sum + dt0*dt0
    dt0buf(icount) = dt0
    t0sum = t0sum + t0
    t0obssum = t0obssum + t0obs
    t0t0obssum = t0t0obssum + t0*t0obs
    t02sum = t02sum + t0*t0
    t0obs2sum = t0obs2sum + t0obs*t0obs
!  ENDIF
ENDIF

IF (luseacc > 0) THEN
! correct output series snowheight in case of climate testing
  hsnowout = hsnow		!observations
  accobs = snowcorrobs	! observations of accumulation
  IF (sbuf(18)-accobs < 0.) THEN
    accobs = sbuf(18)
  ENDIF

  cumdrift = cumdrift + sumdrift
  cumdriftobs = cumdriftobs + sbuf(20)
  precipobs = sbuf(17)*nstep

  IF ((errobs(9) < 1 .and. errorflag(icount) <= 1.) .or. (lerrorgap == 2 .and. errobs(9) < 1)) THEN
    dsmbsum = dsmbsum + (hsnowmod-hsnowout)
    dsmbsumcount = dsmbsumcount + 1
    dsmb2sum = dsmb2sum + (hsnowmod-hsnowout)*(hsnowmod-hsnowout)
    daccsum = daccsum + (dsnowacc-((sbuf(18)-accobs)))
    dacc2sum = dacc2sum + (dsnowacc-((sbuf(18)-accobs)))*(dsnowacc-((sbuf(18)-accobs)))
    dmelsum = dmelsum + ((icemeltout-icemeltcorrmod) - (sbuf(19)-icemeltcorrobs))
    dmel2sum = dmel2sum + ((icemeltout-icemeltcorrmod) - (sbuf(19)-icemeltcorrobs))**2
  ENDIF
ELSE
  hsnowout = hsnow
  precipobs = precipsum
  cumdrift = 0.
  cumdriftobs = 0.
  sumdrift = 0.
  snowcorrobs = 0.
  icemeltcorrobs = 0.
ENDIF

! In paramerror, errobs: 
! 1 = Windspeed, 2 = Sin, 3 = Sout, 4 = Lin, 5 = Lout, 6 = Temperature
! 7 = humidity (error for relhum obs, but stored is q, based on rh, temp and pres)
! 8 = pressure, 9 = altimeter

IF (errobs(6) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN					! air temperature observation problem
 dbuf(1) = dbuf(1) + sbuf(4)				! air temperature in K
 dcount(1) = dcount(1) + 1
ENDIF
IF ((errobs(6) == 0 .and. errobs(7) == 0 .and.errobs(8) == 0) .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN ! specific humidity observation problem
 dbuf(8) = dbuf(8) + 1000.*sbuf(10)			! specific humidity in g/kg, needs T to convert from RH
 dcount(8) = dcount(8) + 1
ENDIF
IF (errobs(1) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN	 				! wind speed observation problem
 dbuf(12) = dbuf(12) + sbuf(6)				! wind speed in m/s
 dcount(12) = dcount(12) + 1
ENDIF
IF (errobs(8) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN					! no observations available at all
 dbuf(14) = dbuf(14) + sbuf(5)*0.01			! air pressure in hPa
 dcount(14) = dcount(14) + 1
ENDIF
IF (errobs(2) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN	 				! incoming short wave radiation observation problem
 dbuf(17) = dbuf(17) + sbuf(7)				! incoming short wave radiation in W/m2
 dcount(17) = dcount(17) + 1
ENDIF
IF (errobs(3) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN					! outgoing short wave radiation observation problem
 dbuf(18) = dbuf(18) + sbuf(8)				! reflected short wave radiation in W/m2
 dcount(18) = dcount(18) + 1
ENDIF
IF (errobs(4) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN 					! incoming long wave radiation problem
 dbuf(21) = dbuf(21) + sbuf(11)				! incoming long wave radiation in W/m2
 dcount(21) = dcount(21) + 1
ENDIF
IF (errobs(5) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN ! long wave radiation problem
 IF ((emis == 1) .or. (emis < 1 .and. errobs(4) == 0) ) THEN
  dbuf(5) = dbuf(5) + (((sbuf(12)-(1.-emis)*sbuf(11))/(emis*StefBoltz))**0.25)		! observed surface temperature in K
  dbuf(23) = dbuf(23) + sbuf(12)-(1.-emis)*sbuf(11)		! observed outgoing long wave radiation in W/m2
  dcount(5) = dcount(5) + 1 
  dcount(23) = dcount(23) + 1 
 ENDIF
 IF (errobs(4) == 0 .or. ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN		! in case of rime, no comparison of t0
  IF (((jcount >= 0).and.(errorflag(icount) < 1)) .or. &
&   ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN
   dbuf(6) = dbuf(6) + (t0-(((sbuf(12)-(1.-emis)*sbuf(11))/(emis*StefBoltz))**0.25))		! difference in modelled and observed surface temperature 
   dcount(6) = dcount(6) + 1
  ENDIF
 ENDIF
ENDIF


! -----------------------------------------------------------
! start writing to avarage arrays

IF (((jcount >= 0).and.(errorflag(icount) < 1)) .or. &
&   ((lerrorgap == 2).and.(errorflag(icount) < maxerror))) THEN	! this does include the rimed values, and in case lerrorgap == 2 also the gaps

 dbuf(2) = dbuf(2) + t2m					! 2 m air temperature in K
 dbuf(3) = dbuf(3) + t2m*((100000./sbuf(5))**(rd/cp))	! potential temperature
 dbuf(4) = dbuf(4) + t0						! modelled surface temp in K
 dbuf(7) = dbuf(7) + t2m-t0					! temperature inversion
 dbuf(9) = dbuf(9) + 1000.*q2m				! 2m specific humidity in g/kg
 dbuf(10) = dbuf(10) + 1000.*q0				! surface saturated specific humidity in  g/kg
 dbuf(11) = dbuf(11) + 100.*relhum(t2m,q2m,sbuf(5)) ! relative humidity
 dbuf(13) = dbuf(13) + ws10m				! 10m wind speed in m/s
 dbuf(15) = dbuf(15) - source				! sum of all fluxes Snet+Lnet+SH+LE+G in W/m2
 dbuf(16) = dbuf(16) + Snet					! net short wave radiation in W/m2
 dbuf(19) = dbuf(19) + sumdivs				! penetrated short wave radiation in W/m2
 dbuf(20) = dbuf(20) + Lnet					! net long wave radiation in W/m2
 dbuf(22) = dbuf(22) + emis*StefBoltz*(t0)**4		! modelled outgoing long wave radiation in W/m2
 dbuf(24) = dbuf(24) + Snet + Lnet			! net radiation in W/m2
 dbuf(25) = dbuf(25) + SH					! sensible heat flux in W/m2
 dbuf(26) = dbuf(26) + LE					! latent heat flux in W/m2
 dbuf(27) = dbuf(27) + GH					! subsurface heat flux in W/m2
 dbuf(28) = dbuf(28) + precipsum			! precipitation in mm w.e.
 dbuf(29) = dbuf(29) + melt					! melt in mm w.e.
 dbuf(30) = dbuf(30) + surfmelt				! melt in mm w.e.
 dbuf(31) = dbuf(31) + runoff				! runoff in mm w.e.
 dbuf(32) = dbuf(32) + subl					! sublimation in mm w.e.
 dbuf(33) = dbuf(33) + precipsum - sumdrift	! accumulation in mm w.e.
 dbuf(34) = dbuf(34) + zt				! T sensor height 
 dbuf(35) = dbuf(35) + zm				! U sensor height 
! dbuf(34) = dbuf(34) + hsnowout				! modeled cumulative mass balance
! dbuf(35) = dbuf(35) + hsnowmod				! measured cumulative mass balance
 dbuf(36) = hsnowout				! modeled cumulative mass balance
 dbuf(37) = hsnowmod				! measured cumulative mass balance  

 dcount(2) = dcount(2) + 1
 dcount(3) = dcount(3) + 1
 dcount(4) = dcount(4) + 1
 dcount(7) = dcount(7) + 1
 dcount(9) = dcount(9) + 1
 dcount(10) = dcount(10) + 1
 dcount(11) = dcount(11) + 1
 dcount(13) = dcount(13) + 1
 dcount(15) = dcount(15) + 1
 dcount(16) = dcount(16) + 1
 dcount(19) = dcount(19) + 1
 dcount(20) = dcount(20) + 1
 dcount(22) = dcount(22) + 1
 DO ii = 24,mmax
  dcount(ii) = dcount(ii) + 1
 ENDDO
 dcloud = dcloud + cloud
 dalbedo = dalbedo + albedo
 
 sumrunoff = sumrunoff + runoff
 summelt = summelt + melt
 sumsurfmelt = sumsurfmelt + surfmelt
 sumacc = sumacc + precipsum - sumdrift

! ------
! melt sums based on model results
  IF (mbsumdy < mbwindy .and. melt > 0.) THEN		!NH  150 300
    IF ((buf(icount,2) < mbwindy) .and. (buf(icount,2) >= mbsumdy)) THEN		! determine a summer melt
      summermelt = summermelt + melt
    ELSE IF ((buf(icount,2) >= mbwindy) .or. (buf(icount,2) < mbsumdy)) THEN		! determine a winter melt   
      wintermelt = wintermelt + melt
    ENDIF
  ELSE IF (mbsumdy > mbwindy .and. melt > 0.) THEN		!SH 335 30
    IF ((buf(icount,2) <= mbwindy) .or. (buf(icount,2) > mbsumdy)) THEN		! determine a summer melt  
      summermelt = summermelt + melt
    ELSE IF ((buf(icount,2) > mbwindy) .and. (buf(icount,2) <= mbsumdy)) THEN		! determine a winter melt
      wintermelt = wintermelt + melt    
    ENDIF
  ENDIF
  annualmelt = summermelt + wintermelt
  
! if (melt > 0) write(*,*) icount,buf(icount,2),wintermelt,summermelt,annualmelt,summelt,melt

ENDIF

!IF (errorflag(icount) > 0) derror = derror + 1!errorflag(icount)
!IF (errorflag(icount) > 0) derror = derror + MIN(errorflag(icount)/30.,1.)
derror = derror + MAX(0.,(30.-errorflag(icount))/30.)


! ---------- daily averages is on ------------
IF (ldwrite == 1) THEN		! determine daily averages
! add up for monthly values
  mcloud = mcloud + dcloud
  merror = merror + derror
  DO i = 1, mmax
! add up for monthly values
    IF (i < 36) THEN
      mbuf(i) = mbuf(i) + dbuf(i)
    ELSE
      mbuf(i) = dbuf(i)
    ENDIF
    mcount(i) = mcount(i) + dcount(i)
! make average for output
    IF (dcount(i) > fracmax*maxdcount) THEN
!      IF (i < 28 .or. i > 33) dbuf(i) = dbuf(i)/dcount(i)
      IF (i < 28 .or. i == 34 .or. i == 35) dbuf(i) = dbuf(i)/dcount(i)
    ELSE
      dbuf(i) = -999.5
    ENDIF
  ENDDO
  IF (dcount(mmax) > 0.) THEN
    dcloud = dcloud / dcount(mmax)
    dalbedo = dalbedo / dcount(mmax)
  ENDIF
  derror = (maxdcount - derror) / maxdcount !dcount(mmax)
  IF (dbuf(17) > 0. .and. dbuf(18) > 0. ) THEN
   dalbedo_obs=dbuf(17)/dbuf(16) 
  ELSE
   dalbedo_obs=-999.5
  ENDIF

! determine counters for degree day and melt days characteristics
  IF (dbuf(29) > 0) THEN
    mnrmeltdys = mnrmeltdys+1	!counter nr of melt days
    snrmeltdys = snrmeltdys+1	!counter nr of melt days
    ynrmeltdys = ynrmeltdys+1	!counter nr of melt days
  ENDIF
  IF ((dbuf(2)-Tkel) > 0) THEN
    mpdd = mpdd + 1		! counter positive degree days
    mtsumpdd = mtsumpdd + dbuf(2)-Tkel 	! summation of amauntpositive degree days
    spdd = spdd + 1		! counter positive degree days
    stsumpdd = stsumpdd + dbuf(2)-Tkel 	! summation of amauntpositive degree days
    ypdd = ypdd + 1		! counter positive degree days
    ytsumpdd = ytsumpdd + dbuf(2)-Tkel 	! summation of amauntpositive degree days
  ENDIF
ENDIF !ldwrite

! ---------- monthly averages is on ------------
IF (lmwrite == 1) THEN
! add up for climatological, seasonal and annual averages
  clcloud(imth) = clcloud(imth) + mcloud
  cpdd(imth) = cpdd(imth)+ mpdd
  ctsumpdd(imth) = ctsumpdd(imth) + mtsumpdd 
  cnrmeltdys(imth) = cnrmeltdys(imth) + mnrmeltdys
  scloud = scloud + mcloud
  serror = serror + merror
  ycloud = ycloud + mcloud
  yerror = yerror + merror
  clerror(imth) = clerror(imth) + merror
  clmth(imth) = clmth(imth) + 1
  DO i=1,mmax
! add up for climatological and annual averages
    IF (i < 36) THEN
      ssbuf(i) = ssbuf(i) + mbuf(i)
      ybuf(i) = ybuf(i) + mbuf(i)
    ELSE
      ssbuf(i) = mbuf(i)
      ybuf(i) = mbuf(i)
    ENDIF      
    scount(i) = scount(i) + mcount(i)
    ycount(i) = ycount(i) + mcount(i)
! make average for output
    IF (mcount(i) > fracmax*maxmcount) THEN
      clbuf(imth,i) = clbuf(imth,i) + mbuf(i)
      clcount(imth,i) = clcount(imth,i) + mcount(i)
!      if (i == 2) clmth(imth) = clmth(imth) + 1
!      IF (i < 28 .or. i > 33) mbuf(i) = mbuf(i)/mcount(i)
      IF (i < 28 .or. i == 34 .or. i == 35) mbuf(i) = mbuf(i)/mcount(i)
    ELSE
      mbuf(i) = -999.5
    ENDIF
  ENDDO
  IF (mcount(mmax) > 0.) THEN
    mcloud = mcloud / mcount(mmax)
  ENDIF
  merror = (maxmcount - merror) / maxmcount	!mcount(mmax)

! ---------- Seasonal averages is on ------------
! only possible when it is end of month
  IF ( lswrite == 1 ) THEN
    DO i=1,mmax
! add up to make total period average for output
     IF (scount(i) > fracmax*maxscount) THEN
! make annual average for output
!      IF (i < 28 .or. i > 33) ssbuf(i) = ssbuf(i)/scount(i)
      IF (i < 28 .or. i == 34 .or. i == 35) ssbuf(i) = ssbuf(i)/scount(i)
     ELSE
      ssbuf(i) = -999.5
     ENDIF
    ENDDO
    IF (scount(mmax) > 0.) THEN
      scloud = scloud / scount(mmax)
    ENDIF
    serror = (maxscount - serror) / maxscount !ycount(mmax)
  
  ENDIF

! ---------- Climatological averages is on ------------
! only possible when it is end of month
  IF ( lclwrite == 1 ) THEN
! make climatological average for output
    DO i=1,mmax
     IF (clcount(imth,i) > 0.) THEN
!      IF (i < 28 .or. i > 33) clbuf(imth,i) = clbuf(imth,i)/clcount(imth,i)
      IF (i < 28 .or. i == 34 .or. i == 35) clbuf(imth,i) = clbuf(imth,i)/clcount(imth,i)
     ELSE
      clbuf(imth,i) = -999.5
     ENDIF
    ENDDO
    IF (clcount(imth,mmax) > 0.) THEN
      clcloud(imth) = clcloud(imth) / clcount(imth,mmax)
    ENDIF
    IF (clmth(imth) > 0) THEN
      cpdd(imth) = cpdd(imth)/clmth(imth)
      ctsumpdd(imth) = ctsumpdd(imth) /clmth(imth)
      cnrmeltdys(imth) = cnrmeltdys(imth) /clmth(imth)
    ENDIF
    maxclcount = maxmcount*clmth(imth)
    IF (imth == 2) maxclcount = maxclcount + ((ilyear - ibyear + 1)/4.)*maxdcount
    clerror(imth) = (maxclcount - clerror(imth)) / maxclcount !ycount(mmax)

! in case last month is not december calculate averages for months up to 12 any way
    j = 0
    IF ((iyr == ilyear).and.(icount == ilast).and.(imth < 12)) THEN
     DO j=imth+1,12
      maxmcount = year(j)*24./(dhour(idh)/3600.)	! month
      DO i=1,mmax
       IF (clcount(j,i) > 0.) THEN
!        IF (i < 28 .or. i > 33) clbuf(j,i) = clbuf(j,i)/clcount(j,i)
        IF (i < 28 .or. i == 34 .or. i == 35) clbuf(j,i) = clbuf(j,i)/clcount(j,i)
       ELSE
        clbuf(j,i) = -999.5
       ENDIF
      ENDDO
      IF (clcount(j,mmax) > 0.) THEN
        clcloud(j) = clcloud(j) / clcount(j,mmax)
      ENDIF
      IF (clmth(j) > 0) THEN
        cpdd(j) = cpdd(j)/clmth(j)
        ctsumpdd(j) = ctsumpdd(j) /clmth(j)
        cnrmeltdys(j) = cnrmeltdys(j) /clmth(j)
      ENDIF
      maxclcount = maxmcount*clmth(j)
      IF (j == 2) maxclcount = maxclcount + ((ilyear - ibyear + 1)/4.)*maxdcount
      clerror(j) = (maxclcount - clerror(j)) / maxclcount !ycount(mmax)
     ENDDO 
    ENDIF

    IF ((imth == 12) .or. (j-1 == 12)) THEN
    ! prepare to write some parameters to output at end of run
      IF (dt0sumcount > 0) THEN
        dt0sum = dt0sum/dt0sumcount
        dt02sum = sqrt(dt02sum/dt0sumcount)
        pearsonR = (dt0sumcount*t0t0obssum - t0sum*t0obssum)/&
&                  (sqrt(dt0sumcount*t02sum-t0sum*t0sum)*sqrt(dt0sumcount*t0obs2sum-t0obssum*t0obssum))
        
        IF (dt0sumcount > 1) THEN
          DO i = 1,ilasttot
            IF (dt0buf(i) > errorval) dt0stdsum = dt0stdsum + (dt0buf(i)-dt0sum)*(dt0buf(i)-dt0sum)
          ENDDO
          dt0stdsum = sqrt(dt0stdsum/(dt0sumcount-1))
        ELSE
          dt0stdsum = errorval - 90.
        ENDIF
      ENDIF
      IF (luseacc > 0 .and. dsmbsumcount > 0) THEN
        daccsum = daccsum/dsmbsumcount
        dacc2sum = sqrt(dacc2sum/dsmbsumcount)
        dsmbsum = dsmbsum/dsmbsumcount
        dsmb2sum = sqrt(dsmb2sum/dsmbsumcount)
        dmelsum = dmelsum/dsmbsumcount
        dmel2sum = sqrt(dmel2sum/dsmbsumcount)
      ENDIF
    ENDIF

  ENDIF	! lclwrite

  IF ( lyrwrite > 0) THEN		!write annual averages
    DO i=1,mmax
! add up to make total period average for output
     IF (ycount(i) > fracmax*maxycount) THEN
       IF (i < 36) THEN
         perbuf(i) = perbuf(i) + ybuf(i)
       ELSE
         perbuf(i) = ybuf(i)
       ENDIF
       percount(i) = percount(i) + ycount(i)
! make annual average for output
!       IF (i < 28 .or. i > 33) ybuf(i) = ybuf(i)/ycount(i)
       IF (i < 28 .or. i == 34 .or. i == 35) ybuf(i) = ybuf(i)/ycount(i)
     ELSE
       IF (i >= 28 .and. i <= 35) THEN
         perbuf(i) = perbuf(i) + ybuf(i)
         percount(i) = percount(i) + ycount(i)
       ELSE IF (i > 35) THEN
         perbuf(i) = ybuf(i)
         percount(i) = percount(i) + ycount(i)         
       ENDIF
!       IF (i < 28 .or. i > 33) ybuf(i) = -999.5
       IF (i < 28 .or. i == 34 .or. i == 35) ybuf(i) = -999.5
     ENDIF
! make period average for output
     IF (lyrwrite == 2) THEN
       IF (percount(i) > 0) THEN
!         IF (i < 28 .or. i > 33) perbuf(i) = perbuf(i)/percount(i)
         IF (i < 28 .or. i == 34 .or. i == 35) perbuf(i) = perbuf(i)/percount(i)
       ELSE
         perbuf(i) = -999.5
      ENDIF
     ENDIF
    ENDDO
    IF (ycount(mmax) > 0.) THEN
      ycloud = ycloud / ycount(mmax)
    ENDIF
    yerror = (maxycount - yerror) / maxycount !ycount(mmax)

    IF (stmeltout > 0 .and. ndmeltout > 0) THEN
      lengthseas = ndmeltout - stmeltout
      IF (mbsumdy > mbwindy) lengthseas = lengthseas + 365		!SH
      IF (lengthseas > 365) lengthseas = lengthseas - 365
    ENDIF

  ENDIF	!lyrwrite

ENDIF !lmwrite

END SUBROUTINE PREPAREOUTPUT

!===============================================================================
SUBROUTINE OUTAWS(&
!Input
& icount, jcount )
!===============================================================================
! Writes hourly output data to file including fluxes
!===============================================================================
USE GLOBALS_EBM , ONLY : buf, sbuf, t0, q0, t2m, q2m, ws10m, &
&                 chdtg, cloud , albedo , errorflag, awsid , &
&                 Snet, Lnet, SH, LE, source, restsource, GH , &
&                 t0obs , dt0 , tinv , tpot , Lout , Loutobs , Rnet , SpenOSin, &
&                 Ch,Cq,ustar,thstar,qstar,psim,psih,psiq, zt, zm,&
&                 psim0, psih0, psiq0, z0h,z0q,z0m, Hice, dHice, paramerror, zenith
USE RADPEN_EBM , ONLY : sumdivs
USE CONSTANTS_EBM , ONLY : emis
USE FILES , ONLY : uo2

IMPLICIT NONE

!Input
INTEGER :: icount, jcount
! local
INTEGER :: jj
! Fuction
REAL	:: relhum

jj = jcount
IF (jj < 0) jj = 0

!OUTPUT
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 

WRITE(uo2,22) chdtg(icount),buf(icount,1),buf(icount,2)+jj/(3600.*24.),&
&            sbuf(4),t2m,tpot,t0,t0obs,dt0,tinv,1000.*sbuf(10),1000.*q2m,1000.*q0,&
&            100.*relhum(t2m,q2m,sbuf(5)), sbuf(6),ws10m,sbuf(5)*0.01,cloud,&
&            sbuf(9),albedo,zenith(icount),&
&            restsource,source,Snet,sbuf(7),sbuf(8),sumdivs,SpenOSin,Lnet,emis*sbuf(11),&
&            Lout,Loutobs,Rnet,SH,LE,GH,&
&            Ch,Cq,ustar,thstar,1000*qstar,psim0-psim,psih0-psih,psiq0-psiq,zt,zm,z0m,z0h,z0q,Hice,dHice,&
&            errorflag(icount),awsid(icount),paramerror(icount)

22 FORMAT(a20,f7.1,48f12.5, ES15.3E3, 2f12.5, i12)

END SUBROUTINE OUTAWS

!===============================================================================
SUBROUTINE OUTACC(&
!Input
& icount, jcount )
!===============================================================================
! Writes hourly output data to file including fluxes
!===============================================================================
USE GLOBALS_EBM , ONLY : chdtg , buf , sbuf, errorflag , &
&                 icemeltcorrmod , icemeltcorrobs , &
&                 snowcorrobs , paramerror
USE SNOW_EBM , ONLY : hsnowmod, dsnowacc , icemeltout , hmass , totwater , topwater, topsnow, topmass, &
&                     precipsum , sumdrift , cumdrift , cumdriftobs , sumsurfmelt, summelt , &
&                     runoff, surfmelt, melt , subl , slushdepth , surfwater , hsnowout , precipobs
USE CONSTANTS_EBM , ONLY :  errorval
USE FILES , ONLY : uo3

IMPLICIT NONE

!Input
INTEGER :: icount, jcount
! local
INTEGER :: jj,ip, errobs

jj = jcount
IF (jj < 0) jj = 0

!OUTPUT
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 10=running mean albedo, 
! 11=qi, 12=Lin, 13=Lout, 14=z0m, 15=zt, 16=zm , 17=serie(running mean), 18=precip,  
! 19=accum, 20=melt , 21=drift  (determined from 17)

ip = 9
errobs = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))

WRITE(uo3,22) chdtg(icount),&
&			hsnowout,precipobs,cumdriftobs,(sbuf(18)-snowcorrobs),sbuf(19)-icemeltcorrobs,sbuf(19),&
!&			hsnowout,precipsum,cumdriftobs,(sbuf(18)-snowcorrobs),sbuf(19)-icemeltcorrobs,sbuf(19),&
!&			hsnowout,precipsum,sumdrift,(sbuf(18)-snowcorrobs),sbuf(19)-icemeltcorrobs,sbuf(19),&
&			hsnowmod,precipsum,cumdrift,dsnowacc,icemeltout-icemeltcorrmod,icemeltout,hmass,totwater,topwater, topsnow, topmass, summelt, &
&           melt,sumsurfmelt,surfmelt,runoff,subl,slushdepth,surfwater, &
&           errorflag(icount),errobs

22 FORMAT(a20,26f12.5,i4)

END SUBROUTINE OUTACC

!========================================================================
SUBROUTINE OUTAVER(&
!Input
& iyr , icount )
!========================================================================
! writes daily averages to file
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast , chdtg , buf , dbuf , dcount , dcloud , dalbedo , &
&                 dalbedo_obs , derror , ldwrite , lmwrite , lswrite , lclwrite , lyrwrite , &
&                 mbuf , mcount , mcloud , merror , clbuf , clcloud , clerror , clmth , &
&                 ssbuf , scount , scloud , serror ,ybuf , ycount , ycloud , yerror , perbuf , &
&                 dt0sum, dt0sumcount, dt02sum , daccsum, dacc2sum , dmelsum, dmel2sum ,&
&                 dsmbsum, dsmbsumcount, dsmb2sum , pearsonR , dt0stdsum
USE SNOW_EBM , ONLY : mpdd , mtsumpdd , mnrmeltdys , cpdd , ctsumpdd , cnrmeltdys , &
&              spdd , stsumpdd , snrmeltdys , ypdd , ytsumpdd , ynrmeltdys , &
&              summelt, sumrunoff , sumacc , mbout , stmeltout , ndmeltout , lengthseas 
USE INPUT_EBM , ONLY : ilyear , tstep , luseacc , mbsumdy , mbwindy , lcomment
USE CONSTANTS_EBM , ONLY : mmax , errorval
USE FILES , ONLY : uo1 , uo7 , uo8 , uo9 , uo10 , uo11

IMPLICIT NONE

!Input
INTEGER :: iyr,icount
!Local
INTEGER :: i,j
INTEGER :: iyear,imth,iseas
INTEGER :: dumyr
 CHARACTER :: chmth*10
 CHARACTER :: chseas(4)*3
 
DATA chseas/'DJF','MAM','JJA','SON'/

 chmth = chdtg(icount)(1:7)//'-15'
READ(chdtg(icount)(1:4),'(i4)') iyear
READ(chdtg(icount)(6:7),'(i2)') imth

IF (imth == 12 .or. imth == 1 .or. imth == 2) iseas = 1
IF (imth == 3 .or. imth == 4 .or. imth == 5) iseas = 2
IF (imth == 6 .or. imth == 7 .or. imth == 8) iseas = 3
IF (imth == 9 .or. imth == 10 .or. imth == 11) iseas = 4

IF (ldwrite == 1) THEN
  ! write daily averages
  WRITE(uo10,27) chdtg(icount)(1:10),&
&               (dbuf(i),i=1,14),dcloud,(dbuf(i),i=15,27),dalbedo_obs,dalbedo,&
&               (dbuf(i),i=28,mmax),derror
ENDIF !ldwrite

IF (lmwrite == 1) THEN
  ! write monthly averages
  WRITE(uo7,25) chdtg(icount)(1:7),(mbuf(i),i=1,14),mcloud,(mbuf(i),i=15,mmax),mpdd,mtsumpdd,mnrmeltdys,merror

! only possible when it is end of month
  IF ( lswrite == 1 ) THEN
  ! write seasonal averages
    IF (imth == 12) iyear = iyear + 1
    WRITE(uo11,26) chseas(iseas),iyear,iyear+(iseas-1.)/4.,(ssbuf(i),i=1,14),scloud,(ssbuf(i),i=15,mmax),&
&                  spdd,stsumpdd,snrmeltdys,serror
  ENDIF
  
! only possible when it is end of month
  IF ( lclwrite == 1 ) THEN
    ! write period averages of the montly averages
    WRITE(uo8,28) imth,(clbuf(imth,i),i=1,14),clcloud(imth),(clbuf(imth,i),i=15,mmax-8),&
&                 cpdd(imth),ctsumpdd(imth),cnrmeltdys(imth),clerror(imth),clmth(imth)

! in case last month is not december calculate averages for months up to 12 any way
    j = 0
    IF ((iyr == ilyear).and.(icount == ilast).and.(imth < 12)) THEN
     DO j=imth+1,12
        WRITE(uo8,28) j,(clbuf(j,i),i=1,14),clcloud(j),(clbuf(j,i),i=15,mmax-8),&
&                  cpdd(j),ctsumpdd(j),cnrmeltdys(j),clerror(j),clmth(j)
     ENDDO 
    ENDIF

! write some parameters to output at end of run
    IF ((imth == 12).or.(j-1 == 12)) THEN
      IF (lcomment == 1) THEN
        WRITE(*,*)
        WRITE(*,*) summelt,' melt sum in mm w.e. (snow + ice/firn)'
        WRITE(*,*) sumrunoff,' runoff sum in mm w.e. (snow + ice/firn)'
        WRITE(*,*) sumacc,' accumulation sum in mm w.e. (snow)'
        WRITE(*,*) MAX(sumrunoff-sumacc,0.),' ice/firn melt (mm w.e.) (excluding fresh snow fall of this year).'
        WRITE(*,*)
        WRITE(*,'(f14.5,a)') pearsonR,' pearsonR correlation coefficient T0 mod vs obs'
        WRITE(*,'(f14.5,a)') dt0stdsum,' standard deviation T0 diff mod - obs'
        WRITE(*,'(f14.5,a,f14.5,a,f8.0)') dt0sum,' average T0  bias ',dt02sum,' average T0  RMSD (mod-obs) ',dt0sumcount
        IF (luseacc > 0) THEN
          WRITE(*,'(f14.5,a,f14.5,a,f8.0)') dsmbsum,' average smb bias ',dsmb2sum,' average smb RMSD (mod-obs) ',dsmbsumcount
          WRITE(*,'(f14.5,a,f14.5,a,f8.0)') daccsum,' average acc bias ',dacc2sum,' average acc RMSD (mod-obs) ',dsmbsumcount
          WRITE(*,'(f14.5,a,f14.5,a,f8.0)') dmelsum,' average mel bias ',dmel2sum,' average mel RMSD (mod-obs) ',dsmbsumcount
        ENDIF
        WRITE(*,'(/,a,i6,/)') 'advised time step below: ',tstep
      ENDIF
      WRITE(uo1,*)
      WRITE(uo1,*) summelt,' melt sum in mm w.e. (snow + ice/firn)'
      WRITE(uo1,*) sumrunoff,' runoff sum in mm w.e. (snow + ice/firn)'      
      WRITE(uo1,*) sumacc,' accumulation sum in mm w.e. (snow)'
      WRITE(uo1,*) MAX(sumrunoff-sumacc,0.),' ice/firn melt (mm w.e.) (excluding fresh snow fall of this year).'
      WRITE(uo1,*)
      WRITE(uo1,'(f14.5,a)') pearsonR,' pearsonR correlation coefficient T0 mod vs obs'
      WRITE(uo1,'(f14.5,a)') dt0stdsum,' standard deviation T0 diff mod - obs'
      WRITE(uo1,'(f14.5,a,f14.5,a,f8.0)') dt0sum,' average T0  bias ',dt02sum,' average T0  RMSD (mod-obs) ',dt0sumcount
      IF (luseacc > 0) THEN
        WRITE(uo1,'(f14.5,a,f14.5,a,f8.0)') dsmbsum,' average smb bias ',dsmb2sum,' average smb RMSD (mod-obs) ',dsmbsumcount
        WRITE(uo1,'(f14.5,a,f14.5,a,f8.0)') daccsum,' average acc bias ',dacc2sum,' average acc RMSD (mod-obs) ',dsmbsumcount
        WRITE(uo1,'(f14.5,a,f14.5,a,f8.0)') dmelsum,' average mel bias ',dmel2sum,' average mel RMSD (mod-obs) ',dsmbsumcount
      ENDIF
      WRITE(uo1,'(/,a,i6,/)') 'advised time step below: ',tstep

    ENDIF

  ENDIF !lclwrite

  IF ( lyrwrite > 0 ) THEN		!write annual averages
    WRITE(uo9,29) iyr,(ybuf(i),i=1,14),ycloud,(ybuf(i),i=15,mmax),stmeltout,ndmeltout,lengthseas,&
&                 mbout(1,4),mbout(1,5),mbout(1,6),&
&                 mbout(1,1),mbout(1,2),mbout(1,3),&
&                 mbout(2,1),mbout(2,2),mbout(2,3),&
&                 mbout(2,4),mbout(2,5),mbout(2,6),&
&                 ypdd,ytsumpdd,ynrmeltdys,yerror

    dumyr = iyr + 50
    lengthseas = mbwindy - mbsumdy
    IF (mbsumdy > mbwindy) lengthseas = lengthseas + 365		!SH
    IF (lyrwrite == 2) WRITE(uo9,29) dumyr,(perbuf(i),i=1,14),ycloud,(perbuf(i),i=15,mmax),&
&                    mbsumdy,mbwindy,lengthseas,&
&                    mbout(3,4),mbout(3,5),mbout(3,6),&
&                    mbout(3,1),mbout(3,2),mbout(3,3),&
&                    mbout(4,1),mbout(4,2),mbout(4,3),&
&                    mbout(4,4),mbout(4,5),mbout(4,6),&
!&                    errorval-90.,errorval-90.,errorval-90.,&
!&                    errorval-90.,errorval-90.,errorval-90.,&
!&                    errorval-90.,errorval-90.,summelt,&
&                    errorval-90.,errorval-90.,errorval-90.,errorval-90.
    IF (lcomment == 1) THEN
      WRITE(*,'(/,a,a,a,f5.0,a,f5.0)') '  Date ',chdtg(icount)(1:10),'  Start summer ',mbsumdy,'  Start winter',mbwindy
      WRITE(*,'(i6,a,f8.3,a,f8.3)') iyr,' Start day melt: ',stmeltout,' End day melt: ',ndmeltout
      IF (luseacc > 0) WRITE(*,'(2i6,a,f10.4,a,f10.4,a,f10.4)') iyr-1,iyr,' Observed: Winter balance (m w.e.): ',mbout(1,1),&
&           ' Summer balance (m w.e.): ',mbout(1,2),' Annual balance (m w.e.): ',mbout(1,3)
      WRITE(*,'(2i6,a,f10.4,a,f10.4,a,f10.4)') iyr-1,iyr,' Modelled: Winter balance (m w.e.): ',mbout(2,1),&
&           ' Summer balance (m w.e.): ',mbout(2,2),' Annual balance (m w.e.): ',mbout(2,3)
      WRITE(*,'(2i6,a,f10.4,a,f10.4,a,f10.4,/)') iyr-1,iyr,' Modelled: Winter melt (m w.e.):    ',mbout(2,4),&
&           ' Summer melt (m w.e.):    ',mbout(2,5),' Annual melt (m w.e.):    ',mbout(2,6)
    ENDIF
    WRITE(uo1,'(/,a,a,a,f5.0,a,f5.0)') '  Date ',chdtg(icount)(1:10),'  Start summer ',mbsumdy,'  Start winter',mbwindy
    WRITE(uo1,'(i6,a,f8.3,a,f8.3)') iyr,' Start day melt: ',stmeltout,' End day melt: ',ndmeltout
    IF (luseacc > 0) WRITE(uo1,'(2i6,a,f10.4,a,f10.4,a,f10.4)') iyr-1,iyr,' Observed: Winter balance (m w.e.): ',mbout(1,1),&
&             ' Summer balance (m w.e.): ',mbout(1,2),' Annual balance (m .we.): ',mbout(1,3)
    WRITE(uo1,'(2i6,a,f10.4,a,f10.4,a,f10.4)') iyr-1,iyr,' Modelled: Winter balance (m w.e.): ',mbout(2,1),&
&           ' Summer balance (m w.e.): ',mbout(2,2),' Annual balance (m w.e.): ',mbout(2,3)
    WRITE(uo1,'(2i6,a,f10.4,a,f10.4,a,f10.4,/)') iyr-1,iyr,' Modelled: Winter melt (m w.e.):    ',mbout(2,4),&
&           ' Summer melt (m w.e.):    ',mbout(2,5),' Annual melt (m w.e.):    ',mbout(2,6)
  ENDIF	!ywrite

ENDIF !lmwrite

25 FORMAT(a8,42f12.5)	! day 1 less than month
26 FORMAT(a6,1i6,43f12.5)
27 FORMAT(a11,41f12.5)	! day 1 less than month
28 FORMAT(i4,34f12.5,i4)
29 FORMAT(i5,57f12.5)

END SUBROUTINE OUTAVER

!===============================================================================
SUBROUTINE OUTSNOW(&
!Input
& icount, jcount )
!===============================================================================
! Writes vertical profiles of snow properties to file, time step variable
!===============================================================================
USE GLOBALS_EBM , ONLY : ilast , ilasttot ,buf , errorflag, maxdays, chdtg
USE SNOW_EBM , ONLY : nl, temp, dens, cpice, z, dz, energy, &
&                     irrwater, water, ice, mass , lid , hsnowmod , hsnow, grainsize
USE RADPEN_EBM , ONLY : dsdz
USE FILES , ONLY : uo4

IMPLICIT NONE

!Input
INTEGER :: icount ,jcount
! local
REAL :: year
REAL :: tmp , hsnowout
REAL :: errorfrac
INTEGER :: il,plus

tmp = 0.

plus = ilasttot - ilast

IF (icount == 1) hsnow = hsnowmod
hsnowout = hsnow

errorfrac = errorflag(icount)

IF (jcount >= 0) THEN
DO il = 1, nl-1
  WRITE(uo4,24) chdtg(icount),buf(icount,1)+buf(icount,2)/(maxdays+1.),il,z(il),dz(il),&
&               temp(il),dens(il),mass(il),water(il),ice(il),&
!&              dsdz(il),&
&              ABS(dsdz(il)*0.5*(dz(il)+dz(il+1))),&
&              cpice(il),irrwater(il),energy(il),grainsize(il),lid(il),hsnowmod,hsnowout,errorfrac
ENDDO
il = nl
WRITE(uo4,24) chdtg(icount),buf(icount,1)+buf(icount,2)/(maxdays+1.),il,z(il),dz(il),&
&              temp(il),dens(il),mass(il),water(il),ice(il),&
!&              dsdz(il),&
&              tmp,&
&              cpice(il),irrwater(il),energy(il),grainsize(il),lid(il),hsnowmod,hsnowout,errorfrac
ELSE
 DO il = 1, nl
  WRITE(uo4,34) chdtg(icount),buf(icount,1)+buf(icount,2)/(maxdays+1.),il,z(il),dz(il),&
&              '-999.00','-999.00','-999.00','-999.00','-999.00',&
&              '-999.00',&
&              '-999.00','-999.00','-999.00','-999.00',lid(il),'-999.00',hsnowout,errorfrac
 ENDDO
ENDIF

24 FORMAT(a20,f12.6,i4,10f12.5,f15.3,es10.3,i4,2f12.5,f8.4)
34 FORMAT(a20,f12.6,i4,2f12.5,10a12,i4,a12,f12.5,f8.4)

END SUBROUTINE OUTSNOW

!===============================================================================
SUBROUTINE OUTSNOWLAYER1(&
!Input
& icount, jcount )
!===============================================================================
! Writes hourly values of snow properties of first snow layer to file
!===============================================================================
USE GLOBALS_EBM , ONLY : buf, chdtg,  t0
USE SNOW_EBM , ONLY : temp, dens, cpice, dz, energy, kice, &
&                     irrwater, water, ice, mass, lid, grainsize
USE RADPEN_EBM , ONLY : dsdz
USE FILES , ONLY : uo5

IMPLICIT NONE

!Input
INTEGER :: icount,jcount

IF (jcount >= 0) THEN
WRITE(uo5,25) chdtg(icount),&
&            buf(icount,1),buf(icount,2)+jcount/(3600.*24.),&
&            dz(1),temp(1),t0,dens(1),mass(1),water(1),ice(1),&
!&              dsdz(1),&
&              ABS(dsdz(1)*0.5*(dz(1)+dz(2))),&
&              cpice(1),kice(1),irrwater(1),energy(1),grainsize(1),lid(1)
ELSE
WRITE(uo5,35) chdtg(icount),&
&            buf(icount,1),buf(icount,2)+jcount/(3600.*24.),&
&            dz(1),'-995.00','-995.00','-995.00','-995.00','-995.00','-995.00',&
&              '-995.00',&
&              '-995.00','-995.00','-995.00','-995.00','-995.00',lid(1)
ENDIF

25 FORMAT(a20,f7.1,12f12.5,f15.3,es10.3,i3)
35 FORMAT(a20,f7.1,2f12.5,12a12,i3)

END SUBROUTINE OUTSNOWLAYER1

!===============================================================================
SUBROUTINE OUTSNOWTEMP(&
!Input
& icount, jcount )
!===============================================================================
! Writes hourly output of snow temperature to file, corrected for changing depth 
! to aid comparison with weather station
! 5, 10, 20, 40, 80 cm usual initial depth sensors
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 
!===============================================================================
USE GLOBALS_EBM , ONLY : chdtg , buf , t0
USE SNOW_EBM , ONLY : nl, temp , z , dz , dsnowh, hsnow
USE INPUT_EBM , ONLY : dsnow , depthin , odepthin, chstation
USE CONSTANTS_EBM , ONLY : Tkel
USE FILES , ONLY : uo6

IMPLICIT NONE

!Input
INTEGER :: icount,jcount
!local
INTEGER :: i,il,ik
INTEGER :: itmp,snowdate
REAL :: ddepth,grad
REAL :: dodepth,ograd
REAL , dimension(5) :: depth,tempout		! based on model snow acc
REAL , dimension(5) :: odepth,otempout		! based on observed snow acc

REAL, SAVE :: ddepthdiff = 0, dodepthdiff = 0
INTEGER, SAVE :: reset = 0

IF(chstation.eq."ant_aws04") THEN
 IF(chdtg(icount).eq.'2000-01-02'//char(9)//'22:00:00' .and. reset .eq. 0) THEN
  ddepthdiff = dsnowh - dsnow
  dodepthdiff = buf(icount,16)-dsnow
  
  ddepth = dsnowh - dsnow - ddepthdiff
  dodepth = buf(icount,16) - dsnow - dodepthdiff
  
  reset = 1
 ELSEIF (chdtg(icount).eq.'2001-12-28'//char(9)//'04:00:00' .and. reset .eq. 1) THEN 
  ddepthdiff = dsnowh - dsnow
  dodepthdiff = buf(icount,16)-dsnow
  
  ddepth = dsnowh - dsnow - ddepthdiff
  dodepth = buf(icount,16) - dsnow - dodepthdiff
  
  reset = 2
 ELSEIF(reset .ne. 0) THEN
  ddepth = dsnowh - dsnow - ddepthdiff
  dodepth = buf(icount,16) - dsnow - dodepthdiff
 ELSE
  ddepth = dsnowh - dsnow 
  dodepth = buf(icount,16) - dsnow
 ENDIF
ELSE
 ddepth = dsnowh !+ hsnow ! - dsnow 
 dodepth = hsnow ! buf(icount,16) !+ hsnow ! - dsnow 
ENDIF

DO i=1,5
 depth(i) = depthin(i) + ddepth
 odepth(i) = odepthin(i) + dodepth
 
 !go to 100 !MvT test to keep height info when sensor sticks out of the snow
 IF (depth(i) < 0.) THEN
   depth(i) = 0.
   depthin(i) = 0.
 ENDIF
 IF (odepth(i) < 0.) THEN
   odepth(i) = 0.
   odepthin(i) = 0.
 ENDIF
!100 continue

 il = 1
 DO WHILE ((il.lt.nl).and.(z(il)+0.5*dz(il+1).lt.depth(i)))
  il = il+1
 ENDDO
 ik = 1
 DO WHILE ((ik.lt.nl).and.(z(ik)+0.5*dz(ik+1).lt.odepth(i)))
  ik = ik+1
 ENDDO

 IF (depth(i) > 0.) THEN
  IF (depth(i).gt.0.5*dz(il)) THEN
   grad = (temp(il+1)-temp(il))/(0.5*(dz(il)+dz(il+1)))
!  z(il)+0.5*dz(il+1) - (z(il) - 0.5*dz(il))
   tempout(i) = grad*(depth(i)-(z(il)-0.5*dz(il))) + temp(il)
  ELSE
   grad = (temp(il)-t0)/(0.5*dz(il))
   tempout(i) = grad*depth(i)+ t0
  ENDIF
 ELSE
  tempout(i) = t0
 ENDIF

 IF (odepth(i) > 0.) THEN
  IF (odepth(i).gt.0.5*dz(ik)) THEN
   ograd = (temp(ik+1)-temp(ik))/(0.5*(dz(ik)+dz(ik+1)))
!  z(il)+0.5*dz(il+1) - (z(il) - 0.5*dz(il))
   otempout(i) = ograd*(odepth(i)-(z(ik)-0.5*dz(ik))) + temp(ik)
  ELSE
   ograd = (temp(ik)-t0)/(0.5*dz(ik))
   otempout(i) = ograd*odepth(i)+ t0
  ENDIF
 ELSE
  otempout(i) = t0
 ENDIF


ENDDO

IF (jcount >= 0) THEN
WRITE(uo6,26) chdtg(icount),&
&            buf(icount,1),buf(icount,2)+jcount/(3600.*24.),&
&            ((otempout(i)-Tkel),i=1,5),(odepth(i),i=1,5),&
&            ((tempout(i)-Tkel),i=1,5),(depth(i),i=1,5)
ELSE
WRITE(uo6,36) chdtg(icount),&
&            buf(icount,1),buf(icount,2)+jcount/(3600.*24.),&
&            '-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00',&
&            '-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00','-995.00'
ENDIF

26 FORMAT(a20,f7.1,21f12.5)
36 FORMAT(a20,f7.1,1f12.5,20a12)

END SUBROUTINE OUTSNOWTEMP

!===============================================================================
SUBROUTINE OUTRANDOM
!===============================================================================
! Writes hourly output data to file including fluxes
!===============================================================================
USE GLOBALS_EBM, ONLY:	dt0sum, dt02sum, dt0stdsum, pearsonR, daccsum, dacc2sum, dmelsum , dmel2sum
USE SNOW_EBM, ONLY:		summelt, cnrmeltdys
USE INPUT_EBM, ONLY:	lz0m , z0msn, z0mice , rhosnprec , albmin
USE FILES, ONLY:		uo2

IMPLICIT NONE

IF (lz0m == 2) THEN
  WRITE(uo2,123) lz0m , z0msn, rhosnprec, summelt, cnrmeltdys, dt0sum, dt02sum, dt0stdsum, pearsonR, daccsum, dacc2sum 
ELSE IF (lz0m == 3) THEN
  WRITE(uo2,123) lz0m , z0mice, albmin, summelt, cnrmeltdys, dt0sum, dt02sum, dt0stdsum, pearsonR, dmelsum, dmel2sum 
ENDIF

123 FORMAT(i3,ES10.3,2F12.4,12F6.2,6F10.4)

END SUBROUTINE OUTRANDOM

!===============================================================================
SUBROUTINE RESETARRAYS
!===============================================================================
! Reset arrays containing daily, monthly and yearly averages
! Also reset some snowmodel arrays
!===============================================================================
USE GLOBALS_EBM, ONLY:		dbuf, dcount, dcloud, derror, dalbedo, mbuf, mcount, mcloud, merror, &
&							ssbuf, scount, scloud, serror, ycount, ybuf, ycloud, yerror, &
&                           ldwrite, lmwrite, lswrite , lyrwrite, Hice, lreset_z0
USE SNOW_EBM, ONLY:			mpdd, mtsumpdd, mnrmeltdys, ypdd, ytsumpdd, ynrmeltdys, &
&							spdd, stsumpdd, snrmeltdys, &
&                           mbout, stmeltout, ndmeltout, lengthseas, precipsum, melt, &
&							surfmelt, runoff, subl, sumdrift, surfwater
USE CONSTANTS_EBM, ONLY:	errorval
USE INPUT_EBM , ONLY : Hmax
 IMPLICIT NONE
 
 precipsum = 0.
 melt = 0.
 surfmelt = 0.
 runoff=0.
 subl=0.
 sumdrift = 0.


 IF (surfwater <= errorval) surfwater = 0.

 IF(ldwrite.eq.1) THEN
  dbuf = 0.
  dcount = 0
  dcloud = 0.
  derror = 0.
  dalbedo = 0.
 ENDIF
 
 IF(lmwrite.eq.1) THEN
  mbuf = 0.
  mcount = 0
  mcloud = 0.
  merror = 0.
  mpdd = 0.
  mtsumpdd = 0.
  mnrmeltdys = 0.

  IF(lswrite.eq.1) THEN
   ssbuf = 0.
   scount = 0
   scloud = 0.
   serror = 0.
   spdd = 0.
   stsumpdd = 0.
   snrmeltdys = 0.
  ENDIF
  
  IF(lyrwrite.gt.0) THEN
   ycount = 0
   ybuf = 0.
   ycloud = 0.
   yerror = 0.
   ypdd = 0.
   ytsumpdd = 0.
   ynrmeltdys = 0.
   mbout = -999.
   stmeltout = -999.
   ndmeltout = -999.
   lengthseas = -999.
  ENDIF

   IF(lreset_z0.gt.0) THEN
    Hice = Hmax
  ENDIF
 ENDIF
 
END SUBROUTINE RESETARRAYS
!===============================================================================