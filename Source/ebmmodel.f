!===============================================================================
! This program uses data from automatic weather stations to
! calculate the surface energy balance.
! Snet + Lnet + H + LE + G = Melt
! The calculations are done in the clasical way
! 1. T0 determined from linear interpolation of Tsn1 and Tsn2
!    or skin layer formulation
!    or T0 determined from measured Lout.
! 2. Snet determined from measured Sin and albedo or estimated albedo.
! 3. Lnet determined from determined T0 (Lout) and measured Lin or Lin
!    calculated from Tair using a parameterisation.
! 4. Calculate H and LE using T0, e0(T0), measured WS and either measured 
!    RH or estimated RH.
! 5. Calculate new temperature profile using energy input at the surface
!    of Snet + Lnet + H + LE
! 6. This way balance is allways achieved because the sum of 
!    Snet + Lnet + H + LE are used for G and Melt. 
!===============================================================================

!===============================================================================
! Module with all the model parameters
!===============================================================================
MODULE GLOBALS_EBM

  IMPLICIT NONE
  
  INTEGER :: ilast,ilasttot					! number of input samples
  INTEGER :: nstep							! number of subtimesteps in between samples
  INTEGER :: ldwrite,lmwrite,lclwrite,lyrwrite,lswrite	! logicals to write average output to file
  INTEGER :: lreset_z0 ! logical to reset Hice=Hmax in case lz0m = 4
  INTEGER :: clmth(12)						! counter for number of months in climatology
  INTEGER , ALLOCATABLE :: clcount(:,:)		! counter for monthly climate averages
  INTEGER , ALLOCATABLE :: dcount(:)		! counter for daily averages
  INTEGER , ALLOCATABLE :: mcount(:)		! counter for monthly averages
  INTEGER , ALLOCATABLE :: scount(:)		! counter for seasonal averages
  INTEGER , ALLOCATABLE :: ycount(:)		! counter for annual averages
  INTEGER , ALLOCATABLE :: percount(:)		! counter for period averages
  INTEGER , ALLOCATABLE :: dhour (:)		! time difference samples
  INTEGER , ALLOCATABLE :: paramerror(:)	! error flag different parameters in sample
  REAL , ALLOCATABLE :: errorflag(:)		! flag that indicates wrong input data
  REAL , ALLOCATABLE :: ibuf (:,:)			! input values
  REAL , ALLOCATABLE :: buf (:,:)			! corrected input values
  REAL , ALLOCATABLE :: sbuf (:)			! interpolated input values
  REAL , ALLOCATABLE :: dbuf (:)			! daily means of some variables
  REAL , ALLOCATABLE :: mbuf (:)			! monthly means of some variables
  REAL , ALLOCATABLE :: ssbuf (:)			! seasonal means of some variables
  REAL , ALLOCATABLE :: clbuf (:,:)			! climatology of monthly means of some variables  
  REAL , ALLOCATABLE :: ybuf (:)			! yearly means of some variables  
  REAL , ALLOCATABLE :: perbuf (:)			! multi year means of some variables  
  REAL , ALLOCATABLE :: zenith(:),avzenith(:)		! zenith angle from input info time
  REAL , ALLOCATABLE :: buflast(:)			! last sample of year to be used if necessary as first sample next year
  REAL , ALLOCATABLE :: awsid(:)			! id number indicating logger/argos data and availability of key parameters (T, Sin/Sout, WS, Lin)
  REAL , ALLOCATABLE :: dt0buf(:)			! hourly dt0 for calculation of std of dt0
  CHARACTER , ALLOCATABLE :: chdtg(:)*19	! date time group
  REAL :: mcloud,ycloud,clcloud(12),dcloud,scloud
  REAL :: merror,yerror,clerror(12),derror,serror
  REAL :: t0, q0, densair, maxdays
  REAL :: t2m, q2m, ws10m, radfresh, dens_closure
  REAL :: t0obs,dt0,tinv,tpot,Lout,Loutobs,Rnet,SpenOSin
  REAL :: szenith, cloud
  REAL :: Snet, Lnet, SH, LE, GH, source, restsource, Ch, Cq, zt, zm
  REAL :: ustar,qstar,thstar,psim,psiq,psih,psim0,psih0,psiq0,z0q,z0h,z0m, Hice, dHice, Hmax_month
  REAL :: alb_old,albedo,snowdays,dalbedo,dalbedo_obs
  REAL :: valerrorgap
  REAL :: dt0sum, dt0sumcount, dt02sum, dt0stdsum
  REAL :: daccsum, dacc2sum , dmelsum, dmel2sum
  REAL :: dsmbsum, dsmbsumcount, dsmb2sum
  REAL :: t0sum, t0obssum, t0t0obssum, t02sum, t0obs2sum , pearsonR
  REAL :: icemeltcorrmod,icemeltcorrobs,cumicemeltobs,snowcorrobs,accobs


END MODULE GLOBALS_EBM

!===============================================================================
! Module with all the snow model parameters
!===============================================================================
MODULE SNOW_EBM

  IMPLICIT NONE
  
  INTEGER :: nl, nlinit, nlsnow				! total number of layers, number of snow layers
  INTEGER :: vink							! switch to turn on redefining grid when layers become smaller than 0.5*dz0
  INTEGER :: lidmax							! max value of layer id for firn (indicative for number of years in snowpack)
  INTEGER , ALLOCATABLE :: lid(:)			! layer id 0 = ice (for glacier purposes), 1 = snow 2 = firn + older firn
  REAL , ALLOCATABLE :: dz(:)				! thicknes layers
  REAL , ALLOCATABLE :: z(:)				! depth layers
  REAL , ALLOCATABLE :: temp(:)				! temperature layer,
  REAL , ALLOCATABLE :: dtdz(:)				! temperature gradient
  REAL , ALLOCATABLE :: dens(:)				! density layer
  REAL , ALLOCATABLE :: mass(:)				! mass layer (dens * dz)
  REAL , ALLOCATABLE :: irrwater(:)			! irreducible water content layer
  REAL , ALLOCATABLE :: water(:)			! water content layer
  REAL , ALLOCATABLE :: ice(:)				! refrozen water content layer
  REAL , ALLOCATABLE :: energy(:)			! energy potential layer
  REAL , ALLOCATABLE :: cpice(:)			! Volumetric heat capacity of ice
  REAL , ALLOCATABLE :: rhocp(:)			! Volumetric heat capacity times density
  REAL , ALLOCATABLE :: kice(:)				! Effective heat conductivity of snow
  REAL , ALLOCATABLE :: freshfrac(:)		! Fresh snow fraction layer
  REAL , ALLOCATABLE :: refrfrac(:)			! Refrozen snow fraction layer
  REAL , ALLOCATABLE :: grainsize(:)		! Snow grain size
  REAL :: rhosn								! density of fresh snow
  REAL :: hsnow	 , hsnowstart				! snow depth = depth model
  REAL :: dsnowh							! Thickness snow layer, thickness ice melt in m ice , 
  REAL :: dsnowacc,icemelt,icemeltout, icemeltmin, icemeltmdt		! Thickness snow layer, thickness ice melt in m ice , 
  REAL :: drift, sumdrift , cumdrift , cumdriftobs
  REAL :: precip , precipsum
  REAL :: acclevel , meltlevel
  REAL :: acc , racc_old
  REAL :: dsnowr,dsnowice
  REAL :: runoff,surfmelt,melt,subl,surfwater,slushdepth
  REAL :: sumrunoff,sumsurfmelt,summelt,sumacc
  REAL :: hsnowmod,hmass , hsnowout , precipobs, totwater, topwater, topsnow, topmass
  REAL :: corrsnow , hsnowmod_l, melt_l
  REAL :: startmelt, endmelt, stmeltout , ndmeltout , lengthseas
  REAL :: winterbal, summerbal, annualbal
  REAL :: winterbalmsn, summerbalmsn, annualbalmsn
  REAL :: winterbalmod, summerbalmod, annualbalmod
  REAL :: wintermelt, summermelt, annualmelt
  REAL :: mbout(4,6)						! output array for mass balance i=1=obs, i=2=mod, j=1=winter, j=2=summer, j=3=annual, 4, 5, 6 are melt
  REAL :: ypdd,ytsumpdd,ynrmeltdys			! year positive degree days, sum temp positive degree days, nr of melt days
  REAL :: cpdd(12),ctsumpdd(12),cnrmeltdys(12)				! climate month positive degree days, sum temp positive degree days, nr of melt days
  REAL :: mpdd,mtsumpdd,mnrmeltdys				! month positive degree days, sum temp positive degree days, nr of melt days
  REAL :: spdd,stsumpdd,snrmeltdys				! Season positive degree days, sum temp positive degree days, nr of melt days
  REAL :: climserie,climacclevel,climmeltlevel,climracc_old 
  
END MODULE SNOW_EBM

!===============================================================================
! Module with all the parameters for the radiation penetration
!===============================================================================
MODULE RADPEN_EBM

  IMPLICIT NONE

  INTEGER , PARAMETER :: bandmax=118		!amount of lines to make up solar spectrum
  INTEGER :: nlradmax 						! INT(zradmax/dzrad)
  REAL,DIMENSION(bandmax) :: SolarPlateauClear, SolarPlateauCloudy, SolarSeaClear
  REAL,DIMENSION(bandmax) :: lambda, dlambda
  REAL,DIMENSION(bandmax) :: asymsn, qextsn, cosinglescatsn
  REAL,DIMENSION(bandmax) :: asymice, qextice, cosinglescatice
  REAL,DIMENSION(7,bandmax) :: lambdaAll, dlambdaAll, asymAll, qextAll, cosinglescatAll
  REAL , ALLOCATABLE :: dsdz(:)	, zrad(:)	! Radiation penetration
  REAL :: sumdivs							! Total of shortwave radiation penetrated in the snow
  
END MODULE RADPEN_EBM

!===============================================================================
! Module with all the input parameters to the model
!===============================================================================
MODULE INPUT_EBM

  IMPLICIT NONE
  
  INTEGER :: lcomment, lmc, lhourlysnowout
  INTEGER :: ibyear, ilyear, ibday, ilday
  INTEGER :: tstep
  INTEGER :: tpcond, tpdens,tpirre
  INTEGER :: tcalc, extrapolation, penetration
  INTEGER :: lwcloud, lalbedo, solzenyes, SSAfresh
  INTEGER :: lclimtemp,lclimprec,lclimrad,lclimws
  INTEGER :: lslush
  INTEGER :: trhoprec, ltempprec
  INTEGER :: lerrorgap , luseacc
  INTEGER :: lsnet, lz0m, lz0h, lrefr
  REAL :: dz0, dzdeep, zdeep, dzrad, zradmax
  REAL :: densice,densfirn,densclosure 					! density of ice, firn and pore closure (g/kg=kg/m3)
  REAL :: rhosninit,rhosnprec
  REAL :: T10m
  REAL :: z0msn,z0mice, zll, zul, Hmax, Hmax_month
  REAL :: dsnow,dfirn
  REAL :: accyear,mbsumdy,mbwindy
  REAL :: albmin,albmax, radrefr, soot
  REAL :: radiussn, radiusice
  REAL :: lwmax(4),lwmin(3),depthin(5),odepthin(5)
  REAL :: climtemp,climprec,climrad,climws
  REAL :: cirre
  REAL :: tausteep,tau1,tauhor,surfangle,slfact
  REAL :: albsnow,albice,albfirn
  REAL :: tstarwet,tstardry0,tstardry10, snowstar
  CHARACTER :: chstation*9

END MODULE INPUT_EBM

!===============================================================================
! Module with all the constants used in the model
!===============================================================================
MODULE CONSTANTS_EBM

  IMPLICIT NONE
  
  INTEGER , PARAMETER :: rowmax=100000		! max amount of lines, 1 year of half hourly data
  INTEGER , PARAMETER :: colmax=20			! max amount of input parameters  = 9 + year + jday + hour + some extra for accumulation and running means
  INTEGER , PARAMETER :: nlmax=3200			! max number of layers in the snow
  INTEGER , PARAMETER :: mmax = 37			! max number of daily, montly and annual parameters into output
  REAL , PARAMETER :: secday = 86400		! seconds in 1 day
  REAL , PARAMETER :: Tkel = 273.15			! 0 degC in degK
  REAL , PARAMETER :: rd = 287.05			! gas constant of dry air (J/Kkg)
  REAL , PARAMETER :: rv = 461.51			! gas constant of moist air (J/Kkg)
  REAL , PARAMETER :: rstar = 8.314			! universal gas constant (JK-1mol-1)
  REAL , PARAMETER :: eps=0.622				! Rd/Rv
  REAL , PARAMETER :: lm = 3.34e5			! latent heat of melting (Jkg-1)
  REAL , PARAMETER :: ls = 2.834e6			! latent heat of sublimation Jkg-1
  REAL , PARAMETER :: lv = 2.501e6			! latent heat of vaporisation Jkg-1
  REAL , PARAMETER :: beta = 2317.			! constant for calculation es J K-1 kg-1
  REAL , PARAMETER :: es0 = 610.78			! water vapour pressure at melting point Pa (Triple point pressure)
  REAL , PARAMETER :: densnow = 150.		! density of snow, lower limit density snow layer (g/kg=kg/m3)
  REAL , PARAMETER :: denswater = 1000.		! density of water (g/kg=kg/m3)
  REAL , PARAMETER :: betadens = 	1		! 3 constant for calculation densification of snow (tuning), used now as minimum value
  REAL , PARAMETER :: tempcut = 272.5		! cut off temperature for gravitational densification
  REAL , PARAMETER :: Deff = 1.1e-4			! Vapor diffusion coefficient (m2s-1)
  REAL , PARAMETER :: StefBoltz = 5.67e-8	! Stephan Boltzman constant
  REAL :: emis 								! Emissivity of the snow/ice surface set in input file
  REAL , PARAMETER :: karman = 0.4			! Von Karman constant
  REAL , PARAMETER :: g = 9.81				! gravity accelaration
  REAL , PARAMETER :: cp = 1005.			! specific heat air at constant pressure J K-1 kg-1
  REAL :: pi
  REAL :: loutmax							! 315 W/m2 to limit the outgoing longwave radiation to melting level
  REAL , PARAMETER :: Tinterv = 5.			! search interval for t0 (K)
  REAL , PARAMETER :: Taccur = 0.005		! accuracy of final t0 (K)
  REAL , PARAMETER :: Lmocrit = 0.01		! accuracy of final MO-length (relative)
  REAL , PARAMETER :: accur = 1.e-10		! accuracy in general
  REAL , PARAMETER :: errorval = -908.0		! errorvalue set when no EB is calculated
  REAL , PARAMETER :: maxerror = 32
  REAL , PARAMETER :: snowthreshold = 0.001 ! Threshold for cumulated snow to be added as snowfall event
END MODULE CONSTANTS_EBM

!===============================================================================
!Module with necessary parameters for new albedo scheme
!===============================================================================
MODULE PKM_EBM
	IMPLICIT NONE
	
	REAL	:: TVals(11), DTDZVals(31), DENSVals(8), &
	& TAUMAT(11,31,8), KAPMAT(11,31,8), DR0MAT(11,31,8)
	REAL,ALLOCATABLE	:: drdry(:)
END MODULE PKM_EBM

!===============================================================================
! Module with all the model parameters related to input and output files
!===============================================================================
MODULE FILES
  INTEGER , PARAMETER :: ui = 10		! unit of input file
  INTEGER , PARAMETER :: uo1 = 11		! unit of output file 1		run info file
  INTEGER , PARAMETER :: uo2 = 12		! unit of output file 2		AWS / Hourly values
  INTEGER , PARAMETER :: uo3 = 13		! unit of output file 3		Mass balance components
  INTEGER , PARAMETER :: uo4 = 14		! unit of output file 4		Snow/firn characteristics
  INTEGER , PARAMETER :: uo5 = 15		! unit of output file 5		First layer snow/firn characteristics
  INTEGER , PARAMETER :: uo6 = 16		! unit of output file 6		Snow temperature in aid of comp with observations
  INTEGER , PARAMETER :: uo7 = 17		! unit of output file 7		Monthly averages
  INTEGER , PARAMETER :: uo8 = 18		! unit of output file 8		Annual averages
  INTEGER , PARAMETER :: uo9 = 19		! unit of output file 9		Monthly climatology
  INTEGER , PARAMETER :: uo10 = 20		! unit of output file 10	Daily averages
  INTEGER , PARAMETER :: uo11 = 21		! unit of output file 10	Seaonal averages
END MODULE FILES

!===============================================================================
! Start main program
!===============================================================================
PROGRAM EBM

USE GLOBALS_EBM , ONLY : ilast, nstep , buf, sbuf, dhour, cloud, albedo , &
&                        t0, q0, Snet, Lnet, SH, LE, source, restsource , & 
&                        errorflag , valerrorgap , ldwrite , lmwrite, maxdays, Hice, z0m, dHice
USE SNOW_EBM , ONLY : z, dz , temp, nl, nlinit, precip, drift, melt, dsnowacc, icemeltmdt
USE INPUT_EBM , ONLY : ibyear, ilyear, tstep, tcalc, lcomment, penetration, lwcloud ,&
&                      chstation, lalbedo, lerrorgap , lsnet , luseacc, lmc, lz0m, Hmax, lhourlysnowout
USE CONSTANTS_EBM , ONLY :  Tkel, StefBoltz, emis, nlmax
USE RADPEN_EBM , ONLY : sumdivs, dsdz
USE FILES , ONLY : uo1

IMPLICIT NONE

!Loop variables
INTEGER :: ii, jj, kk
!Variables related to time interpolation and output
INTEGER :: iyr
REAL :: nst
REAL :: hfrac,offs
!Other necessary variables
REAL :: surft, spechum, energybalance,cloudcover

 CALL SET_RANDOM_SEED !Initialize random number generator

 CALL INFO

 CALL SETCONSTANTS

 CALL SETTOZERO
 
 IF(lalbedo .eq. 4 .or. lalbedo .eq. 5) CALL INITTABLES

 CALL OPENOUTPUT

 IF (penetration .ne. 0) CALL INPUTRADPEN

DO 999 iyr=ibyear,ilyear	!START YEAR LOOP

IF (lcomment == 1) WRITE(*,'(/,A)') '-----------------------------------------------'
WRITE(uo1,'(/,A)') '-----------------------------------------------'
IF (lcomment == 1) WRITE(*,'(A,I6/)') 'START',iyr
WRITE(uo1,'(A,I6/)') 'START',iyr

 CALL INPDATA(iyr)

 CALL CHECKDATA(iyr)
 
 IF(MOD(iyr,4).eq.0.and.((.not.MOD(iyr,100).eq.0).or.MOD(iyr,400).eq.0)) THEN
  maxdays = 366
 ELSE
  maxdays = 365
 ENDIF

! write initial snow/ice profile to output
 IF (iyr == ibyear) THEN
   CALL INITGRID
   CALL INITSNOW
   IF(lalbedo.eq.4 .or. lalbedo.eq.5) CALL INITGRAINS
   IF(lz0m.ne.2 .and. lz0m.ne.3) CALL OUTSNOW(1,0)
 ENDIF

 DO 888 ii = 1, ilast		!START LOOP OVER ALL SAMPLES
  IF ((lcomment == 1).and.(1.*INT(ii*1./INT(ilast/10.)).eq.1.*ii/INT(ilast/10.))) THEN
    WRITE(*,'(I6,2i7,A,2i7)') ii,ilast,NINT(100.*ii/ilast),' % done of year',(iyr-ibyear+1),(ilyear-ibyear+1)
    WRITE(uo1,'(I6,2i7,A,2i7)') ii,ilast,NINT(100.*ii/ilast),' % done of year',(iyr-ibyear+1),(ilyear-ibyear+1)
  ENDIF
  
  hfrac = dhour(ii)/(24.*3600.)
  IF ((chstation.eq."SURE_2007").or.(chstation.eq."LF_0710")) THEN
    offs = ((buf(1,3)-dhour(ii)/3600.) - 12.25)*3600./dhour(ii)
  ELSE
    offs = ((buf(1,3)-dhour(ii)/3600.) - 12.)*3600./dhour(ii)
  ENDIF
  
  nstep = NINT( dhour(ii)*1.0/tstep*1.0 )

  valerrorgap = 100
  IF (lerrorgap == 0 .or. (errorflag(ii) > 29 .and. errorflag(ii+1) > 29)) valerrorgap = 8
  IF (lerrorgap >= 1) valerrorgap = 100
  
  IF (errorflag(ii) < valerrorgap .and. errorflag(ii+1) < valerrorgap) THEN	
! only when enough valid observations are present in case valerrorgap = 8, 
! very large values of valerrorgap and it will use the in input set values usually based on interpolation
   
   DO 777 jj = 0, dhour(ii)-tstep, tstep			!START LOOP OVER (0.5,1,2)-HOURS in steps of tstep
! determine mass balance      
    
    CALL MBYEAR(ii,jj,iyr)

    CALL INTERP_DATA(jj,ii,nstep,nst) ! interpolate to obtain values on higher time resolution than input data
    


   !Formulation calculation surface temperature
    IF (tcalc.eq.1) THEN	! from Lwout observations as start or definite value	
     t0 = (sbuf(12)/(emis*StefBoltz))**(0.25)	
    ELSE IF (tcalc.eq.2) THEN		! chosen equal to temperature of upper most snow/ice layer
     t0 = temp(1)
    ELSE IF (tcalc.eq.3.or.tcalc.eq.4) THEN	! extrapolated from upper 1-3 snow/ice layers, in case of 4: start value for iterations
     t0 = surft(temp(1),temp(2),dz(1),dz(2),z(1),z(2))	! extrapolation from upper most layers snow/ice
    ENDIF


    CALL SET_PRECIP						! convert precipitation to m snow, calculate/set fresh snow density
    IF (luseacc >= 2) CALL SET_HEIGHT	! in case snowheight is restricted to accumulation observations, correct for data gaps
 	IF ((precip /= 0.) .or. (drift /= 0.)) CALL SNOWHEIGHT	! add snowfall layer or remove layers
	
    cloud = cloudcover(sbuf(4),sbuf(11))		! only necessary in aid of radiation penetration
    											! and for calculating albedo in new parameterization scheme (lalbedo==4)
    											! but calculated always for extra information 4 = temp, 11 = Lin
	
	IF(lalbedo.eq.0) THEN
	 IF (lsnet.eq.0) THEN
	  Snet = sbuf(7)-sbuf(8)			! Snet based on observed Sin and Sout
	 ELSE IF (lsnet.eq.1) THEN
	  Snet = sbuf(8)*((1./sbuf(9))-1.)	! Snet based on Sout and albedo, albedo is preferably the 24 hours running mean
										! that removes daily cycle due to albedo!!!
										! But also removes some of the error induced by tilt of the sensor.
	  Sbuf(7) = sbuf(8)/sbuf(9)			! necessary to keep output consistent with what is used for the calculations
  ELSE IF (lsnet.eq.2) THEN
    Snet = sbuf(7)*(1-sbuf(9))     ! Snet based on Sin and albedo, albedo is preferably the 24 hours running mean that removes daily cycle due to albedo!!! But also removes some of the error induced by tilt of the sensor.
    sbuf(8)= sbuf(7)*sbuf(9)     ! necessary to keep output consistent with what is used for the calculations
	 ENDIF
	ELSE
	 IF(lalbedo .eq. 4 .or. lalbedo .eq. 5) THEN
	  CALL NEWALBEDO
	 ELSE
	  CALL CALCALBEDO
	 ENDIF
	 IF(lalbedo .eq. 5) THEN 
	  IF(lsnet .eq. 0) THEN 
	   Snet = sbuf(7) - sbuf(8)
	  ELSE
	   Snet = sbuf(8)*((1./sbuf(9))-1.)
	   sbuf(7) = sbuf(8)/sbuf(9)
	  ENDIF
	 ELSE
	  Snet = sbuf(8)*((1.0/albedo)-1.0)	! Snet based on Sout and parameterised albedo
	  sbuf(7) = Snet + sbuf(8)			! necessary to keep output consistent with what is used for the calculations
	 ENDIF
	ENDIF
	
	IF (penetration.eq.1 .and. sbuf(7).gt.0.0) THEN
	  CALL RadiationPenetration
    ELSE
      dsdz = 0.
      sumdivs = 0.
    ENDIF

    IF (t0.ge.Tkel) t0 = Tkel
    q0 = spechum(t0, 1.0, sbuf(5))
	
    ! Compute roughness length for momentum
    IF (lz0m == 4) CALL Z0M_MODEL(dsnowacc, icemeltmdt, Hmax, Hice, z0m, dHice)

   !Formulation energy balance from skin layer temperature
    IF (tcalc.eq.4) THEN	!Skin layer formulation for surface temperature
     CALL TSKIN
     source=energybalance(t0)
    ELSE
    ! formulation energy balance from t0 previous time step, excess heat put into first layer
    ! Calculate turbulent fluxes
     CALL TURBHF(-1)

     source=energybalance(t0)
     IF (t0 >= Tkel) THEN
      source=energybalance(Tkel)			
      IF (source.lt.0.) source=0.
     ENDIF
    
    ENDIF

  ! check difference from energy balance (should be =0 in case of no melt else > 0)
    restsource = energybalance(t0) !- source
     
  ! Calculate net longwave radiation from model results
    Lnet = (sbuf(11) - emis*StefBoltz*(t0)**4)
	
  ! Calculate new englacial profiles
    CALL ENTEMP
   
! Write hourly output to file
!    IF (jj.eq.0) THEN
    IF (jj.eq.dhour(ii)-tstep) THEN
     CALL PREPAREOUTPUT(iyr,ii,jj)	! prepare arrays for output, determine averages based on (2) hourly data
     IF(lmc.eq.1) THEN
      CALL OUTACC(ii,jj)
     ELSEIF(lz0m.ne.2 .and. lz0m.ne.3) THEN
      CALL OUTACC(ii,jj)			! output on input time resolution of MB components
      CALL OUTAWS(ii,jj)			! output on input time resolution
      IF (ldwrite == 1 .or. lmwrite == 1) CALL OUTAVER(iyr,ii)			! write daily/monthly/yearly average values to file when appropriate 
      CALL OUTSNOWTEMP(ii,jj)	! output snow temperature in aid of comparison to observations
      CALL OUTSNOWLAYER1(ii,jj)	! output characteristics first snow layer 
      IF(lhourlysnowout .eq. 1) THEN
       CALL OUTSNOW(ii,jj)	! output snow characteristics profiles
      ELSEIF ((ii+offs)*hfrac.eq.NINT((ii+offs)*hfrac)) THEN
       CALL OUTSNOW(ii,jj)	! output snow characteristics profiles
      ENDIF
     ENDIF
     CALL RESETARRAYS
    ENDIF

777 CONTINUE				! END (2)-HOUR LOOP

   ELSE
!	in case of large data gaps write error values to files
     jj = -1
     CALL MBYEAR(ii,0,iyr)
     CALL INTERP_DATA(jj,ii,nstep,nst)
     CALL SETTOERROR(ii)
     CALL PREPAREOUTPUT(iyr,ii,jj)	! ?????prepare arrays for output, determine averages based on (2) hourly data 
     IF(lmc.eq.1) THEN
      CALL OUTACC(ii,jj)
     ELSEIF(lz0m.ne.2 .and. lz0m.ne.3) THEN
      CALL OUTACC(ii,jj)			! output on input time resolution of MB components
      CALL OUTAWS(ii,jj)			! output on input time resolution
      IF (ldwrite == 1 .or. lmwrite == 1) CALL OUTAVER(iyr,ii)			! write daily/monthly/yearly average values to file when appropriate 
      CALL OUTSNOWTEMP(ii,jj)	! output snow temperature in aid of comparison to observations
      CALL OUTSNOWLAYER1(ii,jj)	! output characteristics first snow layer 
      IF(lhourlysnowout .eq. 1) THEN
       CALL OUTSNOW(ii,jj)	! output snow characteristics profiles
      ELSEIF ((ii+offs)*hfrac.eq.NINT((ii+offs)*hfrac)) THEN
       CALL OUTSNOW(ii,jj)	! output snow characteristics profiles
      ENDIF
     ENDIF
     CALL RESETARRAYS
   ENDIF ! if enough valid observations are present 

888 CONTINUE				! END LOOP OVER ALL SAMPLES

999 CONTINUE				! END YEAR LOOP

IF (lz0m == 2 .or. lz0m == 3) CALL OUTRANDOM

 CALL FREEARRAYS

WRITE(uo1,'(/,A,/)') 'THE END'

 CALL CLOSEOUTPUT

IF (lcomment == 1) WRITE(*,'(/,A,/)') 'THE END'

END

!===============================================================================
