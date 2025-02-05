!===============================================================================
!
! File with general usefull routines and functions
!
!===============================================================================
FUNCTION julday (year,month,day)
!===============================================================================
! Function which calculates the Julian day (or day number) based on input 
! year, month and day
!===============================================================================
IMPLICIT NONE
INTEGER :: julday
INTEGER :: year,month,day
INTEGER :: dmth(12)
INTEGER :: jday,k
INTEGER :: lyr
DATA dmth/31,28,31,30,31,30,31,31,30,31,30,31/

!Determine whether it is a leap year
lyr=0
IF (INT(year/4.).eq.year/4.) lyr=1
IF (INT(year/100.).eq.year/100.) lyr=0
IF (INT(year/400.).eq.year/400.) lyr=1

IF (lyr == 0) dmth(2)=28
IF (lyr == 1) dmth(2)=29

jday=day
DO k = 1,month-1
 jday = jday + dmth(k)
ENDDO
1 CONTINUE
julday = jday

END FUNCTION julday

!===============================================================================
FUNCTION spechum(t,rh,p)
!===============================================================================
! Function that calculates the specific humidity based on given 
! temperature relative humidity and air pressure
!===============================================================================
USE CONSTANTS_EBM , ONLY : rd , rv , eps , Tkel
IMPLICIT NONE
INTEGER :: k
REAL :: t, rh, p
REAL :: esat, es
REAL :: q, spechum

! saturation vapor pressure
! k=0 with respect to water, k=1 with respect to ice
IF (t.lt.Tkel) k=3 ! MvT CAUTION,, can be changed to 3
IF (t.ge.Tkel) k=2 ! MvT CAUTION,, can be changed to 2
esat=es(t,k)
! specific humidity
q=(eps*esat*rh) / (p-(1.-eps)*esat)

spechum = q

END FUNCTION spechum

!===============================================================================
FUNCTION relhum(t,q,p)
!===============================================================================
! Function that calculates the specific humidity based on given 
! temperature relative humidity and air pressure
!===============================================================================
USE CONSTANTS_EBM , ONLY : rd , rv , eps , Tkel
IMPLICIT NONE
INTEGER :: k

!Input
REAL,INTENT(IN) :: t, q, p

REAL :: esat, es
REAL :: rh, relhum

! saturation vapor pressure
! k=0 with respect to water, k=1 with respect to ice
IF (t.lt.Tkel) k=3 ! MvT CAUTION,, can be changed to 3
IF (t.ge.Tkel) k=2  ! MvT CAUTION,, can be changed to 2
esat=es(t,k)
! relative humidity
rh = (q*(p-(1.-eps)*esat))/(eps*esat)

relhum = rh

END FUNCTION relhum

!===============================================================================
FUNCTION es(t,k)
!===============================================================================
! Function that calculates the vapor pressure of water over ice or 
! a water surface given temperature
!===============================================================================
USE CONSTANTS_EBM , ONLY : es0, rv, lv, ls, beta , Tkel
IMPLICIT NONE
INTEGER :: k
REAL :: t, es
REAL :: fact1, fact2a, fact2b, fact3, fact4

fact1 = 1./rv
fact2a = (lv + beta * Tkel)
fact2b = (ls + beta * Tkel)
fact3 = ( (1./Tkel) - (1./t) )
fact4 = beta * log(t/Tkel)

IF (k.eq.0) THEN		! with respect to water (lv)
 es = es0 * exp(fact1 * ((fact2a*fact3)-fact4) )
ELSEIF (k.eq.1) THEN		! with respect to ice (ls)
 es = es0 * exp(fact1 * ((fact2b*fact3)-fact4) )
ELSEIF (k.eq.3) THEN		! with respect to ice, Magnus formula 
 es = es0 * exp(22.46 * (t-Tkel) / (t-Tkel+272.62))
ELSEIF (k.eq.2) THEN		! with respect to water, Magnus formula
 es = es0 * exp(17.62 * (t-Tkel) / (t-Tkel+243.12))
ENDIF
END FUNCTION es

!===============================================================================
FUNCTION conduc(dens,temp)
!===============================================================================
! Function that calculates the effective conductivity of snow/ice
! Density in g/kg=kg/m3
! most of these are defined between 100-700 kg/m3 density
!===============================================================================
USE INPUT_EBM , ONLY : tpcond , densice
!USE CONSTANTS_EBM , ONLY : densice
IMPLICIT NONE
REAL :: conduc, dens, temp
REAL :: Kice, theta, kreff, krefs

IF (tpcond.eq.1) THEN			!(4) Von Dussens equation T range? , 
 conduc = (0.21e-01+0.42e-03*dens+0.22e-08*dens**3)
ELSEIF (tpcond.eq.2) THEN		!(5) Sturm equation 156 <= rho <= 600
 conduc = (0.138-1.01e-3*dens+3.233e-6*dens**2)
ELSEIF (tpcond.eq.3) THEN		!(1) Douville equation
 conduc = 2.2*((dens/densice)**1.88)
ELSEIF (tpcond.eq.4) THEN		!(2) Jansson
 conduc = 0.02093+0.7953e-3*dens+2.512e-12*dens**4
ELSEIF (tpcond.eq.5) THEN		!(3) Anderson
 conduc = 0.021+2.5*(dens*0.001)**2
ELSEIF (tpcond.eq.6) THEN		!(6) Ostin&Anderson (6, lowest)
 conduc = -0.00871 + 0.439e-3 * dens + 1.05e-6 * dens*dens
ELSEIF (tpcond.eq.7) THEN		!(7) Arthern 1998
 conduc = 2.1*((dens/densice)**2.0)
ELSEIF (tpcond.eq.8) THEN
 Kice = 7.8042*exp(-0.0046507*temp)	! data from internet, fit in kaleidagraph
 conduc = ((2.*dens)/(3.*(densice-dens)))*Kice	! Cox et al., 2014 TCD
 IF (dens >= densice-0.001) conduc = Kice
ELSEIF (tpcond.eq.9) THEN		!(9) tuned value
	! conduc = 10.0
	conduc = 0.26
ELSEIF (tpcond.eq.10) THEN		!(10) Calonne et al., 2019 GRL (Eq.5 for T=Tref=-3C)
	theta =  1 / (1+exp(-2*0.02*(dens-450.)))
	kreff = 2.107 + 0.003618*(dens-917.)
	krefs = 0.024-1.23e-4*dens + 2.5e-6*dens**2
	conduc = (1 - theta)*krefs + theta*kreff
! write(*,*) conduc,temp
ENDIF

END FUNCTION conduc

!===============================================================================
FUNCTION densprec(ws,Tskin)
!===============================================================================
! Function that calculates the density of freshly fallen snow as function of
! wind speed and surface temperature output in g/kg=kg/m3
! gives value between 260 and 350
! based on Lenaerts et al. 2012, JGR
!===============================================================================
USE INPUT_EBM , ONLY : rhosnprec
USE CONSTANTS_EBM , ONLY : Tkel
IMPLICIT NONE
REAL :: ws, Tskin
REAL :: wslocal, tslocal
REAL :: densprec

wslocal = ws
IF (wslocal > 10.) wslocal = 10.
tslocal = Tskin
IF (tslocal > Tkel) tslocal = Tkel
densprec = 97.49 + 4.49 * wslocal + 0.769 * tslocal
IF (densprec < rhosnprec) densprec = rhosnprec

END FUNCTION densprec

!===============================================================================
SUBROUTINE SETTOERROR(icount)
!===============================================================================
USE INPUT_EBM , ONLY : dsnow
USE GLOBALS_EBM , ONLY : errorflag , buf , sbuf , t0, q0, t2m, q2m, ws10m, cloud , &
&                        Snet , albedo , Lnet, SH, LE, source, restsource, GH , &
&                        Ch,Cq,ustar,thstar,qstar,psim,psih,psiq,psim0, psih0, &
&                        psiq0, z0h,z0q,z0m,zt,zm , paramerror
USE SNOW_EBM , ONLY : hsnow, hsnowmod , hmass, totwater, topwater, topsnow, topmass, precipsum, &
&                     dsnowacc , icemeltout, runoff, melt , subl , slushdepth , surfwater , &
&                     corrsnow , hsnowmod_l
USE RADPEN_EBM , ONLY : sumdivs
USE CONSTANTS_EBM , ONLY : errorval , colmax 

IMPLICIT NONE

INTEGER :: icount , iv , ip
INTEGER :: idT,idS,idWS,idL,idall
REAL :: valerror
REAL :: dum, dum2

idT = 0 	! temperature = 
idS = 0 	! short wave radiation
idWS = 0	! wind speed
idL = 0		! long wave radiation
idall = 0	! no data at all

valerror = errorval - 90.

ip = 6	!temperature
dum = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
IF (dum == 9) THEN
  idT = 1
ENDIF
ip = 2	!Sin
dum = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
ip = 3	!Sout
dum2 = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
IF (NINT(dum) == 9 .or. NINT(dum2) == 9) THEN
  idS = 1
ENDIF
ip = 1	!wind speed
dum = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
IF (dum == 9) THEN
  idWS = 1
ENDIF
ip = 4	!Lin
dum = INT(paramerror(icount) /(10**(ip-1))) - 10*INT(paramerror(icount) /(10**(ip)))
IF (dum == 9) THEN
  idL = 1
ENDIF

DO iv = 4,colmax
 sbuf(iv) = buf(icount,iv)
ENDDO

IF (idT == 1) THEN
  sbuf(4) = valerror		! T
  sbuf(11) = valerror/1000.	! q
ENDIF

IF (idS == 1) THEN
  sbuf(7) = valerror		! Sin	
  sbuf(8) = valerror		! Sout
  sbuf(9) = valerror		! albedo
ENDIF

IF (idWS == 1) THEN
  sbuf(6) = valerror		! ws
ENDIF

IF (idL == 1) THEN
  sbuf(11) = valerror		! lin
ENDIF

t2m = valerror
t0 = valerror
q2m = valerror/1000.
q0 = valerror/1000.
ws10m = valerror
 cloud = valerror
albedo = valerror

sumdivs = valerror	 
Snet = valerror	
restsource = valerror
source = valerror
Lnet = valerror
SH = valerror
LE = valerror
GH = valerror

runoff = valerror
melt = valerror
subl = valerror
slushdepth = valerror
surfwater = valerror

 Ch = valerror
 Cq = valerror
ustar = valerror
thstar = valerror
qstar = valerror/1000.
psim0 = valerror
psim = valerror
psih0 = valerror
psih = valerror
psiq0 = valerror
psiq = valerror
z0m = valerror
z0h = valerror
z0q = valerror
zt = valerror
zm = valerror

! = start snow, start sbuf(16), current sbuf(16), 16 = serie mean
hsnow = sbuf(16)		!current value serie
 corrsnow = hsnow - hsnowmod_l

hsnowmod = valerror
hmass = valerror
totwater = valerror
topwater = valerror
topsnow = valerror
topmass = valerror
precipsum = valerror
dsnowacc = valerror
icemeltout = valerror

END SUBROUTINE SETTOERROR
!===============================================================================
SUBROUTINE SET_RANDOM_SEED
!===============================================================================
!Subroutine that seeds the random number generator depending on system time
!===============================================================================
	IMPLICIT NONE
	
	INTEGER				:: i, clock
	INTEGER, PARAMETER	:: n=33
	INTEGER				:: seed(n)
	
	CALL SYSTEM_CLOCK(COUNT=clock)
	seed = clock + 37 * (/ (i - 1, i = 1, n) /)
	CALL RANDOM_SEED(PUT = seed)
END SUBROUTINE SET_RANDOM_SEED
!===============================================================================
SUBROUTINE RANDHUM(q,temp,press,tempold,pressold)
!===============================================================================
!Subroutine that randomly disturbs the relative humidity measurement
!===============================================================================
	USE CONSTANTS_EBM, ONLY: Tkel
	USE INPUT_EBM, ONLY: chstation
	
	IMPLICIT NONE
	
	REAL, INTENT(IN)	:: temp, press, tempold, pressold
	REAL, INTENT(INOUT)	:: q
	!Already defined functions
	REAL	:: relhum, spechum
	!Local
	REAL	:: nudge, rh
	
	rh = relhum((tempold+Tkel),(q/1000.),(pressold*100.))
	
	CALL RANDOM_NUMBER(nudge)
	nudge = (2.*nudge)-1.
	
	IF(chstation.eq."ant_neuma") THEN
	 rh = rh*(1.+0.05*nudge) !5% error
	ELSE
	 IF(rh .gt. 0.9) THEN
	  rh = rh*(1.+0.03*nudge) !If rel hum >90%: 3% error
	 ELSE
	  rh = rh*(1.+0.02*nudge) !Else 2% error
	 ENDIF
	ENDIF
	IF(rh .gt. 1.) rh = 1.
	
	q = (spechum((temp+Tkel),rh,(press*100.)))*1000.
	
END SUBROUTINE RANDHUM
!===============================================================================
FUNCTION solvetemp(temp_old,energy,mass)
!===============================================================================
! Function that finds new layer temperature from its energy and mass
!===============================================================================
    USE CONSTANTS_EBM, ONLY: Tkel
    
    IMPLICIT NONE
    
    !Input
    REAL, INTENT(IN)    :: temp_old, energy, mass
    !Output
    REAL    :: solvetemp
    
    !Local
    REAL,PARAMETER  :: a=7.122, b=152.5
    REAL            :: c, discriminant
    REAL            :: sol1, sol2
    
    c = (energy/mass) - (152.5+7.122*Tkel)*Tkel
    
    discriminant = b**2. - (4.*a*c)
	IF(discriminant.lt.0) THEN
	 WRITE(*,*) "Can't solve second order polynomial!"
	 STOP 60
	END IF
	
	!Usually it's solution with positive sign
    sol1 = ((-b)+SQRT(discriminant))/(2.*a)
    IF(ABS(sol1-temp_old) .lt. 10.) THEN
     solvetemp = sol1
    ELSE
     sol2 = ((-b)-SQRT(discriminant))/(2.*a)
    
     !Now choose solution that is closest to previous value
     IF(ABS(sol1 - temp_old) < ABS(sol2 - temp_old)) THEN
      solvetemp = sol1
     ELSE
      solvetemp = sol2
     ENDIF
    ENDIF
END FUNCTION
!===============================================================================
FUNCTION solvequadpos(a,b,c)
!===============================================================================
! Function that solves a simple quadratic function using the abc-formula
!===============================================================================
	IMPLICIT NONE
	
	!Input
	REAL,INTENT(IN) 			:: a, b, c
	!Output
	REAL	:: solvequadpos
	
	!Local
	REAL	:: discriminant
	
	discriminant = b**2.-(4.*a*c)
	IF(discriminant.lt.0) THEN
	 WRITE(*,*) "Can't solve second order polynomial!"
	 STOP 60
	END IF
	
	solvequadpos = ((-b)+SQRT(discriminant))/(2.*a)
	
END FUNCTION solvequadpos
!===============================================================================
FUNCTION solvequadneg(a,b,c)
!===============================================================================
! Function that solves a simple quadratic function using the abc-formula
!===============================================================================
	IMPLICIT NONE
	
	!Input
	REAL,INTENT(IN) 			:: a, b, c
	!Output
	REAL	:: solvequadneg
	
	!Local
	REAL	:: discriminant
	
	discriminant = b**2.-(4.*a*c)
	IF(discriminant.lt.0) THEN
	 WRITE(*,*) "Can't solve second order polynomial!"
	 STOP 61
	END IF
	
	solvequadneg = ((-b)-SQRT(discriminant))/(2*a)
	
END FUNCTION solvequadneg

!===============================================================================
FUNCTION tempprec(t,q,p)
!===============================================================================
! Function that calculates the temperature of freshly fallen snow or rain as function of
! near surface air temperature, specific humidity and air pressure based on the psychrometric energy balance
! from Harder P, Pomeroy J (2013) Hydrol. Process https://doi.org/10.1002/hyp.9799
!===============================================================================
USE CONSTANTS_EBM , ONLY : Tkel, rd, ls, lv
IMPLICIT NONE
REAL,INTENT(IN) :: t, q, p
REAL :: D, lambdat, lambdaf, esat
REAL :: densair, ll, qsat
REAL :: tempprec
REAL :: spechum
IF(T>Tkel) THEN
 ll = lv ! Evaporation
ELSE
 ll = ls ! Sublimation 
ENDIF

densair = p / (rd*t)
D = 2.06e-5 * (t/Tkel)**(1.75)
lambdat = 6.3e-5*t+6.73e-3

qsat = spechum(t,1.0,p)
tempprec = t + (D/lambdat)*ll*(densair*(q-qsat))

END FUNCTION tempprec