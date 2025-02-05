!===============================================================================
!
! All routines related to the calculation of the turbuent heat fluxes
!
!===============================================================================
FUNCTION energybalance(t0)
!===============================================================================
! This function calculates the energy balance as a function of the surface temperature (K)
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 
! 10=qi, 11=Lin, 12=Lout, 13=z0m, 14=zt, 15=zm , 16=serie(running mean), 17=precip,  
! 18=accum, 19=melt , 20=drift 

USE GLOBALS_EBM , ONLY : sbuf, Snet, Ch, Cq, densair, q0, zt, GH
USE SNOW_EBM , ONLY : kice, temp, dz
USE RADPEN_EBM , ONLY : sumdivs
USE CONSTANTS_EBM , ONLY : lv, ls , StefBoltz , emis, cp, g , Tkel
USE INPUT_EBM , ONLY : extrapolation, tcalc

IMPLICIT NONE

! input
REAL :: t0
! local
REAL :: l, term1, term2, term3, term4, term4a, term4b
REAL :: energybalance, spechum

IF (t0.lt.Tkel) l=ls
IF (t0.ge.Tkel) l=lv
 q0 = spechum(t0, 1.0, sbuf(5))

! net radiation
term1 = Snet - sumdivs + (sbuf(11)-emis*StefBoltz*(t0**4.))
! sensible heat flux
term2 = Ch*densair*cp*sbuf(6)*(sbuf(4)+(zt*g/cp)-t0)
! latent heat flux
term3 = Cq*densair*l*sbuf(6)*(sbuf(10)-q0)

! subsurface energy flux
!kice(1) = 50.0 ! ECMWF trick for glaciers and ice caps
IF (extrapolation.eq.1) THEN
  term4 = -(kice(1)*(t0-temp(1)))/dz(1)
!  term4 = -(kice(1)*(t0-temp(1)))/(0.5*dz(1))
ELSE IF (extrapolation.eq.2) THEN
  term4a = kice(1)*(t0-temp(1))/dz(1)
!  term4a = kice(1)*(t0-temp(1))/(0.5*dz(1))
  term4b = ((kice(1)*dz(1)+kice(2)*dz(2))/(dz(1)+dz(2)))*((temp(1)-temp(2))/(0.5*(dz(1)+dz(2))))
  term4 = -(dz(1)*(2.*term4a - term4b) + dz(2)*term4a)/(dz(1)+dz(2))
ENDIF

IF((tcalc == 2 .or. tcalc == 3) .and. t0 .lt. Tkel) THEN
 GH = -(term1 + term2 + term3)
ELSE
 GH = term4
ENDIF

IF (tcalc == 2.or.tcalc == 3) term4 = 0.
!IF (t0>=Tkel) term4 = 0.

energybalance = term1 + term2 + term3 + term4

END FUNCTION energybalance

!===============================================================================
SUBROUTINE TURBHF(skiter)
!===============================================================================
USE GLOBALS_EBM , ONLY : sbuf, t0, q0, SH, LE, densair, Ch, Cq, zt, &
&                        zm, t2m, q2m, ws10m , ustar,thstar,qstar, psim, psih, psiq , &
&                        psim0, psih0, psiq0, z0q,z0h,z0m, Hice
USE CONSTANTS_EBM , ONLY : lv, ls, karman, g, cp, rd, eps, Lmocrit , Tkel
USE INPUT_EBM , ONLY : lcomment, lz0m, Hmax, lz0h
!USE SNOW_EBM , ONLY : dsnowacc, melt
USE FILES , ONLY : uo1

IMPLICIT NONE
!input
INTEGER, INTENT(IN) :: skiter
!Local
INTEGER :: iter, itermax
INTEGER :: crit
INTEGER :: landreas
REAL :: t,q,ws,p,theta
REAL :: l
REAL :: Lmo,Lmo_old
REAL :: psimin,z0hmin,z0qmin
REAL :: Chn,Cqn

IF (t0.lt.Tkel) l=ls
IF (t0.ge.Tkel) l=lv

t = sbuf(4)
q = sbuf(10)
ws = sbuf(6)
p = sbuf(5)

densair = p / (rd*t)

!First step, calculate u*, th*, q* under neutral conditions
ustar = (karman * (ws - 0.0 )) / log(zm/z0m)

IF (lz0h.eq.0) THEN
  z0h = z0m*0.1
  z0q = z0m*0.1
ELSE
  CALL Z0H_MODEL(ustar, densair, t, z0h, z0q)
ENDIF

theta = t+zt*g/cp

thstar = (karman * (t - t0)) / log(zt/z0h)
qstar = (karman * (q - q0)) / log(zt/z0q)

Lmo = 10.E4
Lmo_old = 1.E4
psimin = -2. !MvT this was changed from -2
z0hmin=1.0E-10
z0qmin=1.0E-10
itermax = 40
iter = 0

psim = 0.0
psih = 0.0
psiq = 0.0
psim0 = 0.0
psih0 = 0.0
psiq0 = 0.0

IF (theta.gt.t0) crit = 1		! stable conditions
IF (theta.le.t0) crit = -1		! unstable conditions

DO; IF ((ABS((Lmo_old-Lmo)/Lmo_old).le.Lmocrit).or.(iter.ge.itermax)) exit
  iter = iter + 1
    
  ! Now add stability and iterate
  CALL STABILITYFUNC(crit,Lmo, zm, zt, zt, psim, psih, psiq)
  CALL STABILITYFUNC(crit,Lmo, z0m, z0h, z0q, psim0, psih0, psiq0)
  IF (crit.eq.1) THEN	! Limited stability correction
    IF (Lmo.eq.0.) iter = iter+itermax
    IF (psim.lt.psimin) psim=psimin		
    IF (psih.lt.psimin) psih=psimin
    IF (psiq.lt.psimin) psiq=psimin
    IF (psim0.lt.psimin) psim0=psimin		
    IF (psih0.lt.psimin) psih0=psimin
    IF (psiq0.lt.psimin) psiq0=psimin
  ENDIF

  ! Recalculate the u*, th* and q*
  ustar = (karman * (ws - 0.0 )) / ( log(zm/z0m) - psim + psim0 )
  IF (crit.eq.1.and.ustar.lt.0.) ustar = (karman * (ws - 0.0 )) / log(zm/z0m)

  IF (lz0h.eq.0) THEN
    z0h = z0m*0.1
    z0q = z0m*0.1
  ELSE
    CALL Z0H_MODEL(ustar, densair, t, z0h, z0q)
  ENDIF

  IF (z0h.lt.z0hmin) z0h=z0hmin
  IF (z0q.lt.z0qmin) z0q=z0qmin

  thstar = (karman * (theta - t0)) / ( log(zt/z0h) - psih + psih0 )
  qstar = (karman *(q - q0)) / ( log(zt/z0q) - psiq + psiq0 )

  Lmo_old = Lmo
  Lmo = (ustar**2) / ( (karman*g/t) * (thstar + t * ((1.-eps)/eps) * qstar ) )
  crit = 1	!Stable
  IF (Lmo < 0.) crit = -1	!Unstable
ENDDO

IF (iter.ge.itermax) THEN		! no solution found
  IF (lcomment == 1) THEN
!    WRITE(*,'(/,A,i3,A,/,2I5,16f16.8,/)') 'TURBHF more than',itermax,' iterations necessary',&
!&       skiter,iter,Lmo,Lmocrit,ABS((Lmo_old-Lmo)/Lmo_old),psim,psih,psim0,psih0,t,t0,ws,z0m,z0h,z0q,&
!&       ustar,thstar,qstar
    WRITE(*,'(/,A,i3,A,/,4f16.8,/)') 'TURBHF more than',itermax,' iterations necessary',&
&       Lmo,Lmocrit,ABS((Lmo_old-Lmo)/Lmo_old),crit*1.0
!  STOP
  ENDIF
  WRITE(uo1,'(/,A,i3,A,/,2I5,16f16.8,/)') 'TURBHF  more than',itermax,' iterations necessary',&
&       skiter,iter,Lmo,Lmocrit,ABS((Lmo_old-Lmo)/Lmo_old),crit*1.0!,psim,psih,psim0,psih0,t,t0,ws,z0m,z0h,z0q,&
!&       ustar,thstar,qstar
 SH = 0.
 LE = 0.
 Ch = 0.
 Cq = 0.
 Chn = 0.
 Cqn = 0.
 psim = 0.0
 psih = 0.0
 psiq = 0.0
 psim0 = 0.0
 psih0 = 0.0
 psiq0 = 0.0
ELSE IF (Lmo.eq.0.) THEN
 SH = 0.
 LE = 0.
 Ch = 0.
 Cq = 0.
 Chn = 0.
 Cqn = 0.
 psim = 0.0
 psih = 0.0
 psiq = 0.0
 psim0 = 0.0
 psih0 = 0.0
 psiq0 = 0.0
ELSE
 SH = densair*cp*thstar*ustar
 LE = densair*l*qstar*ustar
 Ch=(karman**2.)/((log(zm/z0m)-psim+psim0)*(log(zt/z0h)-psih+psih0))
 Cq=(karman**2.)/((log(zm/z0m)-psim+psim0)*(log(zt/z0q)-psiq+psiq0))
 Chn=(karman**2.)/(log(zm/z0m)*log(zt/z0h))
 Cqn=(karman**2.)/(log(zm/z0m)*log(zt/z0q))
ENDIF


t2m=t0+thstar/karman*(log(2./z0h)-psih+psih0)-2.*g/cp
q2m=q0+qstar/karman*(log(2./z0q)-psiq+psiq0)
ws10m=ustar/karman*(log(10./z0m)-psim+psim0)
IF (ws10m<0.1) ws10m=0.1		! can become negative for very low wind speeds

END SUBROUTINE TURBHF

!===============================================================================
SUBROUTINE Z0H_MODEL(&
!Input
& ustar, densair, temp, &
!Output
& z0h, z0q )
!===============================================================================
!Calculate roughness length for heat and moisture, necessary to calculate th* and q* according to Andreas
!===============================================================================
USE GLOBALS_EBM , ONLY : z0m
USE INPUT_EBM , ONLY : chstation, lz0h

IMPLICIT NONE
!Input
REAL, INTENT(IN) :: ustar, densair, temp
!Output
REAL, INTENT(OUT) :: z0h, z0q
!Local
REAL :: Restar , visc, mu
REAL :: z0_min

z0_min=1.0E-10

mu=(-0.039225 + 0.0079067 * temp - 5.1515E-6 * (temp**2)) / 1.0E5
! visc = the kinematic fluid viscosity of air (m2/s) for a given pressure (Pa) and temperature (K)
visc=mu/densair
Restar = ustar*z0m/visc

IF (Restar.le.0.135) THEN		!Smooth regime
 z0h = z0m * exp( 1.250 )
 z0q = z0m * exp( 1.610 )
ELSEIF (Restar.gt.0.135.and.Restar.lt.2.5) THEN	!Transition regime
 z0h = z0m * exp( 0.149 - 0.550*log(Restar) )
 z0q = z0m * exp( 0.351 - 0.628*log(Restar) )
ELSEIF (Restar.ge.2.5) THEN						!Rough Regime
 z0h = z0m * exp( 0.317 - 0.565*log(Restar) - 0.183*log(Restar)**2 )
 z0q = z0m * exp( 0.396 - 0.512*log(Restar) - 0.180*log(Restar)**2 )
ENDIF
IF (z0m >= 0.001 .and. lz0h.eq.2) THEN		! Extension to very rough terrain by Smeets and VdBroeke 2008.
  z0h = z0m * exp( 1.5 - 0.2*log(Restar) - 0.11*log(Restar)**2 ) 
  z0q = z0h
ELSEIF (z0m >= 0.001 .and. lz0h.eq.3) THEN ! Updated parameterization from Van Tiggelen et al (2021) 
  z0h = z0m * exp( 1.5 - 0.15*log(Restar) - 0.16*log(Restar)**2 ) 
  z0q = z0h
ENDIF

IF (z0h.lt.z0_min) z0h=z0_min
IF (z0q.lt.z0_min) z0q=z0_min

END SUBROUTINE Z0H_MODEL

!===============================================================================
SUBROUTINE STABILITYFUNC(&
!Input
& crit, Lmo, hm, hh, hq,  &
!Output
&  psim, psih, psiq)
!===============================================================================
USE CONSTANTS_EBM , ONLY : pi
IMPLICIT NONE
!Input
INTEGER :: crit
REAL :: Lmo, hm, hh, hq
!Output
REAL :: psim, psih, psiq
!Local
REAL :: fim, fih, fiq
REAL :: psim_min

psim_min = -2.  !MvT this was changed from -2

!Check the used functions!!!!
IF (crit.eq.-1) THEN			!unstable conditions Dyer 1974
! fim = (1.0 - 20.0 * hm / Lmo )**0.25		!momentum
! fih = (1.0 - 15.0 * hh / Lmo )**0.5		!temperature
! fiq = (1.0 - 15.0 * hq / Lmo )**0.5		!humidity
! Functions of Dyer 1974
 fim = (1.0 - 16.0 * hm / Lmo )**(0.25)		!momentum
 fih = (1.0 - 16.0 * hh / Lmo )**(0.5)		!temperature
 fiq = (1.0 - 16.0 * hq / Lmo )**(0.5)		!humidity
 psim = 2. * log( (1.+fim)/2. ) + log( (1.+fim**2)/2. ) - 2.0*atan(fim) + pi/2.0
 psih = 2. * log( (1.+fih)/2. )
 psiq = 2. * log( (1.+fiq)/2. )
ELSEIF (crit.eq.1) THEN		!stable conditions Holtslag en de Bruin 1988
! psim = -( 1 + 6.25* hm / Lmo )**0.8		!momentum
! psih = -( 1 + 9.375* hh / Lmo )**0.8		!temperature
! psiq = -( 1 + 9.375* hq / Lmo )**0.8		!humidity
! Functions of Holtslag and de Bruijn 1988
 psim = -( (0.7 * hm / Lmo) + 0.75 * ((hm / Lmo) - (5. / 0.35)) * exp(-0.35 * (hm / Lmo)) + 0.75 * 5. / 0.35 )!momentum
 IF (psim.lt.psim_min) psim = psim_min
 psih = psim								!temperature
 psiq = psim								!humidity
ENDIF

END SUBROUTINE STABILITYFUNC

!===============================================================================
SUBROUTINE Z0M_MODEL(&
  !Input
  & dsnowacc, ablation, Hmax,  &
  !Output
  & Hice, z0m, dHice )
  !===============================================================================
  !Calculate roughness length for momentum based on Raupach (1994) and Van Tiggelen et al. (2021) 
  !===============================================================================
  USE CONSTANTS_EBM , ONLY : karman
  USE INPUT_EBM , ONLY : tstep
  IMPLICIT NONE
  !Input
  REAL, INTENT(IN) :: dsnowacc, ablation, Hmax 
  !Output
  REAL, INTENT(OUT) :: Hice, z0m, dHice
  !Local
  REAL :: Hmin , PsiH, c1, Lo, Cs10, d, gamma
  REAL :: Ho, Hsnow, dHsub, dHabl, f, Cr, Cs
  
  ! Parameters
  Hmin = Hmax / 2
  dHsub = 1e-3 ! obstacle height reduction due to sublimation [m/day]
  dHabl = 0.1 ! fraction of ice ablation contributing to increase of ice obstacle height [-]
  f = 8 ! Obstacles per 100 m
  Cs10 = 1.2071e-3 ! 10m drag coefficient for skin friction
  c1 = 7.5 ! empirical constant 
  PsiH = 0.193 ! roughness layer correction for wind profile
  
  ! Model for obstacle height
  Hsnow = dsnowacc 

  IF (ablation.gt.0) THEN
    dHice = dHabl * ablation 
  ELSE
    dHice = - dHsub*(REAL(tstep)/(3600*24))
  ENDIF

  IF (Hsnow.gt.Hice) THEN
    Hice = Hice
    dHice = 0
  ELSE
    Hice = Hice + dHice 
  END IF

  IF (Hice.gt.Hmax) THEN
    Hice = Hmax 
    dHice = 0
  ELSEIF (Hice.lt.Hmin) THEN
    Hice = Hmin 
    dHice = 0
  ENDIF

  ! Obstacle height 
  Ho = Hice - Hsnow 
  IF (Ho.lt.0.01) Ho=0.01
  IF (Ho.gt.Hmax) Ho=Hmax

  ! Obstacle frontal area ratio
  Lo = (f/100)*Ho 

  ! drag coefficient for form drag, adapted from Garbrecht at al. (2002)
  IF (Ho.le.2.5527) THEN
    Cr = 0.5 * (0.185 + (0.147*Ho))
  ELSE
    Cr = 0.11 * log(Ho/0.2)
  ENDIF
  !IF (Hsnow.ge.0.01) Cr = Cr / 2 !not used

  ! Displacement height
  d = Ho * (1 - (1 - exp(-(c1*Lo)**0.5)) / (c1*Lo)**0.5)

  ! Drag coefficient for skin friction at z = Ho
  Cs = (Cs10**(-0.5) - (1/karman) * (log((10-d) / (Ho-d))-PsiH))**(-2)

  ! Model for u/ustar
  gamma = (Cs + Cr*Lo)**(-0.5)
  
  ! Roughness length for momentum
  z0m = (Ho-d) * exp(-karman * gamma) * exp(PsiH)

END SUBROUTINE Z0M_MODEL