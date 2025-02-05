!===============================================================================
SUBROUTINE ENTEMP
!===============================================================================
! Routine that calculates the subsurface temperature
!===============================================================================
USE GLOBALS_EBM, ONLY:		source, t0, LE
USE SNOW_EBM, ONLY:			nl, temp, dens, rhocp, cpice, kice, z, dz, energy, &
&             vink, lid, hsnow, runoff, hsnowmod, precip, dsnowh, &
&             icemelt, icemeltout, icemeltmdt, hmass, dsnowr, totwater, topwater, topsnow, topmass, &
&             icemeltmin, hsnowmod_l, dsnowacc, dtdz, drift, mass
USE RADPEN_EBM, ONLY:		dsdz
USE CONSTANTS_EBM, ONLY:	Tkel, nlmax, lm, ls, lv, denswater
USE INPUT_EBM, ONLY:		tstep, extrapolation, tcalc, lcomment, dz0, tpdens, densice
USE FILES, ONLY:			uo1

IMPLICIT NONE
!Local
REAL , PARAMETER :: small = 0.00000001
INTEGER :: il,ll
REAL :: dzl,conduc,cond
REAL :: Kdtdz(nlmax)
REAL :: stcrit, dt , stcrit1 , stcrit2
REAL :: terma,termb


! Routine to solve subsurface equation:
! rho*cp*dT/dt = d/dz(K*dT/dz)
! dT/dt = (K/rho*cp)*dT/dz2
! T(i+1) = T(i)+(d/dz(K*dT/dz))*dt/(rho*cp)
! properties of the layer are defined at half the layer thickness!!

!IF (tpdens > 0) 
 CALL RESIZEGRID		! in case of 0, constant density profile, layers remain same size, except top because of melt
IF (vink >= 1) CALL REDEFGRID			! redefine grid in case layers become thinner than 0.5*dz0
!IF ((precip /= 0.) .or. (drift /= 0.)) CALL SNOWHEIGHT		! add snowfall layer or remove layers

DO il=1,nl
 kice(il) = conduc(dens(il),temp(il))
 cpice(il) = 152.5 + 7.122 * temp(il)
 rhocp(il) = dens(il)*cpice(il)
ENDDO
!kice(1) = 50.0

 CALL IRREDUCIBLE

dtdz(1) = (temp(1)-t0)/dz(1)
Kdtdz(1) = kice(1)*dtdz(1)
DO il=2,nl
 dzl=0.5*(dz(il)+dz(il-1))
 dtdz(il) = (temp(il)-temp(il-1))/dzl
 Kdtdz(il) = (1./(2.*dzl))*(kice(il)*dz(il)+kice(il-1)*dz(il-1))*dtdz(il)
ENDDO

! dsdz = radiation penetration
IF (tcalc.eq.2.or.tcalc.eq.3) dsdz(1) = -source + dsdz(1)*dz(1)		! (interpolation upper layer, all energy entering first layer) 
DO il=1,nl-1
  IF ((tcalc.eq.2.or.tcalc.eq.3).and.il.eq.1) THEN
   temp(il) = temp(il) + (tstep/rhocp(il))*((Kdtdz(il+1) - dsdz(il)*dz(il))/dz(il))
  ELSE
   temp(il) = temp(il) + (tstep/rhocp(il))*((Kdtdz(il+1) - Kdtdz(il))/dz(il) - dsdz(il))
  ENDIF
  kice(il) = conduc(dens(il),temp(il))
  cpice(il) = 152.5 + 7.122 * temp(il)
  rhocp(il) = dens(il)*cpice(il)
  energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
ENDDO
!kice(1) = 50.0

temp(nl) = temp(nl-1)			!boundary condition, no heat flux to or from lower layers
energy(nl) = energy(nl-1)

IF (t0.lt.Tkel) source=0		!source is now only melt energy, not energy used for heating
 CALL SNOWCONTENT

! Calculate condensation / evaporation
ll = lv
IF (LE < 0.) ll = ls
IF ((LE > 0.) .and. (t0 < Tkel)) ll = ls
 cond = LE*tstep/ll				! in mm w.e.
IF ((cond > 0.) .and.(t0 >= Tkel)) cond = 0.

icemeltmdt = 0
IF (dsnowh <=0.) THEN
  icemeltmdt = (source * tstep /lm)/dens(1) !+ cond/dens(1) !in m ice per time step
  icemelt = icemelt - (source * tstep /lm)/dens(1) + cond/dens(1)		!in m ice
  IF (cond < 0) THEN
    icemelt = icemelt + cond/dens(1)
    icemeltmdt = icemeltmdt + cond/dens(1)
  ELSE
    dsnowacc = dsnowacc + cond/dens(1)  
  ENDIF
ENDIF

! icemeltout: used for output MB only, (diagnostic parameter)
icemeltout = icemelt + MAX(0.,dsnowr)
IF (icemeltout > icemeltmin) THEN
  icemeltout = icemeltmin
ELSE
  icemeltmin = icemeltout
ENDIF

hsnowmod = dsnowh + icemelt + dsnowr !+ cond/densice (dsnowr = extra correction for adding/removing layers when all snow has melted)
IF ((precip /= 0.) .or. (drift /= 0.)) dsnowacc = dsnowacc + dsnowr
hsnowmod_l = hsnowmod

! dsnowh = thickness snowpack
! icemelt = if no snowpack, all melt is ice melt and lowers surface, accumulated in this parameter
! dsnowr =  corrects for small snow layers that are added to the ice when the layer is too thin to sustain. 

! Check stability of the solution with criterium: D*dt/dx2 < 0.25
 !stcrit=(kice(1)/rhocp(1))*tstep/(dz(1)**2)
 !dt = 0.25*tstep/stcrit
 !IF (stcrit.ge.0.25) THEN
!!   IF (lcomment == 1) write(*,'(4(A,f9.4))') 'stcrit= ',stcrit,' dt = ',0.25*tstep/stcrit,'dt = ',tstep*1.,' dz = ',dz(1)
!   write(uo1,'(4(A,f9.4))') 'stcrit= ',stcrit,' dt = ',0.25*tstep/stcrit,' dt = ',tstep*1.,' dz = ',dz(1)
! ENDIF
!Method as described by WJvdB
 stcrit2=(2*kice(1)/rhocp(1))*tstep/(dz(1)**2)
 stcrit1=(kice(1)/rhocp(1))*tstep/(dz(1)**2)
 stcrit = stcrit2+(1-stcrit2)*stcrit2-stcrit2*stcrit1
 dt = 0.25*tstep/stcrit1
 IF (stcrit < 0) THEN
!!   IF (lcomment == 1) write(*,'(4(A,f9.4))') 'stcrit= ',stcrit,' dt = ',0.25*tstep/stcrit,'dt = ',tstep*1.,' dz = ',dz(1)
   write(uo1,'(4(A,f9.4))') 'stcrit= ',stcrit,' dt = ',0.25*tstep/stcrit1,' dt = ',tstep*1.,' dz = ',dz(1)
   write(*,'(4(A,f9.4))') 'stcrit= ',stcrit,' dt = ',0.25*tstep/stcrit1,' dt = ',tstep*1.,' dz = ',dz(1)
 ENDIF

END SUBROUTINE ENTEMP

!===============================================================================
SUBROUTINE SNOWHEIGHT
!===============================================================================
! Routine that calculates the changes in grid or only first grid point due to addition of layers due to snow fall
!===============================================================================
USE GLOBALS_EBM , ONLY : t0 , sbuf, radfresh
USE SNOW_EBM , ONLY : nl, nlsnow, z, dz, temp, dens, ice, water, mass, lid, precip, dsnowacc , &
&                     dsnowr, dsnowh, hmass, vink ,  rhosn , drift, precipsum, totwater, topwater, topsnow, topmass, &
&                     lidmax, freshfrac, energy, kice, dtdz, cpice, refrfrac, grainsize
USE INPUT_EBM , ONLY : dz0, tpdens, dzdeep, zdeep, ltempprec
USE CONSTANTS_EBM , ONLY : nlmax , accur, Tkel, snowthreshold

IMPLICIT NONE

!local
INTEGER :: il, ic , nlsnow_old, nlgain, nlloss, nl_old
REAL, DIMENSION(nlmax) :: temp_old, water_old, dens_old, ice_old, mass_old, z_old, dz_old, lid_old, grainsize_old
REAL, DIMENSION(nlmax) :: energy_old, kice_old, dtdz_old, cpice_old, refrfrac_old
REAL :: depth,fact,mult,dzl,conduc,freshsnow=0.0,tempprecip=0.0
REAL :: sumsnow,summass,coeff,dens_new, sumwater
REAL :: tempprec 

!It is not necessary to keep track of refrfrac when loosing/adding layers because it is calculated in SNOWCONTENT
freshsnow = freshsnow+precip+drift
tempprecip = tempprecip + precip*rhosn
IF(abs(freshsnow).gt.snowthreshold) THEN
precipsum = precipsum + tempprecip
tempprecip = 0.0

nl_old = nl
nlsnow_old = nlsnow
!nlsnow = nlsnow_old + NINT((precip+dsnowr)/dz0)	! add/remove layers in steps of dz0
nlsnow = nlsnow_old + NINT((freshsnow + dsnowr)/dz0)	! add/remove layers in steps of dz0
IF ( dz(1) + (freshsnow + dsnowr) < 0.5*dz0 .and. nlsnow == nlsnow_old ) nlsnow = nlsnow_old - 1
IF (nlsnow < 0) nlsnow = 0
nlgain = 0
nlloss = 0

IF(nlsnow.eq.nlsnow_old) THEN !Too little snow to add a new layer
 dsnowr = freshsnow+dsnowr
 mult = 1.
 IF ((lid(1) > 1).or.(lid(1) == 0)) mult = 0.0
 DO il=1,nl_old
  fact=0.
  IF (il == 1) fact=1.0
  IF (tpdens == 0) THEN
   dz(il) = dz(il)+mult*fact*dsnowr
   z(il) = z(il)+(1.-0.5*fact)*mult*dsnowr
   mass(il) = dens(il)*dz(il)
  ELSE
   dens_new = rhosn	!+MAX(0.,0.5*(dens_old(il)-2.*rhosn))
   IF (dsnowr < 0) dens_new = dens(il)
   mass(il) = mass(il)+mult*fact*dsnowr*dens_new
   dz(il) = dz(il)+mult*fact*dsnowr
   dens(il) = mass(il)/dz(il)
   z(il) = z(il)+(1.-0.5*fact)*mult*dsnowr
  ENDIF
  refrfrac(il) = refrfrac(il)*((mass_old(il)+water_old(il))/(mass(il)+water(il)))
  kice(il) = conduc(dens(il),temp(il))
  cpice(il) = 152.5 + 7.122 * temp(il)
  energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
  IF(il==1) THEN
   dtdz(il) = (temp(il)-t0)/dz(il)
  ELSE
   dzl=0.5*(dz(il)+dz(il-1))
   dtdz(il) = (temp(il)-temp(il-1))/dzl
  END IF
!  energy(il) = dens(il)*dz(il)*((152.5+7.122*Tkel)*Tkel - cpice(il)*temp(il))
 ENDDO
 !kice(1) = 50.0

 IF(dsnowr.gt.0.0) freshfrac(1) = (dsnowr*rhosn)/(mass(1)+water(1))
 IF ((tpdens > 0).and.(lid(1) > lidmax).and.(dsnowr < 0.)) dsnowr = 0.
 IF (mult == 1) dsnowr = 0.
ELSE !Save old states in _old arrays
 temp_old = temp
 water_old = water
 dens_old = dens
 mass_old = mass
 ice_old = ice
 z_old = z
 dz_old = dz
 lid_old = lid
 grainsize_old = grainsize
 energy_old = energy
 kice_old = kice
 dtdz_old = dtdz
 refrfrac_old = refrfrac
 cpice_old = cpice

 temp = 0.
 water = 0.
 dens = 0.
 mass = 0.
 ice = 0.
 z = 0.
 dz = 0.
 lid = 0
 grainsize = 0.
 energy = 0.
 kice = 0.
 dtdz = 0.
 refrfrac = 0.
 cpice_old = 0.
 IF(nlsnow.gt.nlsnow_old) THEN		!snow gain
  nlgain = nlsnow - nlsnow_old
  dsnowr = (freshsnow+dsnowr)-nlgain*dz0
  IF (ltempprec.eq.1) THEN ! compute hydrometeor temperature based on the psychrometric energy balance
    temp(1:nlgain) = tempprec(sbuf(4),sbuf(10),sbuf(5))
  ELSEIF (ltempprec.eq.2) THEN! use air temperature
    temp(1:nlgain) = sbuf(4) !
  ELSEIF (ltempprec.eq.0) THEN! use urface temperature
    temp(1:nlgain) = t0 
  END IF
  
  dens(1:nlgain) = rhosn
  water(1:nlgain) = 0.
  ice(1:nlgain) = 0.
  lid(1:nlgain) = 1
  freshfrac(1:nlgain) = 1.
  grainsize(1:nlgain) = radfresh
  refrfrac(1:nlgain) = 0.
  DO il=1,nlgain
   fact=1.
   IF (il > 1) fact=0.
   mass(il) = rhosn*(dz0+fact*dsnowr)
   dz(il) = dz0+fact*dsnowr
   z(il) = (il*1.-0.5)*dz0+(1.-0.5*fact)*dsnowr
   kice(il) = conduc(dens(il),temp(il))
   cpice(il) = 152.5 + 7.122 * temp(il)
!   energy(il) = dens(il)*cpice(il)*dz(il)*(Tkel - temp(il))
   IF(il==1) THEN
    dtdz(il) = (temp(il)-t0)/dz(il)
   ELSE
    dzl=0.5*(dz(il)+dz(il-1))
    dtdz(il) = (temp(il)-temp(il-1))/dzl
   END IF
   energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
  ENDDO
  !kice(1) =50.0

  DO il=nlgain+1,nl_old+nlgain
   temp(il) = temp_old(il-nlgain)
   dens(il) = dens_old(il-nlgain)
   energy(il) = energy_old(il-nlgain)
   kice(il) = kice_old(il-nlgain)
   dtdz(il) = dtdz_old(il-nlgain)
   cpice(il) = cpice_old(il-nlgain)
   water(il) = water_old(il-nlgain)
   mass(il) = mass_old(il-nlgain)
   ice(il) = ice_old(il-nlgain)
   dz(il) = dz_old(il-nlgain)
   z(il) = z_old(il-nlgain)+nlgain*dz0+dsnowr
   lid(il) = lid_old(il-nlgain)
   grainsize(il) = grainsize_old(il-nlgain)
   refrfrac(il) = refrfrac_old(il-nlgain)
  ENDDO
  dzl=0.5*(dz(nlgain+1)+dz(nlgain))
  dtdz(nlgain+1) = (temp_old(1)-temp(nlgain))/dzl !Recalculate temperature gradient of first layer below new layers
  nl = nl_old + nlgain
  dsnowr = 0.
 ELSE IF (nlsnow.lt.nlsnow_old) THEN		!snow loss
  nlloss = nlsnow_old - nlsnow
! dsnowr = (freshsnow+dsnowr)+nlloss*dz0
  dsnowr = (freshsnow+dsnowr)+z_old(nlloss) + 0.5*dz_old(nlloss)
  mult = 1.  !NN
  IF ((lid_old(1+nlloss) > 1).or.(lid_old(1+nlloss) == 0)) mult = 0.0 !NN
  DO il=1,nl_old-nlloss
   fact=0.  !NN
   IF (il == 1) fact=1.0 !NN
   IF (tpdens == 0) THEN !NN
    dz(il) = dz_old(il+nlloss)+mult*fact*dsnowr !NN
    z(il) = z_old(il+nlloss)+(1.-0.5*fact)*mult*dsnowr-(z_old(nlloss)+0.5*dz_old(nlloss)) !NN
    dens(il) = dens_old(il+nlloss)
    mass(il) = dens(il)*dz(il) !NN
   ELSE !NN
    dens_new = dens_old(il+nlloss)
    mass(il) = mass_old(il+nlloss)+mult*fact*dsnowr*dens_new !NN
    dz(il) = dz_old(il+nlloss)+mult*fact*dsnowr !NN
    dens(il) = mass(il)/dz(il) !NN
    z(il) = z_old(il+nlloss)+(1.-0.5*fact)*mult*dsnowr -(z_old(nlloss)+0.5*dz_old(nlloss))!NN
    
   ENDIF !NN

   temp(il) = temp_old(il+nlloss)
  ! dens(il) = dens_old(il+nlloss)
   energy(il) = energy_old(il+nlloss)
   kice(il) = kice_old(il+nlloss)
   dtdz(il) = dtdz_old(il+nlloss)
   cpice(il) = cpice_old(il+nlloss)
   water(il) = water_old(il+nlloss)
   !mass(il) = mass_old(il+nlloss)
   ice(il) = ice_old(il+nlloss)
   !dz(il) = dz_old(il+nlloss)
   !z(il) = z_old(il+nlloss)-(z_old(nlloss)+0.5*dz_old(nlloss))
   lid(il) = lid_old(il+nlloss)
   grainsize(il) = grainsize_old(il+nlloss)
   refrfrac(il) = refrfrac_old(il+nlloss)
  ENDDO
  IF ((tpdens > 0).and.(lid(1) > lidmax).and.(dsnowr < 0.)) dsnowr = 0.
  IF (mult == 1) dsnowr = 0.

  temp(nl-nlloss:nl)=temp_old(nl)
  dens(nl-nlloss:nl)=dens_old(nl)
  energy(nl-nlloss:nl)=energy_old(nl)
  kice(nl-nlloss:nl)=kice_old(nl)
  dtdz(nl-nlloss:nl)=dtdz_old(nl)
  cpice(nl-nlloss:nl)=cpice_old(nl)
  water(nl-nlloss:nl)=water_old(nl)
  mass(nl-nlloss:nl) = mass_old(nl)
  ice(nl-nlloss:nl)=ice_old(nl)
  dz(nl-nlloss:nl)=dz_old(nl)
  z(nl-nlloss:nl)=z_old(nl)-(z_old(nlloss)+0.5*dz_old(nlloss)) 
  lid(nl-nlloss:nl)=lid_old(nl)
  grainsize(nl-nlloss:nl)=grainsize_old(nl)
  refrfrac(nl-nlloss:nl)=refrfrac_old(nl)
  nl = nl_old - nlloss
 ENDIF
ENDIF
freshsnow = 0.0
ENDIF

IF (nlsnow.lt.0) nlsnow = 0

IF (tpdens == 0) CALL RESETDENS

! determine information for mb parameters
sumsnow = 0.		! local snow layer in m snow
summass = 0.		! local snow layer in m we
dsnowacc = 0.		! global snow layer in m snow
hmass = 0.			! global snow layer in m we
sumwater = 0.		! local snow liquid water content in kg m-2
totwater = 0.   ! global snow liquid water content in kg m-2
topsnow = 0.   ! top 10cm snow layer in m snow
topmass = 0.   ! top 10cm snow layer in m we
topwater = 0.   ! top 10cm snow liquid water content in kg m-2

DO il=1,nlsnow
  IF ((lid(il) > 0).and.(lid(il) <= lidmax)) THEN
    sumsnow = z(il) + 0.5*dz(il)
    summass = summass + mass(il)
    sumwater = sumwater + water(il)
  ENDIF
  IF (lid(il) == 1) THEN
    dsnowacc = sumsnow
    hmass = summass
  ENDIF
  IF (sumsnow <= 0.1) THEN
      topwater = sumwater
      topmass = summass
      topsnow = sumsnow
  ENDIF
ENDDO
IF (sumsnow > 0.) THEN
  dsnowh = sumsnow
  totwater = sumwater
ELSE
  dsnowh = 0.
  dsnowacc = 0.
  hmass = 0.
  totwater = 0.
  topwater = 0.
  topmass = 0.
  topsnow = 0.
ENDIF
!write(99,*) dsnowh,rhosn,dz(1),dens(1)

IF (tpdens == 0) THEN   
 coeff = 0.75*(dzdeep - dz0)/zdeep + 2.
 IF ((dz(1).lt.0.5*dz0).or.(dz(1).gt.coeff*dz0)) vink = 5  
ENDIF

!IF (nlgain.ne.0.or.nlloss.ne.0) THEN
!IF (nlloss.ne.0) THEN
!  WRITE(*,'(a,6i6)') 'SNOWHEIGHT: Number of layers is: ',nl_old,nl,nlsnow_old,nlsnow,nlgain,nlloss
!ENDIF

END SUBROUTINE SNOWHEIGHT

!===============================================================================
SUBROUTINE SNOWCONTENT
!===============================================================================
! Routine that calculates the amount of snow melt, refreezing and percolation
! not in here yet is changes in layer density layer thickness or depth due to melt
!===============================================================================
USE GLOBALS_EBM , ONLY : nstep , source , LE , t0 , sbuf 
USE SNOW_EBM , ONLY : nl, nlsnow , water, energy, irrwater, dz, z , temp, cpice, dens, mass, ice, &
&                     runoff , surfmelt, melt , subl , lid , surfwater , drift, sumdrift, &
&                     dsnowr , slushdepth , rhosn, dtdz, kice, refrfrac, grainsize
USE INPUT_EBM , ONLY : tstep , tpdens , lslush , tcalc , dz0, densice , densclosure, lcomment, lrefr
USE CONSTANTS_EBM , ONLY : lm, ls , lv , Tkel, denswater

IMPLICIT NONE
!Functions
REAL	:: solvetemp
!local
INTEGER :: il, illost, ilstlost, refr, lbucket
REAL :: maxwater, restwater, ll, refrwater
REAL :: lmelt, cond, surfen, sumsmelt, dzl
REAL :: tmp, tmp2, conduc, temp_old

illost = 0
ilstlost = 0
refr = 0
lbucket = 1
restwater = 0.
lmelt = source * tstep / lm		!in mm w.e.
IF (tcalc.eq.2.or.tcalc.eq.3) lmelt = 0.

sumsmelt = lmelt
surfmelt = surfmelt + lmelt
IF(LE>0. .and. t0>Tkel) THEN
 ll = lv !Condensation on ice
ELSE
 ll = ls !Sublimation or deposition
ENDIF
 cond = LE*tstep/ll				! in mm w.e.
 water(1) = water(1) + lmelt


il = 1
!IF (tpdens > 0) THEN	! only when density is allowed to change

 IF ((cond > 0.) .and.(t0 >= Tkel)) THEN ! condensation, heat/water added to the surface
  water(1) = water(1) + cond
 ELSE  !IF (cond < 0., or >0 with T<Tkel) 
  IF (cond > 0) THEN
    mass(1) = mass(1) + cond			! remove ice in case of sublimation, add ice when T<0
    IF (tpdens > 0) THEN
    dz(1) = dz(1) + cond/densice
    dens(1) = mass(1)/dz(1)
        ELSE
      dz(1) = mass(1)/dens(1)
    ENDIF
    cpice(1) = 152.5 + 7.122*temp(1)
    z(1) = 0.5*dz(1)
    dtdz(1) = (temp(1)-t0)/dz(1)
    energy(1) = dens(1)*dz(1)*cpice(1)*(Tkel-temp(1))
  ELSE
    surfen = cond
!    il = 1
    DO WHILE (surfen < 0.)		!removal of mass through sublimation
      mass(il) = mass(il) + surfen
      IF (mass(il) < 0.) THEN
        surfen = mass(il)
        mass(il) = 0.
        dz(il) = 0.
        water(il) = water(il) + surfen
        energy(il) = 0.
        z(il) = 0.
        IF (water(il) < 0.) THEN
          water(il) = 0.
          surfen = water(il)
        ELSE
          water(il+1) = water(il+1) + water(il)	!not sure what to do with the water, maybe also remove with cond?
          water(il) = 0.
        ENDIF
        il = il+1
      ELSE
        dz(il) = mass(il)/dens(il)
        cpice(il) = 152.5 + 7.122*temp(il)
        z(il) = 0.5*dz(il)
        dtdz(il) = (temp(il)-t0)/dz(il)
        IF(il.eq.nl) THEN
         energy(il) = energy(il-1)
        ELSE
         energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
        END IF
        surfen = 0.
      ENDIF
    ENDDO   
!    dz(1) = dz(1) + cond/dens(1)
  ENDIF
  IF ((dz(1) <= 0) .and. (lcomment == 1)) THEN
    WRITE(*,'(/,a,4f15.8,i3,/)') 'SNOWCONTENT: dz(1) negative!!! ',dz(1),cond,mass(1),drift,il
  ENDIF
 ENDIF

 sumdrift = sumdrift + drift		! in case luseacc = 0, drift = 0
!! sumdrift = sumdrift + sbuf(20)		! in case luseacc = 0, drift = 0
! surfen = -lmelt - drift*rhosn
 surfen = -lmelt 

 IF ((surfen < 0.) .and. (dsnowr > 0.)) THEN
  dsnowr = dsnowr + surfen/rhosn
  IF (dsnowr < 0.) dsnowr = 0.
 ENDIF

 !il = 1
 DO WHILE (surfen < 0.)		!removal of mass through melt
  mass(il) = mass(il) + surfen
  IF (mass(il) < 0.) THEN
    surfen = mass(il)
    mass(il) = 0.
    dz(il) = 0.
    energy(il) = 0.
    z(il) = 0.
    water(il+1) = water(il+1) + water(il)
    water(il) = 0.
    il = il+1
  ELSE
    dz(il) = mass(il)/dens(il)
    cpice(il) = 152.5 + 7.122*temp(il)
    z(il) = 0.5*dz(il)
    dtdz(il) = (temp(il)-t0)/dz(il)
    IF(il.eq.nl) THEN
     energy(il) = energy(il-1)
    ELSE
     energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
    END IF
    surfen = 0.
  ENDIF
 ENDDO 
 IF (il > 1) THEN
  illost = il-1
  IF (lcomment == 1) write(*,*) 'SNOWCONTENT: ',il-1,' Layer(s) completely melted away or removed by drift'
  DO il=1,nl-illost
    temp(il) = temp(il+illost)
    water(il) = water(il+illost)
    ice(il) = ice(il+illost)
    mass(il) = mass(il+illost)
    dens(il) = dens(il+illost)
    kice(il) = kice(il+illost)
	cpice(il) = cpice(il+illost)
    grainsize(il) = grainsize(il+illost)
    dz(il) = dz(il+illost)
    IF(il==1) THEN
     z(il) = 0.5*dz(il)
     dtdz(il) = (temp(il)-t0)/dz(il)
    ELSE
     z(il) = 0.5*dz(il) + z(il-1)
     dzl=0.5*(dz(il)+dz(il-1))
     dtdz(il) = (temp(il)-temp(il-1))/dzl
    END IF
    energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
    lid(il) = lid(il+illost)
  ENDDO
  nl = nl-illost
  nlsnow = nlsnow - illost
  IF (nlsnow < 0) nlsnow = 0
  illost = 0
 ENDIF ! il > 1
 
!ENDIF ! tpdens

IF(lrefr==0) THEN
 DO il=1,nl
  IF(il.eq.1) water(il) = water(il) + surfwater
  runoff = runoff + water(il)
  water(il) = 0.
 ENDDO
ELSE
refrfrac = 0.0
DO il=1,nl
! first calculate the amount of melt or refreezing depending on layer temperature
! energy >0 in case T<Tkel, refreezing
! else energy < 0, melt
  IF (il == 1) THEN
   water(il) = water(il) + surfwater
  ELSE
    IF (dens(il).ge.densclosure) lbucket = 0.
    IF (lbucket == 1) THEN
      water(il) = water(il) + restwater !Add water that percolates from layer(s) above
      restwater = 0.
    ELSE 
      water(il) = water(il) ! No percolation allowed though ice layer
    END IF
  END IF
  IF (temp(il).ge.Tkel) THEN	! melt
    water(il) = water(il) - energy(il)/lm
    sumsmelt = sumsmelt - energy(il)/lm
      IF (tpdens > 0) THEN 
        mass(il) = mass(il) + energy(il)/lm
        IF (mass(il) < 0.) THEN
          mass(il+1) = mass(il+1)+mass(il)
          water(il+1)=water(il+1)+water(il)
          mass(il) = 0.
          water(il) = 0.
          illost = illost + 1  
          IF (ilstlost < 1) ilstlost = il        
          IF (lcomment == 1) write(*,*) 'SNOWCONTENT: ',il,illost,' Layer(s) completely melted away'
        ENDIF
      ENDIF
    temp(il) = Tkel
  ELSE IF ((temp(il).lt.Tkel).and.(water(il).gt.0.).and.(dens(il).lt.densice)) THEN	! refreezing
    maxwater = denswater * dz(il)* (densice-dens(il))/densice
    refrwater = water(il)
    refr = 1
    IF (maxwater < water(il)) refrwater = maxwater
    IF (energy(il).gt.lm*refrwater) THEN	! refreeze all water
      ice(il) = ice(il) + refrwater
      water(il) = water(il) - refrwater
      temp(il) = temp(il) + lm*refrwater/(dens(il)*cpice(il)*dz(il))
!      energy(il) = energy(il)-(lm*refrwater)
!      temp_old = temp(il)
      IF (tpdens > 0) THEN 
        mass(il) = dens(il)*dz(il) + refrwater
        dens(il) = mass(il)/dz(il)
!        temp(il) = solvetemp(temp_old,energy(il),mass(il))
        energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
        cpice(il) = 152.5+7.122*temp(il)
        IF (dens(il) > densice) THEN
          dens(il) = densice
          restwater = restwater + water(il) 
          water(il) = 0.
        ENDIF
        kice = conduc(dens(il),temp(il))
        IF(il.eq.1) THEN
          dtdz(il) = (temp(il)-t0)/dz(il)
        ELSE
          dzl=0.5*(dz(il)+dz(il-1))
          dtdz(il)=(temp(il)-temp(il-1))/dzl
        ENDIF
      ELSE
        energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
        kice = conduc(dens(il),temp(il))
        cpice(il) = 152.5+7.122*temp(il)
        IF(il.eq.1) THEN
          dtdz(il) = (temp(il)-t0)/dz(il)
        ELSE
          dzl=0.5*(dz(il)+dz(il-1))
          dtdz(il)=(temp(il)-temp(il-1))/dzl
        ENDIF
      ENDIF
    ELSE									! refreeze till energy content is gone
      refrwater = energy(il)/lm
      energy(il) = 0.
      water(il) = water(il) - refrwater
      ice(il) = ice(il) + refrwater
      IF (tpdens > 0) THEN 
        mass(il) = dens(il)*dz(il) + refrwater
        dens(il) = mass(il)/dz(il)
      ENDIF
      temp(il) = Tkel
    ENDIF
  ELSE IF (dens(il).ge.densice) THEN
    restwater = restwater + water(il)
    IF (ice(il) > mass(il)) ice(il) = mass(il)
    if (lid(il) == 0) ice(il) = 0.
    water(il) = 0.
  ENDIF
   
! Then calculate the amount of percolating water
  maxwater = denswater * irrwater(il) * dz(il)* (densice-dens(il))/densice
  IF (dens(il).ge.densice-0.001*densice) maxwater = 0.
  IF (water(il).gt.maxwater) THEN			! too much water in a layer
    restwater = restwater + water(il) - maxwater
    water(il) = maxwater
  ENDIF 

 IF ((tpdens > 0).and.(dens(il).lt.densice)) CALL DENSIFICATION(il)
 
 IF(refr.eq.1) THEN !Refreezing has taken place
  IF(lslush.eq.0) THEN
   refrfrac(il) = refrwater/(mass(il)+water(il))
   refr = 0
  ELSE
   refrfrac(il) = refrwater/mass(il) !Water will be changed by slush routine, do it this way to store data already, and multiply by mass(il)/(mass(il)+water(il)) in slush routine to get it done
  END IF
 END IF
ENDDO
ENDIF

slushdepth = 0.
IF ((lslush == 1).and.(restwater > 0.)) CALL SLUSH(restwater,refr)

runoff = runoff + restwater
! MvT start melt lake routine at this point if runoff > 0 but slope = 0
! IF (runoff.ne.0) .and. (llake == 1) CALL LAKE

melt = melt + sumsmelt
IF ((cond > 0.) .and.(t0 >= Tkel)) THEN
  subl = subl
ELSE 
  subl = subl + cond
ENDIF

IF (illost > 0) THEN
 IF (lcomment == 1) WRITE(*,*) 'SNOWCONTENT: adapt grid when layers are lost'
 DO il=ilstlost,nl-illost
  temp(il) = temp(il+illost)
  water(il) = water(il+illost)
  ice(il) = ice(il+illost)
  mass(il) = mass(il+illost)
  dens(il) = dens(il+illost)
  kice(il) = kice(il+illost)
  cpice(il) = cpice(il+illost)
  grainsize(il) = grainsize(il+illost)
  dz(il) = dz(il+illost)
  IF(il==1) THEN
   z(il) = 0.5*dz(il)
   dtdz(il) = (temp(il)-t0)/dz(il)
  ELSE
   z(il) = 0.5*dz(il) + z(il-1)
   dzl=0.5*(dz(il)+dz(il-1))
   dtdz(il) = (temp(il)-temp(il-1))/dzl
  END IF
  energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
  lid(il) = lid(il+illost)
 ENDDO
 nl = nl-illost
 IF (ilstlost <= nlsnow) nlsnow = nlsnow - illost
 IF (nlsnow < 0) nlsnow = 0
ENDIF

if ( water(1) < -99.) STOP 20

END SUBROUTINE SNOWCONTENT

!===============================================================================
SUBROUTINE IRREDUCIBLE
!===============================================================================
! Routine that calculates the potential for irreducible water content of a snow layer
! based on Schneider and Jansson, 2004 J. Glaciol., 50(168), 25-34 or
! Coleou and Lesaffre, 1998
!===============================================================================
USE SNOW_EBM , ONLY : nl, irrwater, dens
USE INPUT_EBM , ONLY : tpirre , cirre , densice
USE CONSTANTS_EBM , ONLY : denswater !, densice 

IMPLICIT NONE
!local
INTEGER :: il
REAL :: porosity
REAL :: percirrwater		! irreducible water content in % of mass according to schneider (mass of water devided by sum of masses of water and snow)
REAL :: dencap				! density of capillary water (kg/m3)
REAL :: denpor				! density of water when all pores are filled completely (kg/m3)
REAL :: dencol				! density of water when maximum amount according to Coleou is filled (kg/m3)

IF (tpirre.gt.1) THEN

DO il = 1,nl

  IF (dens(il).gt.(densice-0.001*densice)) THEN
    irrwater(il) = 0.
  ELSE
    porosity = (densice - dens(il)) / densice
    IF (tpirre == 2) THEN
      percirrwater = ((0.057*porosity)/(1.-porosity)) + 0.017
    ELSE IF (tpirre == 3) THEN
      percirrwater = 0.0143*exp(3.3*porosity)
    ENDIF
    dencol = percirrwater/(1. - percirrwater) * dens(il)
    denpor = porosity*denswater
    dencap = dencol
    IF (dencap.gt.denpor) dencap = denpor
    irrwater(il) = dencap/(porosity*denswater)
  ENDIF

ENDDO
ELSE
 DO il = 1,nl
  irrwater (il) = cirre
 ENDDO
ENDIF

END SUBROUTINE IRREDUCIBLE

!===============================================================================
SUBROUTINE SLUSH(restwater,refrbool)
!===============================================================================
! Routine that calculates the time scale of runoff
! based on Zuo and Oerlemans, 1996
!===============================================================================
USE SNOW_EBM, ONLY:			nl, runoff, water, dens, dz, lid, surfwater, slushdepth, mass, refrfrac
USE INPUT_EBM, ONLY:		tstep, surfangle, tausteep, tauhor, tau1, slfact, densice
USE CONSTANTS_EBM, ONLY:	Tkel, denswater

IMPLICIT NONE

!Input and output
REAL,INTENT(INOUT)		:: restwater
INTEGER, INTENT(INOUT)	:: refrbool

!Local
INTEGER :: il
REAL :: c1,c2,c3
REAL :: timestar
REAL :: timefactsurf, timefactint
REAL :: maxwater, surplus
DOUBLE PRECISION :: cdouble

 cdouble = 1.
 c1 = tausteep
 c2 = tauhor - tausteep
 c3 = -log( (tau1 - tausteep)/(tauhor - tausteep))/dtan(cdouble)
 
 cdouble = surfangle
 
timestar = c1+c2*exp(c3*dtan(cdouble))		! time scale in days
timefactsurf = (tstep/(24.*3600))/timestar		! timescale per timestep for surface runoff, fraction to runoff per time step
IF (timefactsurf > 1.) timefactsurf = 1.				! 1 = immediate runoff everything
timefactint = timefactsurf/slfact							! timescale per timestep for runoff in snow, slower than on surface, fraction to runoff per time step

surplus = restwater*(1.-timefactint)
restwater = restwater * timefactint
slushdepth = 0.
surfwater = 0.
DO il = nl,1,-1
  IF ((lid(il) > 0).and.(dens(il).lt.densice-0.001*densice)) THEN		! in snow/firn, water cannot be retained in ice 
    maxwater = denswater * dz(il)* (densice-dens(il))/densice
    IF (water(il) < maxwater) THEN
      IF (surplus > (maxwater-water(il))) THEN
        surplus = surplus - maxwater + water(il)
        water(il) = maxwater
        IF (surplus < 0.) surplus = 0.
        slushdepth = slushdepth + dz(il)
      ELSE
        water(il) = water(il) + surplus
        surplus = 0.
        slushdepth = slushdepth + (water(il)/maxwater)*dz(il)
      ENDIF
    ENDIF
  ENDIF
  IF(refrbool.eq.1) THEN!Refreezing has occurred in snowcontent subroutine
   refrfrac = refrfrac*(mass(il)/(mass(il)+water(il)))
   refrbool = 0
  END IF
ENDDO
IF (surplus > 0.) THEN		!water on ice
  surfwater = surplus*(1.-timefactsurf)
  restwater = restwater + surplus*timefactsurf
ENDIF

END SUBROUTINE SLUSH
!===============================================================================
SUBROUTINE DENSIFICATION(il)
!===============================================================================
! Routine that calculates the densification of the snowpack due to gravity and
! vapour transport see Herron and Langway (1980), Li and Zwally (2004), Colbeck (1993), Helsen et al. 2008, Arthern et al 2010.
!===============================================================================
USE GLOBALS_EBM, ONLY: t0
USE SNOW_EBM, ONLY: nl, temp, dens, z, dz
USE CONSTANTS_EBM, ONLY: Tkel, betadens, rd, rv, rstar, densnow, Deff, es0, ls, accur, tempcut, g!, densice
USE INPUT_EBM, ONLY: tstep, tpdens, accyear, zdeep, T10m, densice

IMPLICIT NONE

!Input
INTEGER :: il
!local
REAL :: dens_o
REAL :: K0G , K0 , E, J1, J2 , alpha
REAL :: Ec,Eg
REAL :: coef1, coef2, coef3, coef4
REAL :: coeftime,beta
REAL :: temp1, temp2, temp3, z1, z2, z3
REAL :: templim
REAL :: drhodt, dJdz,grav

dens_o = dens(il)

IF ((il.gt.1).and.(il.lt.nl)) THEN
  temp1 = temp(il-1)
  temp2 = temp(il)
  temp3 = temp(il+1)
  z1 = z(il-1)
  z2 = z(il)
  z3 = z(il+1)
ELSE IF(il.eq.1) THEN
  temp1 = t0
  temp2 = temp(il)
  temp3 = temp(il+1)
  z1 = 0.
  z2 = z(il)
  z3 = z(il+1)
ELSE IF(il.eq.nl) THEN
  temp1 = temp(il-1)
  temp2 = temp(il)
  temp3 = temp(il)
  z1 = z(il-1)
  z2 = z(il)
  z3 = zdeep
ENDIF

! input accyear is in m w.e.
 coeftime = 1./(365.25*24.*3600.)		!factor converting accumulation rate per year to per second

IF (tpdens == 2) THEN							! Herron and Langway 1980
  IF (dens_o.le.550.) THEN
    Ec = 10160. 
    K0 = 11.
    alpha = 1.
  ELSE
    Ec = 21400. 
    K0 = 575.
    alpha = 0.5
  ENDIF
  coef1 = -Ec/(rstar*temp2) 
  coef2 = accyear**alpha
  coef3 = (densice - dens_o)		!/densice
  grav = K0*exp(coef1)*coef2*coef3*coeftime
  drhodt =  grav 

ELSEIF ((tpdens == 3).or.(tpdens == 4)) THEN						! Li and Zwally 2004 (as described in Arthern et al. 2004)
  templim = temp(il)
  IF (templim > tempcut) templim = tempcut
  Ec = 883.8*((ABS(templim - Tkel))**(-0.885))			!KJ/mol *1000. problem when you multiply with 1000
  K0G = 8.36*((ABS(templim - Tkel))**(-2.061))		!mm2/yr according to article but I think mm2/kg
  beta = 139.21-0.542*T10m
  IF (beta < betadens) beta = betadens
  K0 = beta*K0G									
  coef1 = -Ec/(rstar*temp2) 
  coef2 = accyear
  coef3 = (densice - dens_o)		!/densice
  grav = K0*exp(coef1)*coef2*coef3*coeftime
  drhodt =  grav 
  IF (tpdens == 4) THEN							! adition of effects of vapor transport Li and Zwally 2004
    coef1 = es0/(Rv*Rv)
    coef2 = (ls-rv*temp1)/(temp1**3)
    coef3 = exp((ls/rv)*((1./Tkel)-(1./temp1)))
    coef4 = (temp2-temp1)/(z2-z1)
    J1 = -Deff*coef1*coef2*coef3*coef4
    coef2 = (ls-rv*temp2)/(temp2**3)
    coef3 = exp((ls/rv)*((1./Tkel)-(1./temp2)))
    coef4 = (temp3-temp2)/(z3-z2)
    J2 = -Deff*coef1*coef2*coef3*coef4
    dJdz = -(J2-J1)/dz(il)
    drhodt =  drhodt + dJdz 
    IF (drhodt < 0. ) drhodt = 0.
  ENDIF

ELSEIF (tpdens == 5) THEN					!Helsen 2008
  templim = temp(il)
  IF (templim > tempcut) templim = tempcut
  Ec = 883.8*((ABS(templim - Tkel))**(-0.885))			!KJ/mol *1000. problem when you multiply with 1000
  K0G = 8.36*((ABS(templim - Tkel))**(-2.061))		!mm2/yr according to article but I think mm2/kg
  beta = 76.138-0.28965*T10m
  IF (beta < betadens) beta = betadens
  K0 = beta*K0G									
  coef1 = -Ec/(rstar*temp2) 
  coef2 = accyear
  coef3 = (densice - dens_o)		!/densice
  grav = K0*exp(coef1)*coef2*coef3*coeftime
  drhodt =  grav 

ELSEIF ((tpdens == 6) .or. (tpdens == 7)) THEN						! Arthern et al. 2010, Ligtenberg 2011
  Ec =  60.*1000.				! 60 kJ/mol
  Eg = 42.4*1000.				! 42.4 kJ/mol
  IF (dens_o <= 550.) THEN
    K0 = 0.07
    coef4 = 1.415-0.147*log(accyear*1000.)
  ELSE
    K0 = 0.03
    coef4 = 2.335-0.288*log(accyear*1000.)
  ENDIF
  IF (coef4 < 0.25) coef4 = 0.25
  IF (tpdens == 6) coef4 = 1.				! Arthern
  coef1 = -(Ec/(rstar*temp2)) + (Eg/(rstar*T10m))
  coef2 = g*accyear*1000.		!accyear must be in mm per year
  coef3 = (densice - dens_o)		!/densice
  grav = K0*exp(coef1)*coef2*coef3*coef4*coeftime
  drhodt =  grav

ENDIF

dens(il) = drhodt*tstep + dens_o

IF (dens(il) > densice-accur) dens(il) = densice
IF ((dens(il) < densnow) .or. (dens(il) > densice)) THEN
WRITE(*,*) 'DENSIFICATION error: ',il,dens(il),dens_o,coef1,coef2,coef3,coef4,grav,densnow
STOP 21
ENDIF

END SUBROUTINE DENSIFICATION

!===============================================================================
