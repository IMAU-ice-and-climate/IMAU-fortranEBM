!===============================================================================
!
! Routines related to the sub-surface snow model excluding the calculation
! of the surface temperature
!
!===============================================================================
SUBROUTINE INITSNOW
!===============================================================================
! Initialises the sub-surface profiles of density, temperature and water content
!===============================================================================
USE SNOW_EBM , ONLY : nl, z, dz, temp, dens, mass, rhocp, cpice, kice, lid, energy
USE INPUT_EBM , ONLY : chstation, lcomment ,dfirn , penetration, tcalc, dzrad
USE CONSTANTS_EBM , ONLY : Tkel 
USE RADPEN_EBM , ONLY : zrad , nlradmax
USE FILES , ONLY : uo1

IMPLICIT NONE
!Local
INTEGER :: il
REAL :: conduc
REAL :: snowtemp, snowdens
REAL :: snowtemp_STGL, snowdens_STGL
!Assuming dry snow

IF (lcomment == 1) WRITE(*,'(/,A)') 'Initialise the subsurface, temp and dens'
WRITE(uo1,'(/,A)') 'Initialise the subsurface, temp and dens'

DO il = 1,nl
!!!determine general profile based on either measurements or theory!
 IF (chstation.ne."STGL_1999") THEN
   dens(il) = snowdens(z(il),lid(il),il)
   temp(il) = snowtemp(z(il),il)
 ELSE IF (chstation.eq."STGL_1999") THEN
   IF (il > 1) THEN
     dens(il) = snowdens_STGL(z(il),dz(il),z(il-1),dz(il-1),il,lid(il))
     temp(il) = snowtemp_STGL(z(il),z(il-1),temp(il-1))
   ELSE
     dens(il) = snowdens_STGL(z(il),dz(il),0.,0.,il,lid(il))
     temp(il) = snowtemp_STGL(z(il),0.,Tkel)
   ENDIF
 ENDIF

 mass(il) = dens(il)*dz(il)
 cpice(il) = 152.5+7.122*temp(il)			!from Patterson1994
 rhocp(il) = dens(il)*cpice(il)
 kice(il) = conduc(dens(il),temp(il))		! effective conductivity
 IF(il.eq.nl) THEN
  energy(il) = energy(il-1)
 ELSE
  energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
!  energy(il) = dens(il)*dz(il)*((152.5+7.122*Tkel)*Tkel - cpice(il)*temp(il))
 END IF
 
ENDDO
!kice(1) = 50.0

 CALL IRREDUCIBLE
 
IF (penetration == 1) THEN
  DO il = 1, nlradmax
    zrad(il) = (il-0.5)*dzrad
  ENDDO
ENDIF

! insert temperature profile only changes results significantly for first
! two days. Put something in based on day of year and given amplitude.

END SUBROUTINE INITSNOW

!===============================================================================
SUBROUTINE INITGRID
!===============================================================================
! Initialises the sub-surface grid
!===============================================================================
USE SNOW_EBM , ONLY : nl, nlinit, nlsnow, dz, z , lid , lidmax , dsnowacc , &
&                     sumdrift , cumdrift , cumdriftobs , hmass , totwater , topwater, topsnow, topmass, mass, water
USE INPUT_EBM , ONLY : lcomment, dz0, dzdeep, zdeep, dsnow, dfirn
USE CONSTANTS_EBM , ONLY : nlmax
USE FILES , ONLY : uo1

IMPLICIT NONE
!Local
REAL , PARAMETER :: small = 0.00001
INTEGER :: il, id
REAL :: depth, coeff, depthl(nlmax)
REAL :: zfirn

IF (lcomment == 1) WRITE(*,'(/,A)') 'Initialise the subsurface grid'
WRITE(uo1,'(/,A)') 'Initialise the subsurface grid'

!No annual layers

depth = 0.0
 coeff = (dzdeep - dz0)/zdeep
id = 1		! snow layer
nlsnow = 1
IF ((dsnow.lt.dz0).and.(dfirn.lt.dz0)) THEN
 id = 0			! ice layer
 nlsnow = 0
ELSE IF (dsnow.lt.dz0) THEN
 id = 2			! firn layer, counted as nlsnow
ENDIF
zfirn = dsnow + dfirn
IF (zfirn > zdeep) zfirn = zdeep

depthl(1) = dz0 
dz(1) = dz0							!thickness of layer
z(1) = 0.5*dz0						!depth of layer = half way the layer, z positive downwards
depth = depth + depthl(1)
lid(1) = id

il = 2
DO WHILE (depthl(il-1).lt.zdeep)
 depthl(il) = depthl(il-1) + coeff*depth + dz0
 dz(il) = depthl(il) - depthl(il-1)
 z(il) = depth + 0.5*dz(il)
 lid(il) = id
 IF ((depthl(il).gt.dsnow).and.(depthl(il).lt.zfirn).and.(id.gt.0).and.(id.lt.2)) THEN
  depthl(il)=dsnow
  dz(il) = depthl(il) - depthl(il-1)
  z(il) = depth + 0.5*dz(il)
  depth = depthl(il)
  id = 2
  IF (zfirn == 0.) id = 0
  IF (dz(il) < dz0) THEN
    depthl(il)=dsnow
    dz(il-1) = depthl(il) - depthl(il-1) + dz(il-1)
    z(il-1) = z(il-1) + 0.5*dz(il)
    il = il-1
    depthl(il)=dsnow
    depth = depthl(il)
  ENDIF
 ELSE IF ((depthl(il).gt.zfirn).and.(depthl(il).lt.zdeep).and.(id.gt.0)) THEN
  depthl(il)=zfirn
  dz(il) = depthl(il) - depthl(il-1)
  z(il) = depth + 0.5*dz(il)
  depth = depthl(il) 
  id = 0
  IF (dz(il) < dz0) THEN
    depthl(il)=zfirn
    dz(il-1) = depthl(il) - depthl(il-1) + dz(il-1)
    z(il-1) = z(il-1) + 0.5*dz(il)
    il = il-1
    depthl(il)=zfirn
    depth = depthl(il)
  ENDIF
 ELSEIF (depthl(il).ge.zdeep-small) THEN
  depthl(il)=zdeep
  dz(il) = depthl(il) - depthl(il-1)
  IF (dz(il) < 0.5*dz0) THEN
    dz(il) = dz(il-1)
    depthl(il) = depth + dz(il)
  ENDIF
  z(il) = depth + 0.5*dz(il)
  GOTO 1
 ELSE
  depth = depthl(il)
 ENDIF
 IF (lid(il).gt.0) nlsnow = nlsnow + 1
 il = il + 1
 IF (il.eq.nlmax) THEN
   WRITE(*,*) 'Number of layers exceeds maximum possible layers'
   STOP 30
 ENDIF
ENDDO
1 CONTINUE

!IF (lid(il).gt.0) nlsnow = nlsnow + 1
nl = il
nlinit=nl

sumdrift = 0.
 cumdrift = 0.
 cumdriftobs = 0.
dsnowacc = 0.
hmass = 0.
totwater = 0.
topwater = 0.
topsnow = 0.
topmass = 0.

lidmax = 1
DO il=1,nlsnow
  IF (lid(il) == 1) THEN	!this mb year snow layer
    dsnowacc = z(il) + 0.5*dz(il)	! in m snow
    hmass = hmass + mass(il)		! in m we
  ENDIF
ENDDO
 
IF (lcomment == 1) WRITE(*,*) 'initial number of layers is: ',nl,nlsnow,dz(nl),z(nl),lidmax

END SUBROUTINE INITGRID

!===============================================================================
SUBROUTINE RESIZEGRID
!===============================================================================
! resizes the sub-surface grid after densification
! mass = rho*dz, dz = mass/rho
!===============================================================================
USE SNOW_EBM , ONLY : nl, z, dz, mass, dens, vink , nlinit , lid , hmass ,  &
&                     dsnowh , dsnowacc , lidmax
USE INPUT_EBM , ONLY : dz0, dzdeep, zdeep  , densice , lcomment
USE CONSTANTS_EBM , ONLY : nlmax , accur 

IMPLICIT NONE

! local
REAL :: sumsnow , summass , sumwater , coeff
INTEGER :: il

 coeff = 0.75*(dzdeep - dz0)/zdeep + 2.

sumsnow = 0.		! local snow layer in m snow
summass = 0.		! local snow layer in m we
dsnowacc = 0.		! global snow layer in m snow
hmass = 0.			! global snow layer in m we


dz(1) = mass(1)/dens(1)
z(1) = 0.5*dz(1)
IF ((dz(1).lt.0.5*dz0).or.(dz(1).gt.coeff*dz0)) vink = 1
IF (lid(1) == 1) THEN
 dsnowacc = dz(1)
 hmass = mass(1)
ENDIF
IF ((lid(1) > 0).and.(lid(1) <= lidmax)) THEN
 sumsnow = dz(1)
 summass = mass(1)
ENDIF

IF ((z(1).gt.0.5*dz(1)) .or. (dz(1).le.0.)) THEN
    il = 1
    IF (lcomment == 1) WRITE(*,'(a,2i6,4f12.5)') '1 RESIZEGRID ',nl,il,dz(il),z(il),mass(il),dens(il)
!    STOP
ENDIF

DO il=2,nl
  dz(il) = mass(il)/dens(il)
  z(il) = 0.5*(dz(il)+ dz(il-1)) + z(il-1)
  IF (dz(il).lt.0.5*dz0) vink = 2
  IF (dz(il).le.0. .and. lcomment == 1) THEN
    WRITE(*,'(a,2i6,4f12.5)') 'nl RESIZEGRID ',nl,il,dz(il),z(il),mass(il),dens(il)
  ENDIF
  IF ((lid(il) > 0).and.(lid(il) <= lidmax)) THEN
    sumsnow = z(il) + 0.5*dz(il)
    summass = summass + mass(il)
  ENDIF
  IF (lid(il) == 1) THEN
    dsnowacc = sumsnow
    hmass = summass
  ENDIF
ENDDO

IF (sumsnow > 0.) THEN
 dsnowh = sumsnow
ELSE
 dsnowh = 0.
 dsnowacc = 0.
 hmass = 0.
ENDIF

! limit number of layers to speed up calculations
IF ((nl > 3.0*nlinit) .or. (nl >= nlmax-1)) vink = 3

END SUBROUTINE RESIZEGRID

!===============================================================================
SUBROUTINE REDEFGRID
!===============================================================================
! redefines the sub-surface grid
! now only works in case only snow is present, no ice layer below
!===============================================================================
USE SNOW_EBM , ONLY : nl, nlsnow, z, dz, temp, dens, mass, ice, water, lid, vink,&
&                     dsnowr , rhosn, energy, kice, cpice, dtdz, grainsize
USE INPUT_EBM , ONLY : dz0, dzdeep, zdeep , densice , lcomment
USE CONSTANTS_EBM , ONLY : nlmax , accur, Tkel
USE GLOBALS_EBM , ONLY : t0,sbuf

IMPLICIT NONE

!Local
INTEGER :: nl_old,nlsnow_old
INTEGER :: il,ik,iks,ike,ilp,ill
INTEGER :: check
INTEGER , DIMENSION(nlmax) :: lid_old
INTEGER :: lid_tmp
REAL, DIMENSION(nlmax) :: temp_old, water_old, dens_old, mass_old, ice_old, z_old, dz_old, grainsize_old
REAL :: temp_tmp, water_tmp, dens_tmp, mass_tmp, ice_tmp, z_tmp, dz_tmp, grainsize_tmp
REAL :: depth, coeff, depthl(nlmax), small, depthnext, depths,depthe, ddepth, dzl, conduc

!No annual layers or ice below the snow

!It is not necessary to keep track of refrfrac when splitting/merging layers because it is calculated in SNOWCONTENT

small = 0.0000001

nl_old = nl
nlsnow_old = nlsnow

nl = 0
nlsnow = 0

temp_old = temp
water_old = water
dens_old = dens
mass_old = mass
ice_old = ice
z_old = z
dz_old = dz
lid_old = lid
grainsize_old = grainsize

temp_tmp = 0.
water_tmp = 0.
ice_tmp = 0.
mass_tmp = 0.
dz_tmp = 0.
grainsize_tmp = 0.

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
 cpice = 0.
dtdz = 0.

 coeff = 0.75*(dzdeep - dz0)/zdeep

depth = 0.0				! depth of upper layer boundary, depthl = depth of lower boundary
depthl = 0.0
ike=0
ik = 0
il = 1

DO WHILE ((il.le.nlmax).and.(ike.lt.nl_old).and.(depth.lt.z_old(nl_old)+0.5*dz_old(nl_old)).and.(nl.lt.nlmax))
 depthl(il) = depth*(1.+coeff) + dz0
 iks=ike+1
 ik=iks
 ilp = 0
 DO WHILE ((ik.lt.nl_old).and.(depthl(il).gt.(z_old(ik)+0.5*dz_old(ik)+small)).and.(lid_old(ik+1).eq.lid_old(iks)))
   ik = ik + 1
 ENDDO
 ike = ik
 IF ((ike == 1) .and. (depthl(il).gt.0.5*dz0) .and. (lid_old(ik).ne.lid_old(ik+1)) .and. (z_old(ik).le.0.5*dz0)) THEN
   ike = 2
   dsnowr = mass_old(1)/dens_old(1) !dz_old(1)
 ENDIF
   ilp=0
   depthnext = depthl(il)*(1.+coeff) + dz0
  DO WHILE (depthnext.le.(z_old(ike)+0.5*dz_old(ike)))
     depthnext = depthnext*(1.+coeff) + dz0
     ilp=ilp+1
   ENDDO
 IF (iks == ike) THEN
   check=1		!nothing to do with the layer
   IF (ilp > 0) check = 3		!split layers
 ELSE
   check=2		!merge layers
   IF ((ike == 2).and.((z_old(ike)+0.5*dz_old(ike)) > 1.5*dz0)) THEN
     check = 4		! first merge and then split in 2 layers
   ELSE IF (ilp > 1) THEN
     check = 5		! first merge and then split in multiple layers
   ENDIF
   IF (dens_old(iks) > 700. .and. dz_old(iks) > dz0) THEN
     check = 1		!nothing to do with the layer
     ike = iks
   ENDIF
 ENDIF

 IF ((check <=2).or.(check == 4)) THEN		!keep layers as is or merge them
  DO ik=iks,ike
   temp(il) = temp(il) + temp_old(ik)*dz_old(ik)
   grainsize(il) = grainsize(il) + grainsize_old(ik)*dz_old(ik)
   water(il) = water(il) + water_old(ik)
   ice(il) = ice(il) + ice_old(ik)
   mass(il) = mass(il) + mass_old(ik)
   dz(il) = dz(il) + dz_old(ik)
   dens(il) = mass(il)/dz(il)
   
  ENDDO
  temp(il) = temp(il)/dz(il)
  grainsize(il) = grainsize(il)/dz(il)
  dens(il) = mass(il)/dz(il)
  IF (dens(il) > densice) dens(il) = densice
  z(il) = z_old(ike)+0.5*dz_old(ike)-0.5*dz(il)
  lid(il) = lid_old(ike)
  depth = z(il) + 0.5*dz(il)
  IF (lid(il).gt.0) nlsnow = nlsnow + 1
  nl = nl + 1
  il = il + 1
  IF (check == 4) THEN
    water(il-1) = 0.5*water(il-1)
    ice(il-1) = 0.5*ice(il-1)
    mass(il-1) = 0.5*mass(il-1)
    dz(il-1) = 0.5*dz(il-1)
    z(il-1) = z(il-1)-0.5*dz(il-1)
    temp(il) = temp(il-1)
    grainsize(il) = grainsize(il-1)
    water(il) = water(il-1)
    ice(il) = ice(il-1)
    mass(il) = mass(il-1)
    dens(il) = dens(il-1)
    IF (dens(il) > densice) dens(il) = densice
    dz(il) = dz(il-1)
    z(il) = z(il-1) + dz(il)
    lid(il) = lid(il-1)
    IF (lid(il).gt.0) nlsnow = nlsnow + 1
    nl = nl+1
    il = il+1
  ENDIF
 ELSE
  IF (check == 5) THEN
   DO ik=iks,ike
    temp_tmp = temp_tmp + temp_old(ik)*dz_old(ik)
    grainsize_tmp = grainsize_tmp + grainsize_old(ik)*dz_old(ik)
    water_tmp = water_tmp + water_old(ik)
    ice_tmp = ice_tmp + ice_old(ik)
    mass_tmp = mass_tmp + mass_old(ik)
    dz_tmp = dz_tmp + dz_old(ik)
   ENDDO
   temp_tmp = temp_tmp/dz_tmp
   grainsize_tmp = grainsize_tmp/dz_tmp
   dens_tmp = mass_tmp/dz_tmp
   IF (dens_tmp > densice) dens_tmp = densice
   z_tmp = z_old(ike)+0.5*dz_old(ike)-0.5*dz_tmp
   lid_tmp = lid_old(ike)
  ELSE
    temp_tmp = temp_old(ike)
    grainsize_tmp = grainsize_old(ike)
    water_tmp = water_old(ike)
    ice_tmp = ice_old(ike)
    mass_tmp = mass_old(ike)
    dens_tmp = dens_old(ike)
    dz_tmp = dz_old(ike)
    z_tmp = z_old(ike)
    lid_tmp = lid_old(ike)
  ENDIF
  
   depths = depth
   depthe = depth*(1.+coeff) + dz0
   DO ill = 1,1+ilp
     ddepth = depthe-depths
     temp(il) = temp_tmp
     grainsize(il) = grainsize_tmp
     water(il) = water_tmp*ddepth/dz_tmp
     ice(il) = ice_tmp*ddepth/dz_tmp
     mass(il) = mass_tmp*ddepth/dz_tmp
     dens(il) = dens_tmp
     dz(il) = ddepth
     lid(il) = lid_old(ike)
     IF(il==1) THEN
      z(il) = 0.5*ddepth
     ELSE
      z(il) = z(il-1) + 0.5*ddepth + 0.5*dz(il-1)
     END IF
     depth = depthe
     IF (lid(il).gt.0) THEN
       nlsnow = nlsnow + 1
    ENDIF
     depths = depthe
     depthe = depths*(1.+coeff) + dz0
     IF (ill == ilp) depthe = (z_tmp+0.5*dz_tmp)
     il = il + 1
     nl = nl + 1
   ENDDO
   temp_tmp = 0.
   grainsize_tmp = 0.
   water_tmp = 0.
   ice_tmp = 0.
   mass_tmp = 0.
   dz_tmp = 0.

 ENDIF

ENDDO

IF (z(nl)+0.5*dz(nl) < zdeep - dzdeep) THEN		!add layer to maintain sufficient deep model
  nl = nl + 1
  dz(nl) = zdeep - z(nl-1)+0.5*dz(nl-1)
  z(nl) = z(nl-1)+0.5*dz(nl-1)+0.5*dz(nl)
  temp(nl) = temp(nl-1)
  grainsize(nl) = grainsize(nl-1)
  water(nl) = 0.
  ice(nl) = 0.
  lid(nl) = lid(nl-1)
  dens(nl) = dens(nl-1)
  IF (dens(nl-1) >= densice) THEN
  dens(nl) = densice
  lid(nl) = 0
  ENDIF
  mass(nl) = dens(nl)*dz(nl)
  IF (lid(nl-1) == 1) lid(nl) = 2
  IF (lcomment == 1) WRITE(*,'(a,2i6,6f12.5)') 'REDEFGRID: added layer at bottom',nl,lid(nl),&
&                    z(nl),dz(nl),z(nl-1),dz(nl-1),dzdeep,dens(nl)
ENDIF

DO il=1,nl
 kice(il) = conduc(dens(il),temp(il))
 cpice(il) = 152.5 + 7.122 * temp(il)
 IF(il==1) THEN
  dtdz(il) = (temp(il)-t0)/dz(il)
 ELSE
  dzl=0.5*(dz(il)+dz(il-1))
  dtdz(il) = (temp(il)-temp(il-1))/dzl
 END IF
 energy(il) = dens(il)*dz(il)*cpice(il)*(Tkel-temp(il))
ENDDO
!kice(1) = 50.0
!IF (lcomment == 1) WRITE(*,*) 'REDEFGRID: Redefine grid ', nl_old,nl,nlsnow_old,nlsnow,dz(1),dz_old(1),z_old(1),vink,check
vink =0

END SUBROUTINE REDEFGRID
!===============================================================================
