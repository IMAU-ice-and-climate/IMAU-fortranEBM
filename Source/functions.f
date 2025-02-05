!===============================================================================
!
! All routines related to the calculation of the (initial) density and temperature profiles
!
!===============================================================================
FUNCTION snowtemp(z,il)
!===============================================================================
USE INPUT_EBM , ONLY : T10m , chstation , lcomment
USE CONSTANTS_EBM , ONLY : Tkel
USE FILES , ONLY : uo1

IMPLICIT NONE
!input
REAL :: z
INTEGER :: il
!Local
REAL :: temp
REAL :: a(10)
!Output
REAL :: snowtemp

! SURE array
data a/-9.9683,-25.67,11.331,-2.7834,0.43098,-0.043561,0.0028545,-0.00011641,2.6765e-06,-2.6455e-08/

SELECT CASE(chstation)
 CASE("ant_aws01","ant_aws02","ant_aws03")	!AWS 1, 2, 3
  IF (z.lt.0.25) THEN
    temp = -16.8+14.4*z + Tkel
  ELSEIF (z.lt.0.5) THEN
    temp = -11.2-8*z + Tkel
  ELSEIF (z.lt.1.0) THEN
    temp = -13.0-4.4 + Tkel
  ELSEIF (z.le.2.) THEN
    temp = -13.8-3.6*z + Tkel
  ELSEIF (z.le.4.) THEN
    temp = -17.2-1.9*z + Tkel
  ELSEIF (z.le.8.) THEN
    temp = -23.8-0.25*z + Tkel
  ELSE
    temp = T10m
  ENDIF
  SELECT CASE(chstation)
   CASE("ant_aws02")
    temp = temp -5.0
   CASE("ant_aws03")
    temp = temp -12.0
  END SELECT
 CASE("ant_aws04")	!AWS 4
  IF (z.le.0.05) THEN
    temp = -6.3915+48.253*z + Tkel
  ELSEIF (z.le.0.1) THEN
    temp = -3.6557-6.462*z + Tkel
  ELSEIF (z.le.0.2) THEN
    temp = -2.9737-13.282*z + Tkel
  ELSEIF (z.le.0.4) THEN
    temp = -4.4619-5.841*z + Tkel
  ELSEIF (z.le.0.8) THEN
    temp = -2.8833-9.7875*z + Tkel
  ELSEIF (z.le.2.) THEN
    temp = -7.37-4.1792*z + Tkel
  ELSEIF (z.le.10.) THEN
    temp = -6.081-6.601*z+0.97859*z**2-0.04496*z**3 + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws05")	!AWS 5
  IF (z.le.0.05) THEN
    temp = -9.362+36.856*z + Tkel
  ELSEIF (z.le.0.1) THEN
    temp = -7.6642+2.9*z + Tkel
  ELSEIF (z.le.0.2) THEN
    temp = -7.3772+0.029998*z + Tkel
  ELSEIF (z.le.0.4) THEN
    temp = -6.9374-2.169*z + Tkel
  ELSEIF (z.le.0.8) THEN
    temp = -6.4775-3.3187*z + Tkel
  ELSEIF (z.le.2.) THEN
    temp = -5.7942-4.1729*z + Tkel
  ELSEIF (z.le.10.) THEN
    temp = -13.259-0.44075*z + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws06")	!AWS 6
  IF (z.le.2.) THEN
    temp = (-12.804-2.273*z) + Tkel
  ELSEIF (z.le.4.) THEN
    temp = (-14.883-1.7098*z) + Tkel
  ELSEIF (z.le.6.) THEN
    temp = (-19.291-0.6079*z) + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws08")	!AWS 8
  IF (z.le.0.05) THEN
    temp = -26.547+67.976*z + Tkel
  ELSEIF (z.le.0.1) THEN
    temp = -24.07+18.44*z + Tkel
  ELSEIF (z.le.0.2) THEN
    temp = -23.019+7.93*z + Tkel
  ELSEIF (z.le.0.4) THEN
    temp = -21.785+1.66*z + Tkel
  ELSEIF (z.le.0.8) THEN
    temp = -19.26-4.6025*z + Tkel
  ELSEIF (z.le.2.) THEN
    temp = -17.19-7.19*z + Tkel
  ELSEIF (z.le.10.) THEN
    temp = -29.84-0.865*z + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws09")	!AWS 9
  IF (z.le.2.) THEN
    temp = (-22.74-9.68*z) + Tkel
  ELSEIF (z.le.10.) THEN
    temp = (-41.245-0.3475*z) + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws00","ant_aws10")	!AWS 00 and 10 (new station)
  IF (z.le.1.) THEN
    temp = (-11.626-5.1288*z) + Tkel
  ELSEIF (z.le.10.) THEN
    temp = (-15.425-0.90747*z) + Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("SURE_2007")	!Summit Greenland
  IF (z.le.10.) THEN
    temp = a(1) + a(2)*z + a(3)*z**2 + a(4)*z**3 + a(5)*z**4 + a(6)*z**5 + a(7)*z**6 &
&        + a(8)*z**7 + a(9)*z**8 + a(10)*z**9 + Tkel
    IF (temp > -15.+Tkel) temp = -15.+Tkel
  ELSE
    temp = T10m
  ENDIF
 CASE("ant_aws11")!Based on model spin-up
  IF(z.le.1.) THEN
   temp = -6.91609495e-02*z**9. + 1.29608840e+00*z**8. - 1.02589288e+01*z**7. + 4.46243617e+01*z**6. - 1.16170824e+02*z**5. +&
   1.84387462e+02*z**4. - 1.72773349e+02*z**3. + 8.58726966e+01*z**2. - 2.05820030e+01*z + 2.66967445e+02
  ELSEIF(z.le.14.) THEN
   temp = -1.72171032e-04*z**5. + 9.40341114e-03*z**4. - 1.98161435e-01*z**3. + 2.00063342e+00*z**2. &
   - 9.50111493e+00*z + 2.71039719e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_aws14")!Based on model spin-up
  IF (z.le.2.0) THEN ! MvT start on 18 Jan 2011 from thermostor string
  temp = Tkel
  !ELSEIF(z.le.10.0) THEN
  !temp = Tkel + (-1.1111111111111*z+1.1111111111111)
   ELSEIF(z.le.2.0) THEN
    temp = 8.05789524e-02*z**4. - 1.66345544e-01*z**3. - 1.40751515e+00*z**2. - 5.67841375e-01*z + 2.73175240e+02
   ELSEIF(z.le.4.0) THEN
    temp = -1.07062096e-04*z**11. + 4.15831657e-03*z**10. - 6.90655950e-02*z**9. + 6.39775202e-01*z**8. &
    - 3.61422778e+00*z**7. + 1.27922480e+01*z**6. - 2.80665074e+01*z**5. + 3.62325481e+01*z**4. &
   - 2.26642477e+01*z**3. - 3.58240572e+00*z**2. + 1.10150599e+01*z + 2.70034743e+02
   ELSEIF(z.le.18.0) THEN
    temp = 4.91130921e-08*z**8. - 4.86407189e-06*z**7. + 2.09046288e-04*z**6. - 5.11920599e-03*z**5. &
    + 7.86489021e-02*z**4. - 7.81063851e-01*z**3. + 4.90252318e+00*z**2. - 1.75580344e+01*z &
    + 2.83644493e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_aws15")!Based on model spin-up
  IF(z.le.15.0) THEN
   temp = 6.17380317e-11*z**12. - 5.82045398e-09*z**11. + 2.12477073e-07*z**10. - 3.15177928e-06*z**9. &
   - 1.53763891e-05*z**8. + 1.42140838e-03*z**7. - 2.61452292e-02*z**6. + 2.60893708e-01*z**5. &
   - 1.56635366e+00*z**4. + 5.52373734e+00*z**3. - 9.64486679e+00*z**2. + 1.96137278e+00*z + 2.67720092e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_aws16")!Based on model spin-up
  IF(z.le.10.0) THEN
   temp = 2.42962830e-03*z**4. - 8.31974968e-02*z**3. + 1.04917746e+00*z**2. - 5.71384769e+00*z + 2.61432548e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_aws17")!Based on model spin-up
  IF(z.le.13.0) THEN
   temp = 1.78221909e-06*z**7. - 1.45598217e-04*z**6. + 4.85755730e-03*z**5. - 8.44032340e-02*z**4. + 7.98126433e-01*z**3. &
   - 3.77657250e+00*z**2. + 5.78177267e+00*z + 2.68028289e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_aws18")!Based on model spin-up
 temp = T10m
  !temp = 8.49874427e-13*z**10. - 4.34358399e-10*z**9. + 6.13934836e-08*z**8. - 4.24843155e-06*z**7. &
  !+ 1.70333656e-04*z**6. - 4.20590496e-03*z**5. + 6.43735554e-02*z**4. - 5.84534683e-01*z**3. &
  !+ 2.75256135e+00*z**2. - 4.13351146e+00*z + 2.66603225e+02
  IF(temp.ge.273.1) temp = 273.1
  WRITE(*,*) z, temp
 CASE("ant_aws19")!Based on model spin-up
  IF(z.le.11.0) THEN
   temp = 3.17645496e-09*z**14. - 3.09989900e-07*z**13. + 1.35629461e-05*z**12. - 3.51295517e-04*z**11. + 5.99062912e-03*z**10. &
   - 7.07667524e-02*z**9. + 5.93259334e-01*z**8. - 3.55634468e+00*z**7. + 1.51603741e+01*z**6. - 4.51206233e+01*z**5. &
   + 9.06576563e+01*z**4. - 1.16632511e+02*z**3. + 8.98104091e+01*z**2. - 4.16227347e+01*z + 2.73628494e+02
  ELSE
   temp = T10m
  ENDIF
 CASE("ant_neuma")
  IF(z.le.0.2631) THEN
   temp = 266.032+((265.-266.032)/0.2631)*z
  ELSE IF(z.le.1.857) THEN
   temp = 265.+((258.154-265.)/(1.857-0.2631))*(z-0.2631)
  ELSE IF(z.le.8.303) THEN
   temp = 258.154+((255.753-258.154)/(8.303-1.857))*(z-1.857)
  ELSE IF(z.le.15.) THEN
   temp = 255.753+((T10m-255.753)/(15.-8.303))*(z-8.303)
  ELSE
   temp = T10m
  ENDIF
 CASE("grl_aws05") ! KAN_L PROMICE thermistorr string after 2021 maintenance
  IF(z.le.1) THEN
   temp = -0.1 + Tkel
  ELSE IF(z.le.2) THEN
   temp = -0.2 + Tkel
  ELSE IF(z.le.8) THEN
   temp = (0.1714*z**2 - 2.271*z + 3.486) + Tkel
  ELSE
   temp = T10m
  ENDIF
  IF(temp.ge.273.1) temp = 273.1
  WRITE(*,*) z, temp
 CASE("grl_awskl") ! KAN_L PROMICE thermistorr string after 2021 maintenance
  IF(z.le.1) THEN
   temp = -0.1 + Tkel
  ELSE IF(z.le.2) THEN
   temp = -0.2 + Tkel
  ELSE IF(z.le.8) THEN
   temp = (0.1714*z**2 - 2.271*z + 3.486) + Tkel
  ELSE
   temp = T10m
  ENDIF
  IF(temp.ge.273.1) temp = 273.1
  WRITE(*,*) z, temp
 CASE("ant_domeA")! dome A fit to thermistor string data 1 Jan 2018
  IF(z.le.0.82) THEN
    temp = -48.56 + Tkel
  ELSE IF(z.le.10) THEN
    temp = -1.44e-05*z**6. + 0.0002271*z**5 + 0.008398*z**4 - 0.2618*z**3. + 2.703*z**2. -11.85*z - 40.52 + Tkel
     ! temp = -1.44e-05*z**6. + 0.0001649*z**5 + 0.009104*z**4 - 0.2365*z**3. + 2.164*z**2. - 8.351*z - 47.75 + Tkel
  ELSE
    temp =   T10m 
  ENDIF
 CASE DEFAULT	!others
  temp = T10m
  IF (il == 1) THEN
    IF (lcomment == 1) THEN
      WRITE(*,*) 'No snowtemp profile initialisation function available for this site'
      WRITE(*,*) 'Set to constant value T10m: ',T10m
    ENDIF
    WRITE(uo1,*) 'No snowtemp profile initialisation function available for this site'
    WRITE(uo1,*) 'Set to constant value T10m: ',T10m
  ENDIF
END SELECT

IF(temp .ge. 273.0) temp = 273.0
snowtemp = temp

END FUNCTION snowtemp

!===============================================================================
FUNCTION snowdens(z,id,il)
!===============================================================================
USE INPUT_EBM , ONLY : chstation , rhosninit , densice , densfirn , tpdens , lcomment , luseacc , tpdens
USE CONSTANTS_EBM, ONLY: densnow
USE SNOW_EBM , ONLY : rhosn
USE FILES , ONLY : uo1

IMPLICIT NONE
!input
REAL :: z
INTEGER :: id,il
!Local
REAL :: zz
REAL :: dens
REAL :: rhost
REAL :: rhocoef
REAL :: rhocoef1,rhocoef2
!output
REAL :: snowdens

!Assuming dry snow
!!!determine general profile based on either measurements or theory!

SELECT CASE(chstation)
 CASE("ant_aws01")	!AWS 1
  rhost = 316.3
  rhocoef1 = 86.821
  rhocoef2 = 0.46217
  dens=rhost+rhocoef1*(z**(rhocoef2))
 CASE("ant_aws02")	!AWS 2
  rhost = 257.03
  rhocoef1 = 131.86
  rhocoef2 = 0.36705
  dens=rhost+rhocoef1*(z**(rhocoef2))
 CASE("ant_aws03")	!AWS 3
  rhost = 304.79
  rhocoef = 25.079
  rhocoef1 = -0.98606
  rhocoef2 = 0.01747
  dens=rhost+rhocoef*z+rhocoef1*(z**2)+rhocoef2*(z**3)  
 CASE("ant_aws04")	!AWS 4
  rhost = 376.33
  rhocoef1 = 38.713
  rhocoef2 = 0.66926
  dens=rhost+rhocoef1*(z**(rhocoef2))
 CASE("ant_aws05")	!AWS 5
  rhost = 355.36
  rhocoef1 = 94.244
  rhocoef2 = 0.4196
  dens=rhost+rhocoef1*(z**(rhocoef2))
 CASE("ant_aws06")	!AWS 6
  rhost = 416.27
  rhocoef = 62.326
  dens = rhost + rhocoef*log(z)
 CASE("ant_aws08")	!AWS 8
  rhost = 176.12
  rhocoef1 = 147.08
  rhocoef2 = 0.35043
  dens=rhost+rhocoef1*(z**(rhocoef2))
 CASE("ant_aws09")	!AWS 9
  rhost = 274.34
  rhocoef1 = 72.747
  rhocoef2 = 0.44933
  dens = rhost + rhocoef1*(z**(rhocoef2))
 CASE("ant_aws00","ant_aws10")	!AWS 00 and B
  rhost = 225		!350.36			!450
  rhocoef1 = 147.08		!94.244		!aws5?
  rhocoef2 = 0.35043	!0.4196		!aws5?
  dens = rhost + rhocoef1*(z**(rhocoef2))
 CASE("ant_aws12")	!AWS 12 values of AWS 9
  rhost = 274.34
  rhocoef1 = 72.747
  rhocoef2 = 0.44933
  dens = rhost + rhocoef1*(z**(rhocoef2))
 CASE("ant_neuma") !Neumayer, based on model spin-up
  IF(z.lt.6) THEN
   dens = 300.+((263./6.)*z)
  ELSE
   dens = 514.+(((650.-563.)/(10.+(2./3.)))*z)
  ENDIF
 CASE("SURE_2007")	!Summit Greenland
  zz = z+0.01
  IF (zz.le.0.30) THEN
    dens = -1833.33 * zz**2 + 1100.0 * zz + 225.
  ELSE IF ((zz.gt.0.30).and.(zz.le.0.40)) THEN
    dens = 390.0
  ELSE IF ((zz.gt.0.40).and.(zz.le.0.60)) THEN
    dens = 610.0 - 550.0 * zz
  ELSE IF ((zz.gt.0.60).and.(zz.le.0.90)) THEN
    dens = 115.0 + 275.0 * zz
  ELSE
    dens = 390.0
  ENDIF
 CASE("ant_domeA") ! dome A fit to snow pit data Jan 2018
  IF (z.le.10) THEN
    dens =  -0.1292 * z**4 + 3.2804* z**3 -28.5735* z**2 + 102.9793*z + 287.4564 
  ELSE
    dens = 450.0
  ENDIF
 CASE DEFAULT	!others
  dens = densfirn +(rhosninit-densfirn)*exp(-z/10.)
!  IF (id == 1) dens = rhosninit 
!  IF (id > 1) dens = densfirn
  IF (il == 1 .and. tpdens > 0) THEN
    IF (lcomment == 1) THEN
      WRITE(*,*) 'No snowdens profile initialisation function available for this site'
      WRITE(*,*) 'Set to constant value rhosninit and densfirn: ',rhosninit,densfirn
    ENDIF
    WRITE(uo1,*) 'No snowdens profile initialisation function available for this site'
    WRITE(uo1,*) 'Set to constant value rhosninit and densfirn: ',rhosninit,densfirn
  ENDIF
END SELECT

IF (tpdens == 0 .and. il == 1) rhosn = dens

IF ((dens > densice) .or. (id == 0)) THEN
 dens = densice
ELSEIF(dens < densnow) THEN
 dens = densnow
ENDIF
snowdens = dens


END FUNCTION snowdens

!===============================================================================
FUNCTION snowtemp_STGL(z,zmin1,tmin1)
! T = coeff[1] + coeff[2]*(m snow depth) + coeff[3]*(m snow depth)^2 
!===============================================================================
USE CONSTANTS_EBM , ONLY : Tkel

IMPLICIT NONE
!input
REAL :: z,zmin1,tmin1
!Local
REAL :: temp
REAL :: coeff(3)
REAL :: tgrad1=0.575;      ! 0.43 vertical temperature gradient below top part till 10 m value tannual
REAL :: tgrad2=0.10;        ! 0.06 vertical temperature gradient from 10 m till temperate part at 30 m depth
REAL :: tdepth1=2.5;
REAL :: tdepth2=17.5;		!was 10
REAL :: tdepth3=25;		!was 30
!Output
REAL :: snowtemp_STGL

data coeff/-5.7912,-0.49947,0.41518/
   
!temperature*/
  temp = coeff(1) + coeff(2) * z + coeff(3) * z*z
  IF ((z <= tdepth2) .and. (z > tdepth1 )) THEN
     temp = tmin1 + (z-zmin1)*tgrad1
  ELSE IF (z > tdepth2 ) THEN
    temp = tmin1 + (z-zmin1)*tgrad2
  ELSEIF (z > tdepth3 ) THEN
    temp = 0.
  ENDIF

  IF (temp > 0.0) temp = 0

snowtemp_STGL = temp + Tkel

END FUNCTION snowtemp_STGL

!===============================================================================
FUNCTION snowdens_STGL(z,dz,zmin1,dzmin1,il,id)
! cm we = rhocoef1*(cm snow)^2 + rhocoef2*(cm snow) !
!   rhocoef1 = 0.0002, rhocoef2 = 0.3414. 
!===============================================================================
USE INPUT_EBM , ONLY : densice , tpdens

IMPLICIT NONE
!input
REAL :: z,dz,zmin1,dzmin1
INTEGER :: il,id
!Local
REAL :: dens , sum
REAL :: rhosn, rhocoef1, rhocoef2
!output
REAL :: snowdens_STGL

!Assuming dry snow

rhosn = 275.			
rhocoef1 = 0.0002			
rhocoef2 = 0.3414			

!!!determine general profile based on either measurements or theory!
dens = rhosn + rhocoef1*(z**(rhocoef2))

!density 
IF (il > 1) THEN
  sum = rhocoef1*((zmin1+0.5*dzmin1)*100.)*((zmin1+0.5*dzmin1)*100.) + &
&          rhocoef2*((zmin1+0.5*dzmin1)*100.)
ELSE
  sum = 0.
ENDIF
 
dens = ((rhocoef1*((z+0.5*dz)*100.)*((z+0.5*dz)*100.) + rhocoef2*((z+0.5*dz)*100.) - sum) / &
&         (100.*z) )*1000.

IF (tpdens == 0 .and. il == 1) rhosn = dens

IF ((dens > densice) .or. (id == 0)) dens = densice
snowdens_STGL = dens

END FUNCTION snowdens_STGL

!===============================================================================
SUBROUTINE RESETDENS
!===============================================================================
! Resets the sub-surface profiles of density and mass profiles
!===============================================================================
USE SNOW_EBM , ONLY : nl, z, dz, dens, mass , lid
USE INPUT_EBM , ONLY : chstation

IMPLICIT NONE
!Local
INTEGER :: il
REAL :: snowdens, snowdens_STGL

DO il = 1,nl
 IF (chstation.ne."STGL_1999") THEN 
   dens(il) = snowdens(z(il),lid(il),il)

 ELSEIF (chstation.eq."STGL_1999") THEN
   IF (il > 1) THEN
    dens(il) = snowdens_STGL(z(il),dz(il),z(il-1),dz(il-1),il,lid(il))
   ELSE
    dens(il) = snowdens_STGL(z(il),dz(il),0.,0.,il,lid(il))
   ENDIF
 ENDIF

 mass(il) = dens(il)*dz(il)
 
ENDDO

 CALL IRREDUCIBLE

END SUBROUTINE RESETDENS

!===============================================================================
