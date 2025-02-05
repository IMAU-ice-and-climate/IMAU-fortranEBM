!===============================================================================
!
! This file contains an albedo parameterisation along with its relevant subroutines
!
!===============================================================================
SUBROUTINE INITGRAINS
!===============================================================================
! Subroutine that initializes the grainsizes
! For now, only for Neumayer a spin has been performed to start with a realistic profile
! For all other stations, the initial grainsize is equal to the fresh snow grain size
!===============================================================================
	USE GLOBALS_EBM, ONLY:	radfresh
	USE SNOW_EBM, ONLY:		z, grainsize, nl, lid
	USE INPUT_EBM, ONLY:	densice, SSAfresh, chstation, radrefr
	
	IMPLICIT NONE
	
	!Local
	INTEGER	:: il
	
	IF(chstation.eq."ant_neuma" .and. SSAfresh.eq.60) THEN
	 radfresh = 0.00025
	ELSE
	 radfresh = 3.0/(SSAfresh*densice)
	ENDIF
	DO il=1,nl
	 IF(lid(il).eq.0) THEN
	  grainsize(il) = radrefr
	 ELSE
	  grainsize(il) = radfresh
	 ENDIF
	ENDDO
END SUBROUTINE INITGRAINS
!===============================================================================
SUBROUTINE NEWALBEDO
!===============================================================================
! Routine that contains the albedo parameterisation as described by Kuipers Munneke et al., 2011 (PKM)
! which is in turn based on Gardner & Sharp, 2010 (G&S)
!===============================================================================
	USE GLOBALS_EBM, ONLY: szenith, sbuf, albedo, cloud
	USE INPUT_EBM, ONLY: albice, densice, albmax, albmin, chstation, soot
	USE SNOW_EBM, ONLY: z, dz, grainsize, lid
	IMPLICIT NONE
	
	!Local
	INTEGER	:: il
	REAL	:: layeralbedo(100)
	REAL	:: coszenith, dalbzenith, dalbclear, dalbtau, x, tempzenith, dalbcarbon, tempsum
	REAL	:: basealb, factor, tau
	
	CALL METAMORPHISM
	
	factor = (30./densice)**(-0.07)
	
	coszenith = COS(szenith)
!	tau = 1.14*(EXP(3.2*cloud)-1.0) !Kuipers Munneke et al., 2011, International Journal of Climatology, Eq .3
!	tau = 8.36*(EXP(1.548*cloud)-1.0)
	
	IF(chstation.eq."alp_aws02") THEN
	 
	 tau = 0.0535*(EXP(6.812*cloud)-1.0)
	 
	ELSEIF(chstation.eq."ant_aws04") THEN
	 
	 tau = 6.541*(EXP(2.065*cloud)-1.0)
	 
	ELSE	!These are actually Neumayer values
!	 tau = 4.58*(EXP(2.054*cloud)-1.0) !Lower bound
!	 tau = 6.228*(EXP(2.359*cloud)-1.0) !Upper bound
	 tau = 5.404*(EXP(2.207*cloud)-1.0)
	ENDIF
	
	!Correction for zenith angle (G&S)
	IF(coszenith.lt.1E-9) THEN
	 x = 0.
	ELSE
	 x = MIN(SQRT(tau/(3.0*coszenith)),1.0) !Definition of x in G&S Eq. 10 / PKM Eq. 6
	ENDIF
	tempzenith = 0.53*((1-0.64*x-(1-x)*coszenith)**1.2) !Part of PKM Eq. 6

	!Correction for clear sky optical thickness (PKM Eq. 11, converted to Pa)
	dalbclear = MIN(0.0,0.03247*LOG(sbuf(5)/153880.0)) !sbuf(5) = pressure
	
	il=1
	DO WHILE((z(il)+0.5*dz(il)).le.0.5) !We go only 10 cm deep
	 IF(lid(il) .eq. 0) THEN
	  layeralbedo(il) = albice
	 ELSE
	  basealb = 1.48 - factor*(grainsize(il)**0.07) !PKM Eq. 5
	  
	  !Correction due to loading by light-absorbing carbon (G&S Eq. 8, modified for grain size in metres)
	  dalbcarbon = MAX(0.04-basealb,(-(soot**0.55))/(0.16+SQRT((0.012*densice)/grainsize(il))+&
	  &1.8*(soot**0.6)*((densice/30.)**0.25)*(grainsize(il)**0.25)))
	  
	  tempsum = basealb + dalbcarbon
	  
	  dalbzenith = basealb*(1.-tempsum)*tempzenith !Rest of PKM Eq. 6
	  
	  dalbtau = (0.1*tau*(tempsum**1.3))/((1.0+1.5*tau)**basealb) !PKM Eq. 8
	  
	  layeralbedo(il) = basealb + dalbzenith + dalbtau + dalbclear !PKM Eq. 4 + Eq. 11
	 ENDIF
	 IF(il==1) THEN
	  albedo = layeralbedo(il)
	 ELSE
	  albedo = albedo + ((layeralbedo(il)-layeralbedo(il-1))*EXP(-(z(il)+0.5*dz(il))/0.01))
	 ENDIF
	 il = il + 1
	END DO
	IF(albedo.gt.albmax) THEN
	 albedo = albmax
	ELSEIF(albedo.lt.albmin) THEN
	 albedo = albmin
	ENDIF
END SUBROUTINE NEWALBEDO
!===============================================================================
SUBROUTINE METAMORPHISM
!===============================================================================
! Routine that calculates the metamorphism of grainsizes due to old snow, refrozen snow and new snow
!===============================================================================
	USE CONSTANTS_EBM, ONLY: pi
	USE GLOBALS_EBM, ONLY: radfresh
	USE INPUT_EBM, ONLY: tstep, radrefr, lcomment, chstation, SSAfresh
	USE SNOW_EBM, ONLY: nlsnow, mass, water, ice, freshfrac, z, dz, refrfrac, grainsize
	USE PKM_EBM, ONLY: drdry
	USE FILES, ONLY: uo1
	
	IMPLICIT NONE
	
	!Local
	INTEGER	:: il, nlsnowdo
	REAL	:: drwet, watcont
	REAL	:: oldfrac
	
	CALL DRYSNOW
	
	DO il=1,nlsnow
	 watcont = water(il)/(mass(il)+water(il))
	 IF(watcont.gt.1) THEN
	  WRITE(*,*) "Water content greater than one..."
	  STOP 40
	 END IF
	 drwet = (4.22E-13*(watcont**3.))/(4.0*pi*grainsize(il)**2)*tstep !PKM Eq. 2
	 
	 oldfrac = 1. - refrfrac(il) - freshfrac(il)
	 IF(oldfrac.gt.1.0.or.refrfrac(il).gt.1.0.or.freshfrac(il).gt.1.0) THEN
	  WRITE(*,*) "Some fraction is greater than 1"
!	  WRITE(*,*) "ii	jj	il	oldfrac	refrfrac(il)	freshfrac(il)"
!	  WRITE(*,*) ii, jj, il, oldfrac, refrfrac(il), freshfrac(il)
	  WRITE(*,*) "oldfrac	refrfrac(il)	freshfrac(il)"
	  WRITE(*,*) oldfrac, refrfrac(il), freshfrac(il)
	  STOP 41
	 ELSE IF(oldfrac.lt.0.0.or.refrfrac(il).lt.0.0.or.freshfrac(il).lt.0.0) THEN
	  WRITE(*,*) "Some fraction is smaller than 0"
!	  WRITE(*,*) "ii	jj	il	oldfrac	refrfrac(il)	freshfrac(il)"
!	  WRITE(*,*) ii, jj, il, oldfrac, refrfrac(il), freshfrac(il)
	  WRITE(*,*) "oldfrac	refrfrac(il)	freshfrac(il)"
	  WRITE(*,*) oldfrac, refrfrac(il), freshfrac(il)
	  STOP 42
	 END IF
	 
	 grainsize(il) = (grainsize(il)+drdry(il)+drwet)*oldfrac+refrfrac(il)*radrefr+freshfrac(il)*radfresh
	 
	 !Reset fractions of refrozen and fresh snow
	 refrfrac(il) = 0.0
	 freshfrac(il) = 0.0
	 
	 IF(grainsize(il).lt.radfresh) THEN
	  IF((grainsize(il)-radfresh) .le. (-1.0E-5)) THEN
	   WRITE(*,*) "Grainsize too small!"
	   STOP 43
	  ELSE
	   IF(lcomment.eq.1) THEN
	    WRITE(*,*) "Watch out, grainsize slightly smaller than radfresh:"
	    WRITE(*,*) "Difference ", (radfresh-grainsize(il))
	   ENDIF
	   WRITE(uo1,*) "Watch out, grainsize slightly smaller than radfresh:"
	   WRITE(uo1,*) "Difference ", (radfresh-grainsize(il))
	  ENDIF
	 ENDIF
	END DO
END SUBROUTINE METAMORPHISM
!===============================================================================
SUBROUTINE DRYSNOW
!===============================================================================
! Routine that determines the dry snow metamorphism
! It uses the look-up tables as initialized in inittables.f
!===============================================================================
	USE PKM_EBM
	USE SNOW_EBM, ONLY: nlsnow, dens, temp, dtdz, grainsize
	USE INPUT_EBM, ONLY: tstep
	USE GLOBALS_EBM, ONLY: radfresh
	USE CONSTANTS_EBM, ONLY: nlmax
	
	IMPLICIT NONE
	
	!Local
	REAL	:: TAU, KAP, DR0
	REAL	:: absdtdz(nlmax)
	REAL	:: tempdens, temptemp, tempdtdz
	REAL	:: fracdens, fractemp, fracdtdz
	INTEGER	:: idens, itemp, idtdz, il
	
	absdtdz = ABS(dtdz)
	
	DO il=1,nlsnow
	 IF(dens(il).lt.DENSVals(1)) THEN
	  idens = 1
	  tempdens = DENSVals(1)
	 ELSE IF(dens(il).ge.DENSVals(8)) THEN
	  idens = 7
	  tempdens = DENSVals(8)
	 ELSE
	  idens = 1
	  DO WHILE(DENSVals(idens).le.dens(il))
	   idens = idens + 1
	  END DO
	  idens = idens - 1
	 END IF
	 
	 IF(temp(il).lt.TVals(1)) THEN
	  itemp = 1
	  temptemp = TVals(1)
	 ELSE IF(temp(il).ge.TVals(11)) THEN
	  itemp = 10
	  temptemp = TVals(11)
	 ELSE
	  itemp = 1
	  DO WHILE(TVals(itemp).le.temp(il))
	   itemp = itemp + 1
	  END DO
	  itemp = itemp - 1
	 END IF
	 
	 IF(absdtdz(il).lt.DTDZVals(1)) THEN
	  idtdz = 1
	  tempdtdz = DTDZVals(1)
	 ELSE IF(absdtdz(il).ge.DTDZVals(31)) THEN
	  idtdz = 30
	  tempdtdz = DTDZVals(31)
	 ELSE
	  idtdz = 1
	  DO WHILE(DTDZVals(idtdz).le.absdtdz(il))
	   idtdz = idtdz + 1
	  END DO
	  idtdz = idtdz - 1
	 END IF
	 
	 fracdens = (dens(il)-DENSVals(idens))/(DENSVals(idens+1)-DENSVals(idens))
	 fractemp = (temp(il)-TVals(itemp))/(TVals(itemp+1)-TVals(itemp))
	 fracdtdz = (absdtdz(il)-DTDZVals(idtdz))/(DTDZVals(idtdz+1)-DTDZVals(idtdz))
	 
	 !Now retrieve the values for tau, kap and dr0 from the look-up tables
	 TAU = 0.0
	 KAP = 0.0
	 DR0 = 0.0
	 
	 TAU = TAU + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * TAUMAT(itemp  ,idtdz  ,idens  )
	 TAU = TAU + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & fracdtdz           * TAUMAT(itemp  ,idtdz+1,idens)
	 TAU = TAU + (1.0 - fracdens) * fractemp           * &
	 & (1.0 - fracdtdz) * TAUMAT(itemp+1,idtdz  ,idens  )
	 TAU = TAU + (1.0 - fracdens) * fractemp           * &
	 & fracdtdz           * TAUMAT(itemp+1,idtdz+1,idens  )
	 TAU = TAU + fracdens           * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * TAUMAT(itemp  ,idtdz  ,idens+1)
	 TAU = TAU + fracdens           * (1.0 - fractemp) * &
	 & fracdtdz           * TAUMAT(itemp  ,idtdz+1,idens+1)
	 TAU = TAU + fracdens           * fractemp           * &
	 & (1.0 - fracdtdz) * TAUMAT(itemp+1,idtdz  ,idens+1)
	 TAU = TAU + fracdens           * fractemp           * &
	 & fracdtdz           * TAUMAT(itemp+1,idtdz+1,idens+1)
	 
	 KAP = KAP + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * KAPMAT(itemp  ,idtdz  ,idens  )
	 KAP = KAP + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & fracdtdz           * KAPMAT(itemp  ,idtdz+1,idens)
	 KAP = KAP + (1.0 - fracdens) * fractemp           * &
	 & (1.0 - fracdtdz) * KAPMAT(itemp+1,idtdz  ,idens  )
	 KAP = KAP + (1.0 - fracdens) * fractemp           * &
	 & fracdtdz           * KAPMAT(itemp+1,idtdz+1,idens  )
	 KAP = KAP + fracdens           * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * KAPMAT(itemp  ,idtdz  ,idens+1)
	 KAP = KAP + fracdens           * (1.0 - fractemp) * &
	 & fracdtdz           * KAPMAT(itemp  ,idtdz+1,idens+1)
	 KAP = KAP + fracdens           * fractemp           * &
	 & (1.0 - fracdtdz) * KAPMAT(itemp+1,idtdz  ,idens+1)
	 KAP = KAP + fracdens           * fractemp           * &
	 & fracdtdz           * KAPMAT(itemp+1,idtdz+1,idens+1)
	 
	 DR0 = DR0 + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * DR0MAT(itemp  ,idtdz  ,idens  )
	 DR0 = DR0 + (1.0 - fracdens) * (1.0 - fractemp) * &
	 & fracdtdz           * DR0MAT(itemp  ,idtdz+1,idens)
	 DR0 = DR0 + (1.0 - fracdens) * fractemp           * &
	 & (1.0 - fracdtdz) * DR0MAT(itemp+1,idtdz  ,idens  )
	 DR0 = DR0 + (1.0 - fracdens) * fractemp           * &
	 & fracdtdz           * DR0MAT(itemp+1,idtdz+1,idens  )
	 DR0 = DR0 + fracdens           * (1.0 - fractemp) * &
	 & (1.0 - fracdtdz) * DR0MAT(itemp  ,idtdz  ,idens+1)
	 DR0 = DR0 + fracdens           * (1.0 - fractemp) * &
	 & fracdtdz           * DR0MAT(itemp  ,idtdz+1,idens+1)
	 DR0 = DR0 + fracdens           * fractemp           * &
	 & (1.0 - fracdtdz) * DR0MAT(itemp+1,idtdz  ,idens+1)
	 DR0 = DR0 + fracdens           * fractemp           * &
	 & fracdtdz           * DR0MAT(itemp+1,idtdz+1,idens+1)
	 
	 IF(KAP.le.0.0.or.TAU.le.0.0) THEN
	  drdry(il) = 0.0
	 ELSE
	  IF(ABS(grainsize(il) - radfresh) .lt. 1.0E-5) THEN
       drdry(il) = (DR0*tstep*(1E-6)*((TAU/(TAU+1.0))**(1./KAP)))/3600.
      ELSE
       drdry(il) = (DR0*tstep*(1E-6)*((TAU/(TAU+1E6*(grainsize(il)-radfresh)))**(1./KAP)))/3600.
      ENDIF
	 END IF
	ENDDO
END SUBROUTINE DRYSNOW
!===============================================================================
