!===============================================================================
!
! Routines related to the calculation of the surface temperature
!
!===============================================================================
FUNCTION surft(t1,t2,dz1,dz2,z1,z2)
!===============================================================================
! T0 calculation by means of linear extrapolation of two upper layers
! temperature
!===============================================================================
USE CONSTANTS_EBM , ONLY : tinterv, taccur, Tkel

IMPLICIT NONE

REAL :: surft
REAL,INTENT(IN) :: t1, t2, dz1, dz2, z1, z2
REAL :: tgrad,tsurf

!tgrad = (t2-t1)/(dz1+dz2)
tgrad = (t2-t1)/(z2-z1)

!tsurf = t1 - tgrad*dz1
tsurf = t1 - tgrad*z1

! Surface temperature of melting snow cannot exceed 0.0 C or 273.16 K
IF (tsurf.gt.Tkel) tsurf = Tkel

surft=tsurf

END FUNCTION surft

!===============================================================================
SUBROUTINE TSKIN
! 1=year, 2=day, 3=hour, 4=T, 5=p, 6=WS, 7=Sin, 8=Sout, 9=albedo, 10=running mean albedo, 
! 11=qi, 12=Lin, 13=Lout, 14=z0m, 15=zt, 16=zm , 17=serie(running mean), 18=precip,  
!===============================================================================
USE GLOBALS_EBM , ONLY : t0 , q0 , sbuf, source, densair 
USE CONSTANTS_EBM , ONLY : tinterv, taccur, Tkel, rd
USE INPUT_EBM , ONLY : lcomment
USE FILES , ONLY : uo1

IMPLICIT NONE
  
INTEGER :: iter,itermax
REAL :: t01,t02,dt0,t0old
REAL :: t0old1,dt01
REAL :: bisection, energybalance, spechum, falsepos

 CALL TURBHF(0)

t01 = t0-tinterv
t02 = t0+tinterv
dt0 = 2.*taccur
dt01 = 2.*taccur
t0old=t01

iter = 0
itermax = 40

source = 0.

DO WHILE ((dt0.gt.taccur).and.(dt01>0.))
 iter = iter + 1

 t0old1 = t0old
 t0old = t0
 
 IF(energybalance(Tkel) .ge. 0.0) THEN
  t0 = Tkel
 ELSE
  IF(t02 .gt. 273.2) t02 = 273.2 !Resulting skin temperature will certainly not exceed this value
!  t0 = bisection(t01,t02,taccur,yr,ii)
  t0 = falsepos(t01,t02,taccur)
 ENDIF
 
 IF (t0.ge.Tkel) t0 = Tkel
 q0 = spechum(t0, 1.0, sbuf(5))
 
 CALL TURBHF(iter)

! source = energybalance(t0)
 IF (t0.ge.Tkel) source=energybalance(Tkel)
 IF (source.lt.0.) source=0.				
				
 dt0=ABS(t0-t0old)			! TEST TEST TEST TEST
 dt01 = ABS(t0-t0old1)

 IF (iter.ge.itermax) THEN		! no solution found
  IF (lcomment == 1) THEN
    WRITE(*,'(/,A,i3,A,/,I5,6f16.8,/)') 'TSKIN more than',itermax,' iterations necessary',&
&       iter,taccur,dt0,t0,t0old,source
!  STOP
  ENDIF
  WRITE(uo1,'(/,A,i3,A,/,I5,5f16.8,/)') 'TSKIN more than',itermax,' iterations necessary',&
&       iter,taccur,dt0,t0,t0old,source
  t0=0.5*(t0+t0old)
  dt0=0.
 ENDIF

ENDDO

 IF ((dt0.gt.taccur).and.(dt01==0.).and.(iter > itermax*0.5)) THEN		! no solution found
!  IF (lcomment == 1) THEN
!    WRITE(*,'(/,A,/,I5,5f16.8,/)') 'TSKIN no solution found, varies between ',&
!&       iter,taccur,dt0,t0,t0old
!  ENDIF
    WRITE(uo1,'(/,A,/,I5,5f16.8,/)') 'TSKIN no solution found, varies between ',&
&       iter,taccur,dt0,t0,t0old
  t0=0.5*(t0+t0old)
 ENDIF

END SUBROUTINE TSKIN

!===============================================================================
FUNCTION bisection(x1,x2,x_acc,yr,ii)
!===============================================================================
! This function determines the surface temperature required to close the energy balance
!===============================================================================
USE INPUT_EBM, ONLY: lcomment

IMPLICIT NONE
	
INTEGER,PARAMETER :: jmax=40
INTEGER :: j

INTEGER :: yr,ii

REAL :: bisection,x1,x2,x_acc
REAL :: dx,f,fmid,xmid,rtb,energybalance

fmid=energybalance(x2)
f=energybalance(x1)
dx=abs(x1-x2)

DO WHILE((f*fmid).ge.0.0) !If we don't capture the root
 IF(fmid.ge.0) THEN !If we're left of the root
  x1 = x2
  x2 = x2 + dx
 ELSE !Right
  x2 = x1
  x1 = x1 - dx
 ENDIF
 fmid = energybalance(x2)
 f = energybalance(x1)
ENDDO

rtb = x1
j=1

DO WHILE((j.lt.jmax) .and. (abs(dx).gt.x_acc .or. abs(fmid).gt.0.00001))
 j=j+1
 dx = dx * 0.5
 xmid = rtb + dx
 fmid = energybalance(xmid)
 
 IF(fmid.ge.0) rtb = xmid
END DO
	
IF (j==jmax .and. lcomment == 1) WRITE (*,*) 'Warning: maximum number of bisections!',yr,ii,xmid,fmid,x1,x2

bisection = rtb

END FUNCTION bisection

!===============================================================================
FUNCTION falsepos(a,b,acc)
!===============================================================================
! This function is similar to the one above but it uses the "false position method"
!===============================================================================
	IMPLICIT NONE
	
	REAL				:: a, b, c, acc
	REAL				:: falsepos, energybalance
	REAL				:: fa, fb, fc, dx, rtb
	INTEGER				:: j
	INTEGER, PARAMETER	:: jmax = 40
	REAL, PARAMETER		:: ebacc = 0.001 !Accuracy of energy balance rest term
	!	b>a, so f(b)<f(a) (because f(x) is monotonically descending)
	fa = energybalance(a)
	fb = energybalance(b)
	dx = abs(a-b)
	
	DO WHILE((fa*fb).ge.0.0) !If we don't capture the root
	 IF(fb.ge.0) THEN !If we're left of the root
	  a = b
	  b = b + dx
	 ELSE !Right
	  b = a
	  a = a - dx
	 ENDIF
	 fa = energybalance(a)
	 fb = energybalance(b)
	ENDDO
	
	j = 1
	c = b - fb*((b-a)/(fb-fa))
	fc = energybalance(c)
	
	DO WHILE(j.lt.jmax .and. dx .gt. acc .and. abs(fc).gt.ebacc)
	 IF(fa*fc .ge. 0.0) THEN
	  a = c
	  fa = fc
	 ELSE
	  b = c
	  fb = fc
	 ENDIF
	 dx = abs(a-b)
	 
	 j = j + 1
	 c = b - fb*((b-a)/(fb-fa))
	 fc = energybalance(c)
	ENDDO
	
	IF(j.eq.jmax) THEN
	 WRITE(*,'(A,I2,A)') "Maximum (>", jmax, ") number of false position iterations!"
	 STOP 70
	ENDIF
	
	falsepos = c
	
END FUNCTION falsepos

!===============================================================================
