*
* $Id: mnvert.F,v 1.1.1.1 2000/06/08 11:19:20 andras Exp $
*
* $Log: mnvert.F,v $
* Revision 1.1.1.1  2000/06/08 11:19:20  andras
* import of MINUIT from CERNlib 2000
*
* Revision 1.2  1996/03/15 18:02:54  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNVERT(A,L,M,N,IFAIL)
#include "minuit/d506dp.inc"
CC        inverts a symmetric matrix.   matrix is first scaled to
CC        have all ones on the diagonal (equivalent to change of units)
CC        but no pivoting is done since matrix is positive-definite.
CC
#include "minuit/d506cm.inc"
      DIMENSION A(L,M) ,PP(MNI), Q(MNI),  S(MNI)
      IFAIL=0
      IF (N .LT. 1)  GO TO 100
      IF (N .GT. MAXINT)  GO TO 100
C                   scale matrix by sqrt of diag elements
      DO 8  I=1,N
      SI = A(I,I)
      IF (SI <= ZERO) GO TO 100 
      S(I) = 1.0/SQRT(SI)
    8 CONTINUE
      DO 20 I= 1, N
      DO 19 J= 1, N
      A(I,J) = A(I,J) *S(I)*S(J)
   19 CONTINUE
   20 CONTINUE
C                                        . . . start main loop . . . .
      DO 65 I=1,N
      K = I
C                   preparation for elimination step1
      IF (A(K,K) .EQ. ZERO)  GO TO 100
      Q(K)=1./A(K,K)
      PP(K) = 1.0
      A(K,K)=0.0
      KP1=K+1
      KM1=K-1
      IF(KM1 < ZERO) GO TO 100
      IF(KM1 > ZERO) THEN
      DO 49 J=1,KM1
      PP(J)=A(J,K)
      Q(J)=A(J,K)*Q(K)
      A(J,K)=0.
   49 CONTINUE
      END IF
   50 IF(K-N > ZERO) THEN
         GO TO 100
      END IF
      IF (K-N < ZERO) THEN 
      DO 59 J=KP1,N
      PP(J)=A(K,J)
      Q(J)=-A(K,J)*Q(K)
      A(K,J)=0.0
   59 CONTINUE
      END IF
C                   elimination proper
   60 DO 64 J=1,N
      DO 63 K=J,N
      A(J,K)=A(J,K)+PP(J)*Q(K)
   63 CONTINUE
   64 CONTINUE
   65 CONTINUE
C                   elements of left diagonal and unscaling
      DO 70 J= 1, N
      DO 69 K= 1, J
      A(K,J) = A(K,J) *S(K)*S(J)
      A(J,K) = A(K,J)
   69 CONTINUE
   70 CONTINUE
      RETURN
C                   failure return
  100 IFAIL=1
      RETURN
      END
