C******* -SUBROUTINE ADDCAN- *******************************************
C----
C---- IF NO SUITABLE CANDIDATE WAS FOUND POINT C IS ADDED TO THE
C---- CANDIDATE LIST

      SUBROUTINE ADDCAN
      use cpoin
      use ccandi
      use cdelta
      IMPLICIT NONE

C---- LOCKAL VARIABLES
C---- I : LOOP INDEX
C---- XK1 : COORDINATES OF THE FIRST POINT OF THE CANDIDATE LIST
C---- AK1 : DISTANCE BETWEEN POINT XA AND POINT XK1
C---- BK1 : DISTANCE BETWEEN POINT XB AND POINT XK1
C---- CK1 : DISTANCE BETWEEN POINT XC AND POINT XK1
C---- N : MAXIMAL ALLOWED DISTANCE BETWEEN XA AND XK1 OR XB AND XK1
C----     1.5*BASE LENGTH OF THE TRIANGLE
C---- DIST : FUNCTION FOR CALCULATING THE DISTANCE BETWEEN TWO POINTS
      INTEGER I
      DOUBLE PRECISION XK1(2),AK1,BK1,CK1,N,DIST

      IF (NCANDI .GT. 0) THEN
        XK1(1) = X(ICANDI(1))
        XK1(2) = Y(ICANDI(1))
        AK1 = DIST(XA(1),XA(2),XK1(1),XK1(2))
        BK1 = DIST(XB(1),XB(2),XK1(1),XK1(2))
        CK1 = DIST(XC(1),XC(2),XK1(1),XK1(2))
        N   = 1.5*DELTA2

        IF ((AK1.GT.N).OR.(BK1.GT.N).OR.(CK1.GT.DELTA2)) THEN
          IF (NCANDI+1 .GT. size(icandi)) THEN
             call realloc_ccandi('icandi',10)
          ENDIF
          DO I=NCANDI,1,-1
            ICANDI(I+1) = ICANDI(I)
          ENDDO
          ICANDI(1) = IC
          NCANDI = NCANDI + 1
        ENDIF
      ELSE
        ICANDI(1) = IC
        NCANDI = 1
      ENDIF

      END