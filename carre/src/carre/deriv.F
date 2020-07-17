
************************************************************************
      SUBROUTINE DERIV(nmax, r, f, fp, n)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..   Cette sous-routine calcule la derivee fp en chaque point r d'un
*  vecteur f.
! calculate the difference of every point
*  arguments
      INTEGER nmax, n
      REAL(kind=R8) r(n), f(nmax), fp(nmax)

*  variables locales
      INTEGER i
      REAL(kind=R8) r2,r3,f2,f3

c=========================
c.. r  : tableau de la coordonnee des points.
c.. f  : tableau de la valeur de la fonction en chacun de ces points.
c.. n  : nombre de points dans chaque tableau.
c.. fp : tableau de la valeur de la fonction derivee par rapport a r
c        en chacun de ces points.
c.. r2 : distance entre les 2 prermiers (ou derniers) points.
c.. r3 : distance entre les 3 prermiers (ou derniers) points.
c.. f2 : difference de psi entre les 2 premiers (ou derniers) points.
c.. f3 : difference de psi entre les 3 premiers (ou derniers) points.
c=========================

*..Debut du calcul
! ================== translate from french to english by google translater
! r: table of the coordinate of points.
! f: Table of the value of the function in each of these points.
! n: number of points in each table.
! fp: table of the value of the derived function in relation to r in each of these points.
! r2: distance between the first two (or last) points.
! r3:distance between 3first (or last) points.
! f2: psi difference between the first 2 (or last) points.
! f3: psi difference between the first 3 (or last) points.

! ================== similar with fx=gradient(f,x) ========================

*..Calcul des derivees a partir de la valeur de la fonction de chacun
*  des deux points adjacents.

      DO 10 i=2, n-1

         fp(i) = ((f(i+1)-f(i))*(r(i)-r(i-1))/(r(i+1)-r(i))
     .  + (f(i)-f(i-1))*(r(i+1)-r(i))/(r(i)-r(i-1))) /(r(i+1)-r(i-1))

   10 CONTINUE

*..Determination de la derivee du premie```r et du dernier point.
! the derive in the first and last are different between gradient
      r2=r(2)-r(1)
      r3=r(3)-r(1)
      f2=f(2)-f(1)
      f3=f(3)-f(1)
      fp(1)=(f2*r3*r3-f3*r2*r2)/(r2*r3*(r3-r2))
      r2=r(n-1)-r(n)
      r3=r(n-2)-r(n)
      f2=f(n-1)-f(n)
      f3=f(n-2)-f(n)
      fp(n)=(f2*r3*r3-f3*r2*r2)/(r2*r3*(r3-r2))

      RETURN
      END
