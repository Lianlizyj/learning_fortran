      subroutine cadre
c
c  version : 05.04.97 15:53
c
c======================================================================
      implicit none
c  on definit les dimensions du cadre dans lequel qui sert a tracer les
c  graphiques.
!  we define the dimensions of the frame in which the graphs are drawn.
c
c  variables en common
#include <PERIM.F>
! declaring the real variables(x1a...), and set the named common variables perimtr, constaining x1a,x1b ...
c======================================================================
c  calculs
c
      x1a=0.15
      x1b=0.95
      x2a=0.15
      x2b=0.95
c
      return
      end
