      subroutine fcraxn(nam)
c
c  version : 25.08.97 16:30
c
c======================================================================
c*** Input of auxiliary data for Carre
c======================================================================
      implicit none
#include <CARRETYPES.F>
#include <FCRCOM.F>
      character*8 nam
c
      integer nnm
      parameter (nnm=7)
      real(kind=R4) rdummy, rarray(ntrgx)
      character*8 name, nms(nnm)
      integer i
      logical streql ! string equal check
      external streql
c
c*** List of the valid input keywords
      data nms /
     1    'repart  ','nrelax  ','relax   ','pasmin  ','rlcept  ',
     2    'tgarde  ','xpttol  '/
c======================================================================
c
      call locase(nam,name,8) ! lower the variables
      do i=1,nnm
        if(streql(name,nms(i))) go to (10,20,30,40,50,60,70),i
      end do
      call fcrtrn(name) ! get the carre target input data
      return
c
 10   call resimi(repart) ! repart:
      return
 20   call resimi(nrelax) ! nrelax
      return
 30   call resime(rdummy)
      relax = real(rdummy,R8)
      return
 40   call resime(rdummy)
      pasmin = real(rdummy,R8) ! pasmin
      return
 50   call resime(rdummy)
      rlcept = real(rdummy,R8) ! rlcept
      return
 60   call rearre(rarray,ntrgx,ntrg)
      tgarde(1:ntrg) = real(rarray(1:ntrg),R8) ! guard length in front of targets
      return
 70   call resime(rdummy)
      xpttol = real(rdummy,R8) ! tolerance parameter for distinguishing the x-points
      return
c======================================================================
      end
