      subroutine fcrfldi(lun)
c
c  version : 16.09.2000 00:15
c
c======================================================================
c*** Read the field data from dg equilibrium file or from a separate
c*** file 'field.dg'
c======================================================================
      implicit none
      integer lun
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i
      real(kind=R8) btf,rtf
      logical ex
c======================================================================
c ! initinalize the rbtor, btf and rtf
      rbtor=0.0_R8
      btf=0.0_R8
      rtf=0.0_R8
      inquire(file='field.dg',exist=ex) ! check whether the "field.dg" exist
      if(ex) then ! if exist
        open(lun+2,file='field.dg') ! open the field.dg, set the uint to lunt+2
      end if
      call rdeqdg(lun,ngpr,ngpz,i,nr,nz,btf,rtf,rgr,zgr,pfm) ! call read procedure to read the equ file
      if(i.ne.0) then ! if i .ne.0, display the error message
        write(*,*) 'fcrfldi: error reading the magnetic data. ',
     ,                                                  'Error code ',i
        stop ! stop the program
      end if
      if(ex) close(lun+2) ! close the field.dg files
      rbtor=rtf*btf ! rbtot is specified by the product of rtf and btf
      if(rbtor.ne.0.0_R8) return                                       ! DPC
c======================================================================
      write(*,*) 'fcrfldi: no data on the toroidal field found.' ! if rbtor is zero, no data on the toroidal field found
      write(*,*) 'These data should be either in the equilibrium file', ! and the variable is necessary.
     ,            ' or in the separate file "field.dg" in the current', ! display the error message and stop the program
     ,                                                     ' directory'
      stop
c======================================================================
      end
