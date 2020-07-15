      program fcrr
c
c  version : 05.02.99 13:41
c
c======================================================================
c*** Converts the dg data into the input files for Carre
c======================================================================
c      implicit none
#include <CARRETYPES.F>  ! declaring three parameter for futher use
#include <FCRCOM.F> ! declarin the basic variables and named constant
#include <impcon.inc> ! declaring real and logical variables
      logical ex ! assign value to variables, which specify the exist status
      character*14 chtrg,chstr,chequ,chaux,chdgo,
     ,             chcfld,chcequ,chcstr,chccrr,chcslp
      data chtrg ,  chstr ,  chequ ,  chaux , chdgo  /
     /   'dg.trg','dg.str','dg.equ','dg.aux','dg.dgo'/,
     ,     chcfld    ,  chcequ   ,     chcstr    ,  chccrr   /
     /   'btor.dat'  ,'rzpsi.dat','structure.dat','carre.dat'/,
     ,      chcslp   /
     /   'selptx.inf'/
     ! assign filename to ch*(i.e. chtrg)
      external fcraxn,fcrtrn,fcrdgi ! declaring the external procedures
c======================================================================
c
      inquire(file=chtrg,exist=ex) ! get the file status, if file exist, the value of
      ! ex is .ture.
      if(.not.ex) then ! if the file don't exist, display the error message
        write(*,*) 'fcrr: the file ',chtrg,' must be present!'
      end if
      inquire(file=chstr,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chstr,' must be present!'
      end if
      inquire(file=chequ,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chequ,' must be present!'
      end if
c
      immed=.false. ! declearing in impcon.F
      keylen=8 ! declearing in impcon.F
      mimp=1 ! ditto
      lena=8 ! ditto
c
c*** Read the data
c
      ! test for the first read procedure
      open(1,file=chtrg) ! open the trg file and assign the unit value to 1
      call fcrchktp(1) ! callback the fcrchktp subroutine,
      call import(fcrtrn) ! fcrchktp belong to fcrtrn, use entry to declaring fcrchktp
      close(1) ! close the file
      open(1,file=chstr) ! read the structure data, but it don't output the x,y position
      call fcrstri(1)
      close(1)
      open(1,file=chdgo) ! read the XP OP position，
      ! set the trg_spcf: trg_spcf1 and trg_spcf2  specify the pfr and sol, materials, temperature and tgtgrd both set to 0.2
      ! tgtgrd: descripted above
      ! lm_cnfg:1, lm_pntrt:0.3, lm_grcln:0.1, only appreance in limiter configuration
      call import(fcrdgi)
      close(1)
      open(1,file=chequ)! read the equ file
      call fcrfldi(1)
      close(1)
      inquire(file=chaux,exist=ex)
      if(ex) then
        open(1,file=chaux)
        call import(fcraxn) ! read dg.aux
        close(1)
      end if
c
c*** ... make the necessary transformations
c
      call fcrprp ! data from dg to carre
c
c*** ... and produce the files to be read by carre
c
      call fcrequo(chcequ) ! rzpsi
      call fcrstro(chcstr) ! structure.dat
      call fcrcrro(chccrr) ! carre.dat
      call fcrfldo(chcfld) ! btor.dat
      call fcrslpo(chcslp) ! selptx.inf
c======================================================================
      end
