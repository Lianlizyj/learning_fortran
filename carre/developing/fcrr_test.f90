module base

contains
  logical function streql (str0, str1) result(s_eq)
  implicit none
  character str0*(*), str1*(*)
!*     ------------------------------------------------------------------
!*     STREQL checks for equality between two strings, ignoring case
!*     shift and trailing blanks.
!*     The text of this routine is case-sensitive.
!*     ------------------------------------------------------------------
  integer len0, len1, k
  character ch0, ch1
  intrinsic len, ichar
  logical upcase, chreql
  ! check whrther ch0 is upcase, and ch1 is the lower case
  upcase(ch0,ch1) = ichar('a').le.ichar(ch0).and.ichar(ch0).le.ichar('z').and. &
  ichar(ch1)-ichar(ch0).eq.ichar('A')-ichar('a')
  ! equal or corresponding
  chreql(ch0,ch1) = ch0.eq.ch1.or.upcase(ch0,ch1).or.upcase(ch1,ch0)
!*     ------------------------------------------------------------------
!*   ..test installation
!*     (protection against errors of case conversion.)

! test for ichar
  if (ichar('a').eq.ichar('A')) then
   stop 'streql--installation error'
  endif
! *   ..preliminaries
  len0 = len(str0) ! get the length of str0
  len1 = len(str1) ! ditto!
! *   ..search for mismatch
  k = 0
1 continue
  if (k.lt.min(len0,len1)) then ! if len0 or len1 >0, next
   if (chreql(str0(k+1:k+1),str1(k+1:k+1))) then ! check whrther str0(k) equal to str1(k) or
     ! corresponding
    k = k+1 ! scan the strings
    goto 1 ! continue
   endif
  endif
  !*   ..set return value
  if (len0.eq.len1) then
   s_eq = k.eq.len0 ! logial value, specify whether the string is equal with the minmum length
   ! of two strings
 else if (len0.lt.len1) then ! if str0 is shorter than str1
   s_eq = k.eq.len0.and.str1(len0+1:len1).eq.' '
  else if (len1.lt.len0) then
   s_eq = k.eq.len1.and.str0(len1+1:len0).eq.' '
  endif
  return
! *     ------------------------------------------------------------------
  end

end module base


program test_carre

! ========================================================
! =============== declaring modules ======================
! ========================================================

! declaring the variables type
integer,parameter::r8=selected_real_kind(14)
integer,parameter::r4=selected_real_kind(6)
integer,parameter::i4=selected_int_kind(8)
character*14 chtrg,chstr,chequ,chaux,chdgo &
,chcfld,chcequ,chcstr,chccrr,chcslp

! test for inquire
logical::ex

! ========================================================
! ================== assign modules ======================
! ========================================================

data chtrg,chstr,chequ,chaux,chdgo &
    /'dg.trg','dg.str','dg.equ','dg.aux','dg.dgo'/




! ========================================================
! ===================== I/O modules ======================
! ========================================================



print *,r8
print *,r4
print *,i4

! check data.dat exist or not
inquire(file=chtrg,exist=ex)
print *,ex

if(.not.ex)then
    write(*,*) 'fcrr: the file',chtrg,'must be present!'
else
    write(*,*) chtrg,' exist!'
end if

! test for fcrtrn (read fucntion) and entry statement

open(1,file=chtrg)
call fcrchktp(1)
! call import(fcrtrn)
close(1)

! test for read str
open(1,file=chstr)
call fcrstri(1)
! call import(fcrtrn)
close(1)

! read the *.dgo file
open(1,file=chdgo)
call fcrdgi
close(1)






end program test_carre

subroutine fcrchktp(lun)
    ! grep topo dg.trg
    implicit none
    integer::lun
    logical:: ldgv2=.false.
    character*8 uline*80
    rewind lun
9000 read(lun,'(a)',end=9010) uline
    if(index(uline,'#').eq.0) go to 9010
    ldgv2=index(uline,'# topo').gt.0
    if(.not.ldgv2) go to 9000
9010 rewind(lun)
    print *,uline
    return
end subroutine fcrchktp


subroutine fcrstri(lun)
    implicit none
    integer::lun
    integer::i,j,k,l,n,nstr
    real(kind=4)::x,y
    integer,parameter::ngpr=1025,ngpz=1025, nnstr=60, nmstr=1000
    real(kind=8)::xstr(nmstr),ystr(nmstr),lstr(nnstr)

    rewind(lun) ! reset the file pointer
    read(lun,*) n
    nstr=0 ! initinalize the nstr, specify the number of closed
    ! polygon
    l=0

    do i=1,n ! read each of polygon
        read(lun,*,err=900) k ! read the number of element || error message
        do j=1,k ! scaning every element index
            l=l+1 ! index for storing the polygon position
            read(lun,*,err=900)x,y ! get the x,y position
            xstr(l)=1e-3*x ! convert to mm
            ystr(l)=1e-3*y ! ditto
        end do
        nstr=nstr+1 ! read the next polygon
        lstr(nstr)=k ! array contains their length
    end do
    print *,"It's confusing that the xstr and ystr don't output????????"
    print *,"Is it only for check ?"
    return

    ! error procedure
900 write(*,*)'fcrstri: error in the structure',i
    stop
end subroutine fcrstri

subroutine fcrdgi(nam)
  implicit none
  character*(*)::nam ! declaring a string, don't specify the string length
  integer,parameter::nnms=7 !
  integer::i,j,lutrg
  integer,parameter::nxptm=2 ! maximum number of XP
  integer::nxpt ! actual number of XP
  real(kind=4)::x(3),xarray(3,nxptm),rdummy
  ! nxptm: maximum of pre-define XP
  character(len=8)::name,unm(nnms)
  logical::streql
  real(kind=8)::xptcntr(3,nxptm)
  external s_eq

  ! list of the valid input keywords
  data num /'xptcntr','xlpcntr ','trg_spcf','tgtgrd','lm_cnfg ',&
  'lm_pntrt','lm_grcln'/
  ! xptcntr: coordinates of the XP
  ! xlpcntr: coordinates of the op
  data lutrg /0/

  call locase(nam,name,8) ! make all string into lower character ????
  do i=1,nnms
    if(streql(name,num(i))) go to (10,20,30,40,50,60,70),i
  end do
  call skipit ! what is it skipit ???
  return
  ! check the xptcntr
10 call rearre(xarray,3*nxptm,i)
  print *,"To get the XP position"


  nxpt=0 ! actual number of pre-defined XP
  if(i.lt.2) then  ! correspond to the second variables, OP location
    write(*,*) '*** fcrtrn: too few values for xptcntr'
    xptcntr(1,1)=-1.0
  else
    nxpt=max(i/3,1)
    do j=1,nxpt
      do i=1,3
        xptcntr(i,j)=0.001*xarray(i,j) ! get the XP position
      end do
    end do
  end if
  return
