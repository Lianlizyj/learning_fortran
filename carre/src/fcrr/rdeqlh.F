      subroutine rdeqlh(lun,nr,nz,btf,rtf,*)
c
c  version : 16.09.2000 00:19
c
c=======================================================================
c*** Reads the header of the equilibrium file and returns dimensions
c*** and the value of the toroidal field, btf, measured at radius rtf -
c*** provided that this information is present in the equilibrium file.
c
      implicit none
      integer lun,nr,nz
      real btf,rtf
      integer i,j,k,l
      character ss*80, s(80), hh*80, h(80), utb
      equivalence (s,ss),(h,hh) ! share memery between a and b
      logical ll
c=======================================================================
c
      utb=char(9)
      rtf=-1.
 10   read(lun,'(a)',end=90) ss ! read the line, if error, display the error message
      if(ss.eq.' ') go to 10 ! if ss is empty, read the next line (until the last line)
      k=0 ! read the first not empty line
      hh=' ' ! set hh to empty
      do 100 i=1,80 ! do i=1,80
          if(s(i).eq.'(') then ! check if read the position head lines
              if(hh.eq.'r') return ! if read the character "r" r(1:jm),
              go to 90 !  display the error message
          end if
          if(s(i).eq.':') go to 10 ! if read the ":", containing the header information, read the line
          if(s(i).eq.' ' .or. s(i).eq.utb) go to 100 ! if blank, next loop
          if(s(i).eq.'=') go to 20 ! if find "=",
          k=k+1
          h(k)=s(i) ! get the header line information
 100  continue

      if(hh.eq.'r(1:jm);') return ! if read r(1:jm), display error message
      go to 90
c-----------------------------------------------------------------------
c*** Parse the string
c
 20   l=i+1
      if(hh.eq.'jm') then
          j=1
      else if(hh.eq.'km') then
          j=2
      else if(hh.eq.'btf') then
          j=3
      else if(hh.eq.'rtf') then
          j=4
      else
          go to 10
      end if
      k=0
      hh=' '
      ll=.false.
      do i=l,80 !{
        if(s(i).eq.';') go to 30
        if(s(i).eq.' ' .or. s(i).eq.utb) then !{
          if(ll) go to 30
        else !}{
          ll=.true.
          k=k+1
          h(k)=s(i)
        end if !}
      end do !}

 30   continue
      select case (j) ! get the nr,nz,btf,rtf
      case (1)
        read(hh,'(bn,i4)') nr
      case (2)
        read(hh,'(bn,i4)') nz
      case (3)
        read(hh,'(bn,e12.0)') btf
      case (4)
        read(hh,'(bn,e12.0)') rtf
      case default
        continue
      end select
      go to 10
c=======================================================================
c*** Error encountered
c
 90   write (6,*) 'Wrong format of the equilibrium file - sorry!'
      return 1
c=======================================================================
      end
