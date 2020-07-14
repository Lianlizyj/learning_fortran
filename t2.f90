module constants1
  implicit None
  real,parameter,private::pi3=3.1415926536
  real,parameter,private::e3=2.7182818285
contains
  subroutine show_constants1()
    print *,"Pi= ",pi3
    print *,"e= ",e3
  end subroutine show_constants1
  function epowerxf(x) result(epx)
    implicit None
    real::x
    real::epx
    epx=e3**x
  end function epowerxf

  function areacircle(r) result(a)
    implicit None
    real::r
    real::a
    a=pi3*r**2
  end function areacircle


end module constants1

module constants
  implicit None
  real,parameter::pi2=3.1415926536
  real,parameter::e=2.7182818285
contains
  subroutine show_constants()
    print *,"Pi= ",pi2
    print *,"e= ",e
  end subroutine show_constants
end module constants

program addNumbers
! this is a simple program adds two numbers
! test for use Modules
use constants
use constants1
implicit none

! implicit typing: ????????????????????????????????????
! ?????????
! i,j,k,l,m,n ???????????? real type






! type declarations
real :: a,b,result
! test for integer type
integer :: largeval

! test for data byte
    ! two byte integer
    integer(kind=2):: shortval
    ! four byte integer
    integer(kind=4)::longval
    ! eight byte integer
    integer(kind=8)::verylongval
    ! default integer
    integer :: defval



! test for real type
    ! read type and double precision
    ! define real variables
    real:: p,q,realres
    ! defiene integer variables
    integer::i,j,intres,mod_num

! character type
    character (len=40)::name

! test for variables declaration
    integer :: total ! variables declaration is set by type ??
    real :: average
    complex::cx,cy
    logical::done
    character(len=80)::message ! a string of 80 characters

! test for named constant
    real,parameter::pi=3.14159265389793238
    ! gravitational acceleration
    real, parameter::g=9.81
    ! variable declaration
    real:: s! displacement
    real:: t! dtime
    real:: u!initial speed
! test for decision of if...else if...else statement
    ! logical variables declaration
    integer::a_decision=100 ! you can assign when variabels are declared

! test for number of complex
    integer::i_number=10
    real::x=5.17

    ! test for complex number arithmetic
    complex,parameter::i_arithmetic=(0,1) ! sqrt(-1)
    complex::x_arithmetic,y,z

! test for rangeprecision
    real::x_real,y_real,z_real

! test for kind specifier check
    integer ::i_kind
    real::r_kind
    character*1::c_kind
    logical::lg_kind
    complex::cp_kind

! test for characters
    character(len=15)::surname,firstname
    character(len=6)::title
    character(len=25)::greetings
! test for character functions
    ! test for index
    character (80):: text
    integer::i_cf
    ! test for trim
    character(len=15)::surname_cf,firstname_cf
    character(len=6)::title_cf
    character(len=25)::greetings_cf
    ! test for achar
    character::ch
    integer::i_cf_achar,i_cf_len,i_cf_iachar
    ! test for adjustl & adjustr
    character(len=20)::str_l="     gfortran",str_r="  gfortran       "

! declaration of test for lexcial order of characters
    character::a_ll,b_ll,c_ll

! test for extracting substrings
    character(len=11):: hello
    ! test for date and time functions
    character(len=8)::dateinfo ! ccyymmdd
    character(len=4)::year,month*2,day*2
    character(len=10)::timeinfo ! hhmmss.sss
    character(len=2)::hour,minute,second*6
    ! test for trim fucntion
    character(len=*),parameter:: fname='Susanne',sname='Rizwan'
    character(len=20)::fullname

! test for declaring the arrays
    real,dimension(5)::numbers
    integer,dimension(3,3)::matrix
    real,dimension(2:6)::numbers_low
    integer,dimension(-3:2,0:4)::matrix_arr ! specify the bound in fortran

! test for passing arrays to precedures
    integer, dimension(5)::my_arr
    integer::i_sub

    ! test for interface
    integer,dimension(10)::my_arr1
    integer::i_arr1

    interface
      subroutine fillarray1(a)
        integer,dimension(:),intent(out)::a
        integer::i
      end subroutine fillarray1

      subroutine printarray1(a)
        integer,dimension(:)::a
        integer::i
      end subroutine printarray1
    end interface

! test for array sections
    real,dimension(10)::a_sec,b_sec
    integer::i_sec,asize,bsize

! test for dynamic arrays
    ! rank is 2, but size not known
    real,dimension(:,:),allocatable::darray
    integer::s1,s2
    integer::i_dy,j_dy

! test for data statement
    integer::a_data(5),b_data(3,3),c_data(10)

! test for use of where statement
    integer ::a_wh(3,5)

! test for declaring the derived data type
    type books
        character(len=50)::title
        character(len=50)::author
        character(len=150)::subject
        integer::book_id
    end type books
    ! declaring a book type
    type(books)::book1

    ! declaring an array of structure
    type(books),dimension(2)::list

! declaring a pointer variable
    integer,pointer::p1
    integer,target::t1,t2

! declaring for the test of formatted output
    ! declared aboved
    ! formatted output for characters
    character(len=15)::first_name_out
    ! formatted real output
    real::c_out=1.2786456e-9,d=0.1234567e3
    integer::n=300789,k=45,i_out=2
    character(len=15)::str="Tutorials point"
    ! test for format statements
    character(len=15)::name_out
    integer::id
    real::weight

! declaring for the test of open and close
    real,dimension(100)::x_open,y_open
    real,dimension(100)::p_open,q_open
! test for fucntion
    real :: a_fcn

! test for subroutines
    real::a_subs,b_subs

! test for intent attribute
    real::x_int,y_int,z_int,disc

! test for recursive precedures
    integer::f_rec

! test for Modules
    real::x_mod,epowerx,area_mod,radius_mod

! test for private variables instrinsic Modules

! test for numeric fucntions
    real::a_nu,b_nu
    complex::z_nu

! test for mathematical functions
    real,parameter::g_ma=9.8
    real,parameter::pi_ma=3.1415927
    real::a_ma,t_ma,u_ma,x_ma,y_ma







! assigning values
    print *, "=============== variables declaration! ==========="
    print *, "=============== next, we will assign value for variables ========="

    ! assigning values
    p=2.0
    q=3.0
    i=2
    j=3
    ! floating point division
    realres=p/q
    intres=j/i
    mod_num=mod(j,i)
    name="zara ali" ! ???��???????????????????

    ! test for the assign of named constant
    t=5.0
    u=50
        ! displacement
    s=u*t-g*(t**2)/2



    ! assign for variables test
    total=20000
    average=1666.67
    done=.true.
    message="a big hello from tutorials point"
    cx=(3.0,5.0) ! cx=3.0+5.0i

    ! another way for assign value for complex type
    cy=cmplx(1.0/2.0,-7) ! cy=0.5-7.0i

    ! assign for complex arithmetic test
    x_arithmetic=(7,8)
    y=(5,-7)

    ! assign for rangepresision
    x_real=1.5e+20
    y_real=3.73e+20

    ! assign for character
    title='Mr'
    firstname='Rowan '
    surname='Atkinson'
    greetings='A big hello from Mr. Beans'

    ! assign for test character functions of index & trim & iachar & len
    text="The instrinsic data type character stores characters and strings."
    i_cf=index(text,'character')
    i_cf_len=len(text)
    title_cf=' Mr. '
    firstname_cf='Rowan'
    surname_cf='Atkinson'

    ! assign for test of lexcial order of characters
    a_ll='A'
    b_ll='a'
    c_ll='B'

    ! assign for test of extracting substrings
    hello='Hello world!'

    ! assign for a books type subject
    book1%title="C PROGRAMMING"
    book1%author="Nuha Ali"
    book1%subject="C programming Tutorial"
    book1%book_id=6495407

    ! assign for a books type of an array structures
    list(1)%title="C PROGRAMMING"
    list(1)%author="Nuha Ali"
    list(1)%subject="C programming Tutorial"
    list(1)%book_id=6495407
    list(2)%title="Telecom Billing"
    list(2)%author="Zara Ali"
    list(2)%subject="Telecom Billing Tutorial"
    list(2)%book_id=6495700


! complex type: real part and imaginary part

! logical type: only two logical value: .true. and .false.

! character type: stores character and strings. The length
! of the string can be specified by len specifier. If no len
! gth is specified, it is 1(character)

    print *,"================= output the results ==========================\n"
    print *,"newline test"
    print *,"ok .not. ok?"

! excutable statements
a=12.0
b=15.0
result=a+b
print *,"The total is ", result

! test for integer type
print *,"=============== test for integer type ============="
print *,huge(largeval) ! ?????????????????

! test for data byte
    print *, "=============== test for data byte ============="
    print *, huge(shortval)
    print *, huge(longval)
    print *, huge(verylongval)
    print *, huge(defval)

! test for real type
    print *, "================= test for real type =============="
    print *, realres
    print *, intres ! ?????????????????????
    print *, mod_num ! ????

! test for character type
    print *, "================== test for character type ============"
    print *, name
    print *, name(1:4) ! ??????????????????????


! variable declaration : names are case-insensitive
    ! test for case-insensitive
    print *, "============= test for case-insensitive ========="
    print *, huge(Defval) ! yes, the varables is case-insensitive in fortran

    ! variables are decleared at the beginning of a program
    ! syntax : type-specifier :: variable_name

    print *, "============== for multiply type display ========="
    print *, "integer type: ",total
    print *, "real type: ",average
    print *, "complex type: ",cx
    print *, "use cmplx(x,y) to assign value: ",cy
    print *, "logical type: ",done
    print *, "character type: ",message
! constants: cannot alter during its execution. These fixed values
! are called literals.
    ! there are two type of constant: literal constants and named
    ! constants

    ! literal constant(have value but no name)
    ! named constant has a value as well as a name
    ! named constant at the beginning of a program or procedure
    ! named constant are decleared with the parameter attribute.
    print *,"================ test for named constant"
    print *,"Time =",t
    print *,"Displacement =",s

! test for operators: tell the complier to perform specific mathematical
! or logical manipulations.
    ! arithermetic operators
    ! relational operators
        ! == .eq.
        ! /= .ne.
        ! > .gt.
        ! < .lt.
        ! >= .ge.
        ! <= .le.
    ! logocal operators

! decision: logical decision
    ! if ...else if...else statement

    ! check the logical condition using if statement
    print *,"=============== test for decision of if...else if...else statement ============"

    if (a==10) then
    ! if condition is true then print the following
        print *, "The value is 10"
    else if (a==20) then
        print *, "The value is 20"
    else if (a==30) then
        print *, "The value is 30"
    else
        ! if none of the conditions is true
        print *, "None of the value is matching"
    end if
    print *, "excat value of a is ",a_decision

! test for loop:
    ! do loop
    ! do while loop
    ! nested loops

    ! loop control statements
        ! (1)exit: loop is exited, program continue at the first
        ! executable statement after the end do statement
        ! (2)cycle: continue ate the start of the next iteration (continue)
        ! (3)stop: similar with a breakpoint

! numbers:three instrinsic data types:
    ! integer type
    ! real type
    ! complex type

    ! test for cmplx(x,y), which produces aresult who's real and imaginary parts
    ! are single precision
    print *, "================ test for complex arithmetic ============"
    print *, cmplx(i_number,x)

    ! test for complex number arithmetic
    write(*,*)i_arithmetic * x_arithmetic * y
    z=x_arithmetic+y
    print *,"z=x+y= ",z
    z=x_arithmetic-y
    print *,"z=x-y= ",z
    z=x_arithmetic*y
    print *,"z=x*y= ",z
    z=x_arithmetic/y
    print *,"z=x/y= ",z

! the range, precision and size of numbers
    ! number of bits:
        ! 64 2**63-1
        ! 32 2**31-1
    ! display the number of bits
    ! Number of bits |largest value|smallest value|precision
    ! :--:|:--:|:--:|:--:
    ! 64 |0.8e+308|0.5e-305|15-18
    ! 32 |1.7e+38|0.3e-38|6-9
    print *,"=============== test for rangeprecision ============"
    z_real=x_real*y_real
    print *,"x_real*y_real= ",z_real
    z_real=x_real/y_real
    print *,"x_real/y_real= ",z_real

! the kind specifier��query the details of the hardware's
    print *, "============== test for kind specifier ============="
    print *, "integer ", kind(i_kind)
    print *, "real ",kind(r_kind)
    print *, "complex ",kind(cp_kind)
    print *, "character ",kind(c_kind)
    print *, "logical",kind(lg_kind)
! characters: fortran treat character as single character or contiguous strings
    ! the length of strings can be specified by len specifier
    print *,"=============== test for character ===================="
    print *,"Here is ",title,firstname,surname
    print *,greetings
! test for concatenation of character
    print *,"================ test for concatenation ==============="
    !write(*,"(r3,/,r3)")x_real,y_real
    name=title//firstname//surname
    print *,"Here is ",name
    print *,greetings
! test for characters functions
    if (i/=0) then
      print *,"================ test for cahracter functions (index) =============="
      print *,'The word charcter found at the position',i_cf
      print *, " in text: ", text
    end if
    ! test for trim & len
    print *, "================= test for character functions (trim & len )"
    print *, "Here is", title_cf,firstname_cf,surname_cf
    print *, "Here is", trim(title_cf),' ',trim(firstname_cf),' ',trim(surname_cf)

    print *, "The length of text is : ", i_cf_len
    ! test for achar & iachar (convert format between integer and character)
    print *, "================= test for character fucntions (achar & iachar) =============="
    do i_cf_achar=65,90
      ch=achar(i_cf_achar)
      print *,i_cf_achar,' ',ch
      i_cf_iachar=iachar(ch)
      print *, i_cf_achar,' ',i_cf_iachar
    end do
    ! test for scan
    print *,"================= test for character functions (scan ) ====================="
    print *,"The text is: ",text
    print *,scan(text,"ig") ! test for a set of characters
    print *,scan(text,'ig',.true.) ! test for .true. parameter
    print *,scan("fortran",'s') ! test for nothing found
    ! test for verify
    print *,"================= test for verify ==================="
    print *,verify("fortran",'ao')
    print *,verify("fortran",'foo')
    print *,verify("fortran",'c++')
    print *,verify("fortran",'c++',.true.)
    print *,verify("fortran",'fortran')
    ! test for adjustl & adjustr
    print *,"================ test for adjustl & adjustr =============="
    str_l=adjustl(str_l)
    str_r=adjustr(str_r)
    print *,"adjustl is ",str_l
    print *,"adjustr is ",str_r
    ! test for repeat
    print *,'================ test for repeat ========================'
    print *,trim(repeat('ha ',5)) ! ha ha ha ha ha ha
    print *,len(trim(repeat('x ',5))) ! the right value should be 9

! test for lexcial order of characters
    print *, "============= test for lexical order of character =============="
    if (lgt(a_ll,b_ll)) then
      print *,'A is lexically greater than a'
    else
      print *,'a is lexically greater than A'
    end if
    if (lle(a_ll,c_ll)) then
      print *,'A is lexically less than or equal to B'
    end if

! test for extracting substrings
    print *,"============== test for extracting substrings ==================="
    print *,"hello(7:11) is ", hello(7:11)

    ! test for date_and_time function
    call date_and_time(dateinfo,timeinfo)
    ! let's break dateinfo into year,month, and day
    ! dateinfo has a form of ccyymmdd, where cc= century ,yy =year
    ! mm=month, and dd = day
    year=dateinfo(1:4)
    month=dateinfo(5:6)
    day=dateinfo(7:8)

    print *,"============== test for date_and_time function =================="
    print *,'date string:', dateinfo
    print *,'year:',year
    print *,'month:',month
    print *,'day:',day

    ! let's break timeinfo into hour,minute and second
    ! timeinfo has a form of hhmmss.sss, where h= hour, m= minute and s= second

    hour=timeinfo(1:2)
    minute=timeinfo(3:4)
    second=timeinfo(5:10)

    print *,'Time string:', timeinfo
    print *,"Hour:",Hour
    print *,"minute:",minute
    print *,"second",second

    ! test for triming strings
    print *,"test for trim"
    fullname=fname//" "//sname ! concatenating the strings
    print *,fullname, ", the beautiful dancer from the east!"
    print *,trim(fullname),", the beautiful dancer from the east!"

! test for arrays: all arrays consist of contiguous memory locations. The lowest address corresponds to the first element and
! the highest address to the last element
    ! number1 number 2 number 3...
    ! declaring arrays:

    ! assigning values
    numbers(1)=2.0
    print * ,"================== test for the array assigning ================="
    print *,numbers
    do i=1,5
      numbers(i)=i*2.0
    end do
    print *,"The complete numbers is : ", numbers
    numbers=(/1.5,3.2,4.5, 0.9,7.2 /)
    print *, "The directly assigned values is : ", numbers ! no space between the bracket'(' and the back slash'/'
    print *,"The value in fortran is not exacting, i.e. (0.9 and 0.899999976)"

    ! demostrates the concepts discussed aboved
    print *,"============== assign and display the vector and matrix value =============="
    print *,"Display the each element of numbers: "
    do i=1,5
      print *,numbers(i)
    end do

    ! assign some values to the array matrix
    do i=1,3
      do j=1,3
        matrix(i,j)=i+j
      end do
    end do
    ! display the values
    print *,"Display the matrix directly.",matrix
    do i=1,3
      do j=1,3
        print *,matrix(i,j)
      end do
    end do
    ! short hand assignment
    numbers=(/1.5,3.2,4.5,0.9, 7.2/)
    ! display the values
    do i=1,5
      print *,numbers(i)
    end do

    print *,"Fortran is column-marjor order like matlab. However, the order for sorting the matrix in python is row-marjor"

! test for array related terms
    print *,"========================== Test for array related terms ======================="
    print *, "Ndims of numbers is :",rank(numbers)
    print *, "Ndims of numbers is :",rank(matrix)
    print *, "Shape of matrix is :",shape(matrix)
    print *, "Size of matrix is :",size(matrix)

! test for passing arrays to precedures
    print *,"======================= Test for subroutine ====================== "
    call fillarray (my_arr)
    call printarray(my_arr)

! test for interface
    print *,"============== test for interface ======================="
    print *,"interface should declaing in the begining of precudure or before assigning!"
      call fillarray1(my_arr)
      call printarray1(my_arr)
    print *,"can't use interface (need to be done)"

! test for array sections
    a_sec(1:7)=5.0
    a_sec(8:)=0.0
    b_sec(2:10:2)=3.9
    b_sec(1:9:2)=2.5
    ! Display
    asize=size(a_sec)
    bsize=size(b_sec)
    do i=1,asize
      print *,a_sec(i)
    end do
    do i=1,bsize
      print *,b_sec(i)
    end do

! array intrinsic functions: vector and matrix multiplication; reduction; inquiry ; construction ; reshape; manipulation; locations

! test for dynamic arrays
    ! declared by attribute allocatable
    print *,"============ test for dynamic array =================="
    print *,"enter the size of the arrays"
!    read *, s1,s2 ! can't use in simply fortran
    s1=3
    s2=4
    ! allocatable memory
    allocate(darray(s1,s2))
    do i=1,s1
      do j=1,s2
        darray(i,j)=i*j
        print *,"darray(",i,",",j,") =",darray(i,j)
      end do
    end do
    deallocate(darray)

! use of data statements
    ! initialising more than one array, or for array sections initialisation
    data a_data /7,8,9,10,11/
    data b_data(1,:) /1,1,1/
    data b_data(2,:) /2,2,2/
    data b_data(3,:) /3,3,3/
    data (c_data(i),i=1,10,2) /4,5,6,7,8/
    data (c_data(i),i=2,10,2) /5*2/
    print *,"============= test for data statement ==============="
    print *,"The A array"
    do j=1,5
      print *,a_data(j)
    end do

    print *,"The B array"
    do i=lbound(b_data,1),ubound(b_data,1) ! return the lower and upper nbound of an array
      ! print every row separately
      write(*,*) (b_data(i,j),j=lbound(b_data,2),ubound(b_data,2))
    end do
    print *,"The C array:"
    do j=1,10
        print *,c_data(j)
    end do

! use of where statement
    print *,"Test for where statement"
    do i=1,3
        do j=1,5
        a_wh(i,j)=j-i
        end do
    end do
    print *, 'The A array:'
    do i=lbound(a_wh,1),ubound(a_wh,1)
        write(*,*)(a_wh(i,j),j=lbound(a_wh,2),ubound(a_wh,2))
    end do

    where(a_wh<0)
        a_wh=1
    elsewhere
        a_wh=5
    end where
    print *,'The A array'
    do i=lbound(a_wh,1),ubound(a_wh,1)
        write(*,*) (a_wh(i,j),j=lbound(a_wh,2),ubound(a_wh,2))
    end do
! derived data types : also called a stucture
    ! display the book info
    print *,book1%title
    print *,book1%author
    print *,book1%subject
    print *,book1%book_id

! array of structures
    ! display the list(2) info
    print *,list(1)%title
    print *,list(1)%author
    print *,list(1)%subject
    print *,list(1)%book_id
    ! display the list(2) info
    print *,list(2)%title
    print *,list(2)%author
    print *,list(2)%subject
    print *,list(2)%book_id

! test for pointers
    ! pointer in fortran contains more information about a
    ! particular objectm like type, rank, extents and memory address

    ! test for allocate space
    allocate(p1)
    p1=1
    print *,"========== test for allocate of a pointer ==========="
    print *,p1
    p1=p1+4
    print *,p1
    ! test for targets and association
    p1=>t1 ! create association between pointer and target
    p1=1
    print *,"============ test for target assocoation =============="
    print *,p1
    print *,t1
    p1=p1+4
    print *,p1
    print *,t1
    t1=8
    print *,p1
    print *,t1
    ! test for nullify statement disassociates a pointer from a target
    p1=>t1 ! create association between pointer and target
    p1=1
    print *,"============ test for nullify the assocoation =============="
    print *,p1
    print *,t1
    p1=p1+4
    print *,p1
    print *,t1
    t1=8
    print *,p1
    print *,t1
    nullify(p1)
    print *,t1
    p1=>t2
    print *,associated(p1) ! is it associated?
    print *,associated(p1,t1) ! p1=>t1?
    print *,associated(p1,t2) ! p1=>t2?
    ! what is the value of p1 at present
    print *,p1
    print *,t2
    p1=10 ! before p1 associated with a rand number
    print *,p1
    print *,t2
    print *,"target is the highes order,when target change, the pointer value change also."

! test for basic input and output: The form of input-outout of read and print is called list-directed input-output
    ! this is free format I/O
    ! formatted input output: read fmt,variable_list , fmt is the format specification
    ! display the pi
    print *,"============= Test for formatted output =================== "
    print "(f6.3)",PI
    print "(f10.7)",PI
    print "(e16.4)",PI
    print "(es10.3)",PI*1e6
    print "(/,5x,f6.3,/)",PI
    print "(f20.15)",PI

    ! test for character output
    print *,"please enter your firstname"
    print *,"Up to 20 characters, please"
    read *,first_name_out
    print "(1x,a)",first_name_out ! character output

    ! test for real format
    print "(i6)",k
    print "(i6.3)",k
    print "(3i10)",n,k,i_out
    print "(i10,i3,i5)",n,k,i_out
    print "(a15)",str
    print "(f12.3)",d
    print "(e12.4)",c_out
    print "(/,3x,'n=',i6,3x 'd=',f7.4)",n,d

    ! test for format statements
    name_out="Ardupilot"
    id=1
    weight=0.08
    print *,'The product details are'
    print 100
    100 format(7x,'name:',7x,'id:',1x,'weoght:')
    print 200,name_out,id,weight
    200 format(1x,a,2x,i3,2x,f5.2)

! test for file input and output
    ! test for open and close
    ! data
    do i=1,100
      x_open(i)=i*0.1
      y_open(i)=sin(x_open(i))*(1-cos(x_open(i))/3.0)
    end do
    ! output data into a file
    open(1,file='data1.dat',status='new')
    do i=1,100
      write(1,*) x_open(i),y_open(i)
    end do
    close(1)

    ! test for opening the file for reading
    open(2,file='data1.dat',status='old')
    print *,"============ test for read from a file ======================="
    do i=1,100
      read(2,*) p_open(i),q_open(i)
    end do
    close(2)

    do i=1,100
      write(*,*)p_open(i),q_open(i)
    end do
    print *,"how to delete a exist file via linux cmd?"
    ! test for system command
    call system('rm data1.dat')
    print *,'Well done, you delete the "data1.dat" successfilly!'
! test for procedures: There are two types of procedires: functions and subroutines
    ! A function is a procedure that returns a single quantity. A function should not modify its arguments
    ! The returmed quantity is known as function value, and it is denoted by the function name
    a_fcn=area_of_circle(2.0)
    print *,"================ test for function ====================="
    print *,"The area of a circle with radius 2.0 is"
    print *,"=============== test for contains function ===================="
    print *,a_fcn


    print *,"=============== test for external fucntion ====================="
    print *,"need to be done"

! test for subroutines
    a_subs=2.0
    b_subs=3.0
    print *,"================ test for subroutine ============================"
    print *,"Before calling swap"
    print *,"a= ",a_subs
    print *,"b= ",b_subs
    call swap(a,b)
    print *,"After calling swap"
    print *,"a= ",a_subs
    print *,"b= ",b_subs

! test for specifying the intent of the arguments
    ! The intent attribute allows you to specify the intention which arguments are
    ! used in the procedure. The follow table provides the values of intent attribute
    ! in / out / inout
    x_int=1.0
    y_int=5.0
    z_int=2.0
    call intent_example(x_int,y_int,z_int,disc)
    print *,"=========== Test for intent attribute ============"
    print *,"The value of the discrimininat is"
    print *,disc

! test for recusive precedures
    print *,"============ Test for recursive precedures ================"
    i=15
    print *,"The value of factorial 15 is"
    f_rec=myfacrotial(15)
    print *,f_rec

! test for internal procedures
    ! when a procedure is containsed within a program, it is called the internal
    ! procedure of the program. The syntax for containing an internal procedure is
    ! as follows:
    print *,"================ test for internal procedures ============================"
    print *,"Before calling swap"
    print *,"a= ",a_subs
    print *,"b= ",b_subs
    call swap1(a,b)
    print *,"After calling swap"
    print *,"a= ",a_subs
    print *,"b= ",b_subs

! test for modules
    ! A modues is like a package where you can keep your functions and subroutines, in case
    ! you are writing a very big program, or your funtions or subroutines can be used in more
    ! than one program.
    ! Modules provide you a way of splitting your programs between multiple files.
      !used for:
      ! (1) packaging subprograms, data and interface blocks
      ! (2) Defining global data that can be used by more than one routine
      ! (3) Declaring variables that can be made avilable within any routines you choose.
      ! (4) Importing a module entirely, for use, into another program or subroutine.
      print *, "============ Test for using modules ================="
      x_mod=2.0
      radius_mod=7.0
      epowerx=e**x_mod
      area_mod=pi2*radius_mod**2
      call show_constants()
      print *,"e raised to the power of 2.0= ", epowerx
      print *,"Area of a circle with radius 7.0= ",area_mod

      ! test for accessibility of variables and subroutines in a module
        ! when you declare some variable or subroutine as private, it is not avilable
        ! outside the module.
        print *,"============= test for private variables in modules ==========="
        print *, "It's ok, when module declared after the main program."
        print *,"Howerer, it occur error, when module declared before the programs"

      ! test private in a Modules fucntions
      call show_constants1()
      print *,"e raised to the power of 2.0=", epowerxf(2.0)
      print *,"area of a circle with radius 7.0= ",areacircle(7.0)

! test for intrinsic function:
    ! instrinsic funtion are some common and important functions that are provided
    ! as a part of the fortran language. We have already discussed some of these functions
    ! in the arrays,characters and string chapters
    ! instrinsic funtion can be categorised as:
      ! numberic functions
      ! mathematical fucntions
      ! numeric inquiry fucntions
      ! floating-point manipulation functions
      ! bit manipulation functions
      ! cahracter fucntions
      ! kind function
      ! logical fucntion
      ! array fucntion
    ! test for numeric fucntions
    print *,"============= test for numeric fucntion ================"
    a_nu=15.2345
    b_nu=-20.7689
    write(*,*) 'abs(a):',abs(a),'abs(b)',abs(b)
    write(*,*) 'aint(a):',aint(a),'aint(b)',aint(b)
    write(*,*) 'ceiling(a):',ceiling(a),'ceiling(b):',ceiling(b)
    write(*,*) 'floor(a):',floor(a),'floor(b):',floor(b)

    z_nu=cmplx(a_nu,b_nu)
    write(*,*)'z: ',z_nu

! test for mathematical fucntions
    print *,"============ test for mathematical function ================="
    a_ma=45.0
    t_ma=20.0
    u_ma=10.0
    ! convert angle to radians
    a_ma=a_ma*pi_ma/180.0
    x_ma=u_ma*cos(a_ma)*t_ma
    y_ma=u_ma*sin(a_ma)*t_ma-0.5*g_ma*t_ma**2
    write(*,*) 'x:',x_ma,'y: ',y_ma
! test for numeric inquiry fucntion
      ! digit / epslion / huge / maxexponent / precision / radix / range / tiny /

! test for floating-point manipulation functions
    ! exponent / fraction / nearest / rrspacing / scale / set_exponent / spacing

! test for bit manipulation function
    ! bit_size, btest, iand, ibclr, ibits, ibset, ieor, ior, ishift, ishftc, not

! test for character fucntion
    ! achar adjustl, adjustr, char, iachar, ichar, len, len_trim, lge, lgt,lle, llt,
    ! repeat, scan, trim, verify

! test for kind functions
    ! kind, selected_int_kind, selected_real_kind

! logical fucntions
    ! logical

! test for kind attribute
    ! precision, kind, maxexponent, range

! program libraries
      ! randlib: random number and statistical distribution generators
      ! blas
      ! eispack
      ! gams-nist to available math software
      ! some statistical and other routine from nist
      ! lapack
      ! linpack
      ! minpack
      ! mudpack
      ! ncar mathematical library
      ! the netlib collection of mathematical software, papers, and databases
      ! odepack
      ! oderpack, a set of routines for ranking and ordering
      ! expokit for computing matrix expoentials
      ! slatec
      ! specfun
      ! starpac
      ! statlib statistical library
      ! toms
      ! sorting and merging strings

      ! not free
      ! nar fortran numerical library
      ! the visual numberics imsl library
      ! numerical recipes

! programming stytle
      ! a good program should have following characteristics
          ! (1) readability
          ! (2) proper logical structures
          ! (3) self-explanatory notes and comments
          ! i.g. ! loop to calculate ncr
      ! (1) indented code blocks to make various levels of code clear
      ! (2) self-checking code to ensure there will be no numerical errors like
      ! division by zero, square root of a negative real number or logarithm of
      ! a negative real number
      ! (3) including codes that ensure variables do not take illegal or out of range
      ! values, i.e., input validation
      ! (4) not puttung checks where it would be unnecessary and slows down the execution
      ! (5) using appropriate algorithms
      ! (6) splitting the long expressions using the continuation marker '&'
      ! (7) making meaningful variable names

! debuging programs
      ! breakpoint, stepping, watch points, read or write (disp)
      ! gdb: gnu debugger comes with linux operating system
      ! (1) break, run, cont, next, step
      ! dbx: stop, stop in, stop at, run, cont, next, step











    contains
  function area_of_circle(r)
  ! this fucntion compute the area of a circle with radius r
    implicit None
  ! fucntion results
    real::area_of_circle
  ! dummy arguments
    real::r
  ! local variables
    real::pi
    pi=4*atan(1.0)
    area_of_circle=pi*r**2
  end function area_of_circle

  ! recursive precedures
    recursive function myfacrotial (n) result(fac)
      ! computes the factorial of n(n!)
      implicit None
      ! fucntion results
      integer::fac
      ! dummy arguments
      integer,intent(in)::n
      select case(n)
      case (0:1)
        fac=1
      case default
        fac=n*myfacrotial(n-1)
      end select
    end function myfacrotial

! test for internal procedures
subroutine swap1(x,y)
  implicit None
  real::x,y,temp
  temp=x
  x=y
  y=temp
end subroutine swap1

end program addNumbers



! declaring the subroutines for program

subroutine fillarray(a)
  implicit None
  integer,dimension(5),intent(inout)::a ! specify the argument will be passed to outside world( also in and inout)
  ! local variables
  integer ::i

  do i=1,5
    a(i)=i
  end do
end subroutine fillarray

subroutine printarray(a)
  implicit None
  integer,dimension(5)::a
  integer::i
  do i=1,5
    print *,a(i)
  end do
end subroutine printarray

subroutine fillarray1(a)
  implicit None
  integer,dimension(:),intent(out)::a ! specify the argument will be passed to outside world( also in and inout)
  ! local variables
  integer ::i,arr_size
  arr_size=size(a)
  do i=1,arr_size
    a(i)=i
  end do
end subroutine fillarray1

subroutine printarray1(a)
  implicit None
  integer,dimension(:)::a
  integer::i,arr_size
  do i=1,arr_size
    print *,a(i)
  end do
end subroutine printarray1

subroutine swap(x,y)
  implicit None
  real::x,y,temp
  temp=x
  x=y
  y=temp
end subroutine swap

subroutine intent_example(a,b,c,d)
  implicit None
  ! dummy arguments
  real,intent(in)::a
  real,intent(in)::b
  real,intent(in)::c
  real,intent(out)::d

  d=b*b-4.0*a*c
end subroutine intent_example



! test for other module avilable
