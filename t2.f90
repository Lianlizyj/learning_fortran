program addNumbers
! this is a simple program adds two numbers
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
