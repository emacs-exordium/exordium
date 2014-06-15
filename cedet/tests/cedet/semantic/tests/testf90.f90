! Test Fortran 90 statements
! David Engster <dengste@eml.cc>

program main
  use testmodule
  implicit none

  integer :: eineVar, foo1, foo2, ppp
  double precision, parameter :: bar, foo3
  complex :: honk
  DOUBLE PRECISION, parameter :: foopi=3.141
  integer, dimension(:,:) :: anArray
  integer :: foo4=5,foo5,foo6=42
  ! Old style declarations
  INTEGER fooOldStyleDecl
  integer :: foo7(:,:),foo8(5)

  type :: aType
     integer :: one,two
     complex :: three,four
     real :: five
  end type aType

  type(aType) :: theType

  ! completion of subroutines
  call external!-1-
  !#1# ( "externalPolySub" "externalSub" )
  call my!-2-
  !#2# ( "myModSub" "mySub" )

  ! type-aware completion of functions
  bar = m!-3-
  !#3# ( "myFunc" "myModFunc" )

  ! completion of variables
  eineVar = foo!-4-
  !#4# ( "foo1" "foo2" "foo4" "foo5" "foo6" "foo7" "foo8" "fooOldStyleDecl" )

  bar = foo!-5-
  !#5# ( "foo3" "foopi" )

  ! completion of type members
  theType%!-6-
  !#6# ( "one" "two" "three" "four" "five" )
  honk = theType%!-7-
  !#7# ( "four" "three" )

contains

  subroutine mySub(aaa,bbb,ccc)
    integer,intent(in) :: aaa,bbb
    real,intent(out) :: ccc
    
    ccc = aaa*bbb
  end subroutine mySub

  function myFunc(ddd,eee)
    integer :: ddd
    real :: eee
    double precision myFunc
    
    myFunc = ddd*e!-8-
    !#8# ( "eee" "eineVar" )
  end function myFunc
  
  integer function anotherFunc(fff)
    integer,intent(in),dimension(:) :: fff
    anotherFunc = fff(1)
  end function anotherFunc

end program main

