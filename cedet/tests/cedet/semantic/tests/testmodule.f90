! For testing of Fortran 90 modules
! David Engster <dengste@eml.cc>

module testmodule
  implicit none

  type :: myModType
     integer :: one,two
     real :: three
  end type myType

  ! interface for external sub
  interface
     subroutine externalSub(arg1,arg2,arg3)
       integer :: arg1
       type(myModType) :: arg2
       real :: arg3
     end subroutine externalSub
  end interface

  ! interface for polymorphism
  interface externalPolySub
     subroutine polySub_1(arg1,arg2)
       integer :: arg1,arg2
     end subroutine polySub_1

     subroutine polySub_2(arg1,arg2)
       real :: arg1,arg2
     end subroutine polySub_2
  end interface polySub


contains
  
  double precision function myModFunc(arg1,arg2)
    double precision :: arg1,arg2
    
    myModFunc = arg1*arg2
  end function myModFunc

  subroutine myModSub(arg1,arg2)
    integer,intent(in) :: arg1
    integer,intent(out) :: arg2

    arg2=2*arg1
  end subroutine myModSub

end module testmodule
