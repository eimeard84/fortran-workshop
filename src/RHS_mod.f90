module RHS_mod
  use :: types_mod, only: dp

  implicit none
  ! everything is private unless otherwise stated
  private
  public :: func
  ! Variables go here
  contains


  function func(j, x) result (d)
    implicit none

    integer, intent(in) :: j
    real (kind=dp) :: d
    real (kind=dp), dimension(:), intent(in) :: x

    !> Why the FUCK does a function just return ZERO regardless of input? -EMD, 2018.04.12
    d = 0.0e+00_dp
  end function



end module RHS_mod
