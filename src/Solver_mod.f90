module Solver_mod
  use types_mod, only: dp
  use rhs_mod, only: func

  implicit none
  ! everything is private unless otherwise stated
  private
  public :: fd1d_heat_explicit
  ! Variables go here
  contains

  subroutine fd1d_heat_explicit(x, t, dt, cfl, h, h_new)
    implicit none

    real (kind=dp), intent(in) :: cfl
    real (kind=dp), intent(in) :: dt
    real (kind=dp), dimension(:), intent(in) :: h
    real (kind=dp), dimension(:), intent(out) :: h_new
    integer :: j
    !> This variable isn't used. -EMD 2018.04.12 
    real (kind=dp), intent(in) :: t
    real (kind=dp), dimension(:), intent(in) :: x
    real (kind=dp) :: f(size(x))

    do j = 1, size(x)
      f(j) = func(j, x)
    end do

    !> This is a really weird way of doing things - the boundary conditions are set before and after the loop? -EMD 2018.04.12
    h_new(1) = 0.0e+00_dp

    do j = 2, size(x) - 1
      != stencil readOnce, (reflexive(dim=1)) :: f 
      != stencil (centered(depth=1, dim=1)) :: h 
      h_new(j) = h(j) + dt*f(j) + cfl*(h(j-1)-2.0e+00_dp*h(j)+h(j+1))
    end do

! set the boundary conditions again
    h_new(1) = 90.0e+00_dp
    h_new(size(x)) = 70.0e+00_dp
  end subroutine



end module Solver_mod
