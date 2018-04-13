module IO_mod
  use :: types_mod, only: dp
  use netcdf

  implicit none
  ! everything is private unless otherwise stated
  private
  public :: r8mat_write
  public :: r8vec_linspace
  public :: r8vec_write
  ! Variables go here
  contains

  subroutine r8mat_write(output_filename, table, x, t)
    implicit none

    integer :: m, n

    character (len=*), intent(in) :: output_filename
    integer :: ncid, tid, xid, table_id, x_dimid, t_dimid
    integer :: ierr
    real (kind=dp), dimension(:,:), intent(in) :: table
    real (kind=dp), dimension(:), intent(in) :: x
    real (kind=dp), dimension(:), intent(in) :: t

    m = size( table(:,:), 1 ) ! This is the space dimension
    n = size( table(:,:), 2 ) ! This is the time dimension

    ierr = NF90_CREATE( output_filename, NF90_CLOBBER, ncid)
      ierr = NF90_DEF_DIM( ncid, "x", m, x_dimid )
      ierr = NF90_DEF_DIM( ncid, "t", n, t_dimid ) 
      ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "Purpose", "Fortran workshop") 
      ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "Name", "Eimear Dunne") 
      ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "Institution", "University of Cambridge")
  
      ierr = NF90_DEF_VAR( ncid, "x-range", NF90_REAL8, x_dimid, xid )
      ierr = NF90_PUT_ATT( ncid, xid, "Units", "metres" )
      ierr = NF90_DEF_VAR( ncid, "t-range", NF90_REAL8, t_dimid, tid )
      ierr = NF90_PUT_ATT( ncid, tid, "Units", "seconds" )
      ierr = NF90_DEF_VAR( ncid, "solution", NF90_REAL8, [x_dimid, t_dimid], table_id )
      ierr = NF90_PUT_ATT( ncid, table_id, "Units", "Celsius" )
    ierr = NF90_ENDDEF( ncid )
      ierr = NF90_PUT_VAR( ncid, xid, x )
      ierr = NF90_PUT_VAR( ncid, tid, t ) 
      ierr = NF90_PUT_VAR( ncid, table_id, table )
    ierr = NF90_CLOSE( ncid )

  end subroutine r8mat_write

  subroutine r8vec_linspace(a_first, a_last, a)

    implicit none

    real (kind=dp), dimension(:), intent(out) :: a
    real (kind=dp), intent(in) :: a_first
    real (kind=dp), intent(in) :: a_last
    integer :: i

    do i = 1, size(a)
      a(i) = (real(size(a)-i,kind=dp)*a_first+real(i-1,kind=dp)*a_last)/ &
        real(size(a)-1, kind=dp)
    end do

  end subroutine r8vec_linspace

  subroutine r8vec_write(output_filename, x)

    implicit none

    integer :: j
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    real (kind=dp), dimension(:), intent(in) :: x

    output_unit_id = 11
    open (unit=output_unit_id, file=output_filename, status='replace')

    do j = 1, size(x)
      write (output_unit_id, '(2x,g24.16)') x(j)
    end do

    close (unit=output_unit_id)
  end subroutine r8vec_write



end module IO_mod
