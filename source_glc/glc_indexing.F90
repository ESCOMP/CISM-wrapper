!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module glc_indexing

  !BOP
  ! !MODULE: glc_indexing

  ! !DESCRIPTION:
  ! Contains information about the indexing of the points owned by each processor.
  !
  ! This includes local indices (translation between (i,j) and a scalar 1..n) and global
  ! indices (unique indices across all procs).
  !
  ! Also contains subroutines for translating arrays between (i,j) and scalar 1..n
  !
  ! There are separate mappings for each ice sheet instance. In this module, "ice sheet
  ! instance" refers to a specific ice sheet, such as Greenland or Antarctica (following
  ! the meaning of "instance" in the CISM code), NOT an instance in the multi-instance
  ! sense as used for data assimilation, etc.

#include "shr_assert.h"
  use shr_log_mod, only : errMsg => shr_log_errMsg
  use shr_kind_mod, only: R8=>SHR_KIND_R8

  implicit none
  private
  save

  ! !PUBLIC ROUTINES:
  public :: allocate_indices
  public :: init_indices_one_icesheet
  public :: get_nx
  public :: get_ny
  public :: get_npts
  public :: get_nx_tot
  public :: get_ny_tot
  public :: get_npts_tot
  public :: get_nzocn
  public :: local_to_global_indices
  public :: vector_to_spatial
  public :: spatial_to_vector

  ! !PRIVATE TYPES:
  type :: indices_type
     private

     integer :: nx       ! number of columns owned by this proc
     integer :: ny       ! number of rows owned by this proc
     integer :: npts     ! total number of points owned by this proc
     integer :: nx_tot   ! total number of columns in full grid (all procs)
     integer :: ny_tot   ! total number of rows in full grid (all procs)
     integer :: npts_tot ! total number of points in full grid (all procs)
     integer :: nzocn    ! number of ocean levels for ocean coupling fields

     integer, allocatable :: local_indices(:,:)  ! mapping from (i,j) to 1..npts
     integer, allocatable :: global_indices(:,:) ! unique indices across all procs (matches indexing on mapping files)

   contains
     procedure, private :: init  ! initialize this instance
  end type indices_type

  ! !PRIVATE MODULE VARIABLES:
  type(indices_type), allocatable, private :: indices(:)  ! There will be one instance of this type for each ice sheet instance

contains

  !-----------------------------------------------------------------------
  subroutine init(this, instance_index, params)
    !
    ! !DESCRIPTION:
    ! Initialize indices for this indices_type object
    !
    ! Note that the global indexing needs to match the indexing on the SCRIP grid file
    ! that is used to generate GLC mapping files for the coupler.
    !
    ! !USES:
    use glad_main, only : glad_params, glad_get_grid_size, glad_get_grid_indices, glad_get_nzocn
    !
    ! !ARGUMENTS:
    class(indices_type), intent(inout) :: this
    integer, intent(in) :: instance_index  ! index of current ice sheet
    type(glad_params), intent(in) :: params
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'init'
    !-----------------------------------------------------------------------

    call glad_get_grid_size(params, instance_index, &
         ewn = this%nx, nsn = this%ny, npts = this%npts, &
         ewn_tot = this%nx_tot, nsn_tot = this%ny_tot, npts_tot = this%npts_tot)

    call glad_get_nzocn(params, instance_index, this%nzocn)

    allocate(this%local_indices(this%nx, this%ny))
    allocate(this%global_indices(this%nx, this%ny))

    call glad_get_grid_indices(params, instance_index, this%global_indices, this%local_indices)

  end subroutine init

  !-----------------------------------------------------------------------
  subroutine allocate_indices(num_icesheets)
    !
    ! !DESCRIPTION:
    ! Allocate the indices array for the number of ice sheet instances
    !
    ! !ARGUMENTS:
    integer, intent(in) :: num_icesheets  ! number of ice sheet instances in this run
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'allocate_indices'
    !-----------------------------------------------------------------------

    allocate(indices(num_icesheets))

  end subroutine allocate_indices

  !-----------------------------------------------------------------------
  subroutine init_indices_one_icesheet(instance_index, params)
    !
    ! !DESCRIPTION:
    ! Initialize indices for one ice sheet
    !
    ! !USES:
    use glad_main, only : glad_params
    !
    ! !ARGUMENTS:
    integer, intent(in) :: instance_index  ! index of current ice sheet
    type(glad_params), intent(in) :: params
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'init_indices_one_icesheet'
    !-----------------------------------------------------------------------

    call indices(instance_index)%init(instance_index, params)

  end subroutine init_indices_one_icesheet

  !-----------------------------------------------------------------------
  function get_nx(instance_index) result(nx)
    !
    ! !DESCRIPTION:
    ! Get nx for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: nx  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_nx'
    !-----------------------------------------------------------------------

    nx = indices(instance_index)%nx

  end function get_nx

  !-----------------------------------------------------------------------
  function get_ny(instance_index) result(ny)
    !
    ! !DESCRIPTION:
    ! Get ny for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: ny  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_ny'
    !-----------------------------------------------------------------------

    ny = indices(instance_index)%ny

  end function get_ny

  !-----------------------------------------------------------------------
  function get_npts(instance_index) result(npts)
    !
    ! !DESCRIPTION:
    ! Get npts for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: npts  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_npts'
    !-----------------------------------------------------------------------

    npts = indices(instance_index)%npts

  end function get_npts

  !-----------------------------------------------------------------------
  function get_nx_tot(instance_index) result(nx_tot)
    !
    ! !DESCRIPTION:
    ! Get nx_tot for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: nx_tot  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_nx_tot'
    !-----------------------------------------------------------------------

    nx_tot = indices(instance_index)%nx_tot

  end function get_nx_tot

  !-----------------------------------------------------------------------
  function get_ny_tot(instance_index) result(ny_tot)
    !
    ! !DESCRIPTION:
    ! Get ny_tot for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: ny_tot  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_ny_tot'
    !-----------------------------------------------------------------------

    ny_tot = indices(instance_index)%ny_tot

  end function get_ny_tot

  !-----------------------------------------------------------------------
  function get_npts_tot(instance_index) result(npts_tot)
    !
    ! !DESCRIPTION:
    ! Get npts_tot for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: npts_tot  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_npts_tot'
    !-----------------------------------------------------------------------

    npts_tot = indices(instance_index)%npts_tot

  end function get_npts_tot

  !-----------------------------------------------------------------------
  function get_nzocn(instance_index) result(nzocn)
    !
    ! !DESCRIPTION:
    ! Get nzocn for the given ice sheet instance
    !
    ! !ARGUMENTS:
    integer :: nzocn  ! function result
    integer, intent(in) :: instance_index
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_nzocn'
    !-----------------------------------------------------------------------

    nzocn = indices(instance_index)%nzocn

  end function get_nzocn

  !-----------------------------------------------------------------------
  function local_to_global_indices(instance_index) result(gindex)
    !
    ! !DESCRIPTION:
    ! Returns an array that maps local indices to global indices
    !
    ! gindex(n) gives the global index corresponding to local index n
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    integer, allocatable :: gindex(:)  ! function result
    integer, intent(in) :: instance_index  ! index of current ice sheet
    !
    ! !LOCAL VARIABLES:
    integer :: npts, nx, ny
    integer :: i, j, n

    character(len=*), parameter :: subname = 'local_to_global_indices'
    !-----------------------------------------------------------------------

    npts = indices(instance_index)%npts
    nx = indices(instance_index)%nx
    ny = indices(instance_index)%ny

    allocate(gindex(npts))
    do j = 1,ny
       do i = 1,nx
          n = indices(instance_index)%local_indices(i,j)
          gindex(n) = indices(instance_index)%global_indices(i,j)
       end do
    end do

  end function local_to_global_indices

  !-----------------------------------------------------------------------
  subroutine vector_to_spatial(instance_index, arr_vector, arr_spatial)
    !
    ! !DESCRIPTION:
    ! Convert a vector array (1..n) to a spatial array (i,j)
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    integer, intent(in) :: instance_index  ! index of current ice sheet
    real(r8), intent(in) :: arr_vector(:)
    real(r8), intent(out) :: arr_spatial(:,:)
    !
    ! !LOCAL VARIABLES:
    integer :: npts, nx, ny
    integer :: i, j, n

    character(len=*), parameter :: subname = 'vector_to_spatial'
    !-----------------------------------------------------------------------

    npts = indices(instance_index)%npts
    nx = indices(instance_index)%nx
    ny = indices(instance_index)%ny

    SHR_ASSERT_ALL((ubound(arr_vector) == (/npts/)), errMsg(__FILE__, __LINE__))
    SHR_ASSERT_ALL((ubound(arr_spatial) == (/nx, ny/)), errMsg(__FILE__, __LINE__))

    do j = 1, ny
       do i = 1, nx
          n = indices(instance_index)%local_indices(i,j)
          arr_spatial(i,j) = arr_vector(n)
       end do
    end do

  end subroutine vector_to_spatial

  !-----------------------------------------------------------------------
  subroutine spatial_to_vector(instance_index, arr_spatial, arr_vector)
    !
    ! !DESCRIPTION:
    ! Convert a spatial array (i,j) to a vector array (1..n)
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    integer, intent(in) :: instance_index  ! index of current ice sheet
    real(r8), intent(in) :: arr_spatial(:,:)
    real(r8), intent(out) :: arr_vector(:)
    !
    ! !LOCAL VARIABLES:
    integer :: npts, nx, ny
    integer :: i, j, n

    character(len=*), parameter :: subname = 'spatial_to_vector'
    !-----------------------------------------------------------------------

    npts = indices(instance_index)%npts
    nx = indices(instance_index)%nx
    ny = indices(instance_index)%ny

    SHR_ASSERT_ALL((ubound(arr_spatial) == (/nx, ny/)), errMsg(__FILE__, __LINE__))
    SHR_ASSERT_ALL((ubound(arr_vector) == (/npts/)), errMsg(__FILE__, __LINE__))

    do j = 1, ny
       do i = 1, nx
          n = indices(instance_index)%local_indices(i,j)
          arr_vector(n) = arr_spatial(i,j)
       end do
    end do

  end subroutine spatial_to_vector


end module glc_indexing
