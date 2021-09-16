!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module glc_fields

!BOP
! !MODULE: glc_fields

  ! !DESCRIPTION:
  !  Holds coupling fields and other important data
  !
  ! There is a separate set of coupling fields for each ice sheet instance. In this
  ! module, "ice sheet instance" refers to a specific ice sheet, such as Greenland or
  ! Antarctica (following the meaning of "instance" in the CISM code), NOT an instance in
  ! the multi-instance sense as used for data assimilation, etc.

! !REVISION HISTORY:
! 
!  Author: William Lipscomb, LANL
!
! !USES:

  use glc_kinds_mod
  use glad_main, only: glad_params

  implicit none
  save

! !PUBLIC MEMBER FUNCTIONS:

  public :: allocate_cpl_bundles
  public :: glc_allocate_fields
  public :: glc_deallocate_fields

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

  type, private :: coupling_fields_type

     ! Fields received from CESM coupler

     real(r8),dimension(:,:), allocatable ::  &
          tsfc        ,&! surface temperature (Celsius)
                                ! received from coupler in Kelvin, must be converted
          qsmb          ! flux of new glacier ice (kg/m^2/s)

     real(r8),dimension(:,:,:), allocatable ::  &
          salinity    ,&! ocean salinity, per layer (g/kg)
          tocn          ! ocean temperature, per layer (K)

     ! output to coupler

     real(r8),dimension(:,:), allocatable ::  &
          ice_covered,&         ! whether each grid cell is ice-covered [0,1]
          topo                  ! glacier surface elevation (m)

     real(r8),dimension(:,:), allocatable :: &
          rofi       ,&! ice runoff (calving) flux (kg/m^2/s)
          rofl       ,&! ice runoff (calving) flux (kg/m^2/s)
          hflx         ! heat flux from glacier interior, positive down (W/m^2)

     real(r8),dimension(:,:), allocatable :: &
          ice_sheet_grid_mask  ! mask of ice sheet grid coverage

  end type coupling_fields_type

  ! cpl_bundles needs to be a target for the sake of override code in glc_export
  type(coupling_fields_type), allocatable, target :: cpl_bundles(:)
  type(glad_params) :: ice_sheet   ! Parameters relevant to all model instances

!EOP
!***********************************************************************

 contains

   !***********************************************************************
   !BOP
   ! !IROUTINE: allocate_cpl_bundles
   ! !INTERFACE:

   subroutine allocate_cpl_bundles (num_ice_sheets)

     ! !DESCRIPTION:
     !  Allocate the cpl_bundles array for the number of ice sheet instances

     ! !USES:
     use glc_kinds_mod

     ! !INPUT/OUTPUT PARAMETERS:

     integer (i4), intent(in) :: &
          num_ice_sheets  ! number of ice sheet instances in this run

     !EOP
     !BOC

     allocate(cpl_bundles(num_ice_sheets))

   end subroutine allocate_cpl_bundles

!***********************************************************************
!BOP
! !IROUTINE: glc_allocate_fields
! !INTERFACE:

 subroutine glc_allocate_fields (instance_index, nx, ny)

! !DESCRIPTION:
!  Allocate coupling fields for one ice sheet
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !USES:
   use glc_kinds_mod

! !INPUT/OUTPUT PARAMETERS:

   integer (i4), intent(in) :: &
        instance_index   ! index of current ice sheet

   integer (i4), intent(in) :: &
        nx, ny           ! grid dimensions

!EOP
!BOC

   ! from coupler
   allocate(cpl_bundles(instance_index)%tsfc(nx,ny))
   allocate(cpl_bundles(instance_index)%qsmb(nx,ny))
   ! ktc temporary
   allocate(cpl_bundles(instance_index)%salinity(1,nx,ny))
   allocate(cpl_bundles(instance_index)%tocn(1,nx,ny))

   ! to coupler
   allocate(cpl_bundles(instance_index)%ice_covered(nx,ny))
   allocate(cpl_bundles(instance_index)%topo(nx,ny))
   allocate(cpl_bundles(instance_index)%rofi(nx,ny))
   allocate(cpl_bundles(instance_index)%rofl(nx,ny))
   allocate(cpl_bundles(instance_index)%hflx(nx,ny))
   allocate(cpl_bundles(instance_index)%ice_sheet_grid_mask(nx,ny))
   
 end subroutine glc_allocate_fields

!***********************************************************************

!BOP
! !IROUTINE: glc_deallocate_fields
! !INTERFACE:

 subroutine glc_deallocate_fields(instance_index)

! !DESCRIPTION:
!  Deallocate coupling fields for one ice sheet
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

   ! !USES:
   use glc_kinds_mod

   ! !INPUT/OUTPUT PARAMETERS:

   integer (i4), intent(in) :: &
        instance_index   ! index of current ice sheet

!EOP
!BOC

   ! from coupler
   deallocate(cpl_bundles(instance_index)%tsfc)
   deallocate(cpl_bundles(instance_index)%qsmb)
   deallocate(cpl_bundles(instance_index)%salinity)
   deallocate(cpl_bundles(instance_index)%tocn)

   ! to coupler
   deallocate(cpl_bundles(instance_index)%ice_covered)
   deallocate(cpl_bundles(instance_index)%topo)
   deallocate(cpl_bundles(instance_index)%rofi)
   deallocate(cpl_bundles(instance_index)%rofl)
   deallocate(cpl_bundles(instance_index)%hflx)
   deallocate(cpl_bundles(instance_index)%ice_sheet_grid_mask)
  
   end subroutine glc_deallocate_fields

!***********************************************************************

 end module glc_fields

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
