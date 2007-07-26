!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_params

!BOP
! !MODULE: glc_params

! !DESCRIPTION:
!  Holds parameters that are common to glc_InitMod and glc_RunMod
!
! !REVISION HISTORY:
! 
!  author William Lipscomb, LANL
!
! !USES:

   use glc_KindsMod
   use glint_example_clim, only: glex_climate
   use glint_main, only: glint_params

   implicit none
   save

! !PUBLIC MEMBER FUNCTIONS:

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

  type(glint_params) :: ice_sheet   ! Parameters relevant to all model instances, 
                                    ! i.e. pertaining to the global model 

  type(glex_climate) :: climate     ! Climate parameters and fields

  ! glint input fields, on global grid
  real(glc_r8),dimension(:,:),allocatable ::  & 
      temp        ,&! temperature     (deg C)
      precip      ,&! precipitation   (mm/s) 
      orog          ! orography       (m) 

  ! glint output fields
  ! These are all on the normal global grid, except for orog_out
 
  real(glc_r8),dimension(:,:),allocatable ::   &
      albedo      ,&! Fractional albedo
      orog_out    ,&! Output orography (m)
      ice_frac    ,&! Ice coverage fraction 
      fw          ,&! Freshwater output flux (mm/s)
      fw_in         ! Freshwater input flux (mm/s)
 
  ! arrays which hold information about the global grid
 
  real(glc_r8),dimension(:), allocatable ::  &
     lats_orog    ,&! Latitudes of global orography gridpoints
     lons_orog      ! Longitudes of global oropraphy gridpoints

  ! arrays which hold information about the ice model instances
 
  real(glc_r8),dimension(:,:), allocatable ::   &
      coverage    ,&! Coverage map for normal global grid
      cov_orog      ! Coverage map for orography grid

!lipscomb - This time is internal to glint.  Remove later and use CCSM time.
  integer(glc_i4) :: time           ! current time in hours

!EOP
!***********************************************************************

 end module glc_params

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
