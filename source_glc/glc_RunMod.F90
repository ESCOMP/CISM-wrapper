!lipscomb - Uncomment references to lcoupled

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_RunMod

!BOP
! !MODULE: glc_RunMod

! !DESCRIPTION:
!  Contains the routine for stepping the model forward one timestep
!
! !REVISION HISTORY:
!  SVN:$Id: step_mod.F90 2019 2006-09-29 22:00:15Z njn01 $
!  WHL, May 2007: Adapted from step_mod.F90 in POP 2.0 and from 
!                 glint_example.F90 in GLIMMER
!
! !USES:

!lipscomb - These are from POP.  Not sure which are needed.
   use glc_KindsMod
   use glc_blocks, only: nx_block, ny_block, block, get_block
   use glc_domain_size
   use glc_domain, only: distrb_info, nblocks, bndy_info, blocks_glc
   use glc_timers, only: get_glc_timer, glc_timer_start, glc_timer_stop
   use glc_time_management, only:                                           &
       time_to_do, freq_opt_nstep, time_manager, check_time_flag,  &
       init_time_flag, check_time_flag_freq, check_time_flag_freq_opt, eod
   use glc_xdisplay, only: lxdisplay, nstep_xdisplay, display
   use shr_sys_mod
   use glc_communicate, only: my_task, master_task
   use glc_io_types, only: stdout
   use glc_forcing_coupled, only: lcoupled

!lipscomb - debug
   use shr_sys_mod, only: shr_sys_flush
   use glc_time_management, only: set_time_flag  ! to signal end of run

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_run

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

!EOP
!BOC
!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_run
! !INTERFACE:

 subroutine glc_run

! !DESCRIPTION:
!  This routine advances the simulation one timestep.
!
! !REVISION HISTORY:
!  same as module

! !USES:

   use glint_main
   use glc_params
   use glimmer_log
   use glint_global_interp
   use glint_example_clim

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local or common variables:
!
!-----------------------------------------------------------------------

!lipscomb - from POP - not sure which are needed
   integer (glc_i4) ::   &
      cpl_stop_now      ,&! flag id for stop_now flag
      tavg_flag         ,&! flag to access tavg frequencies
      iblock              ! block counter

   logical (glc_Logical), save ::    &
      first_call = .true.,        &! flag for initializing timers
      first_global_budget = .true.

   type (block) ::        &
      this_block          ! block information for current block


!lipscomb - from GLIMMER.
 
  character(fname_length) ::  & 
      paramfile   ,&! Name of the top-level configuration file
      climatefile   ! Name of climate configuration file
 
  ! Scalars which hold information about the global grid
 
  integer (glc_i4) ::  &
     nx,ny        ,&! Size of normal global grid 
     nxo,nyo        ! Size of global orography grid

  ! Scalar model outputs
 
  real(glc_r8) ::      & 
     twin         ,&! Timestep-integrated input water flux (kg) 
     twout        ,&! Timestep-integrated output water flux (kg) 
     ice_vol        ! Total ice volume (m^3) 
 
  ! Other variables
 
  logical :: out    ! Outputs set flag 
 
  integer (glc_i4) ::  & 
     i,j            ! Array index counters 

!-----------------------------------------------------------------------
!
!  if this is the first call to glc_run, start some timers
!
!-----------------------------------------------------------------------

!lipscomb - uncomment later?
!!   if (first_call) then
      ! this line should set cpl_stop_now = 1 (flag id index)
      cpl_stop_now  = init_time_flag('stop_now',default=.false.)
!!      tavg_flag     = init_time_flag('tavg')      
!!      first_call = .false.
!!   endif

!-----------------------------------------------------------------------
!
!  Get climate information
!-----------------------------------------------------------------------

!lipscomb - debug
!!    write (6,*) 'Run glint, time(days) =', time/24.0
!!    call shr_sys_flush(6)


!lipscomb - This is done at initialization.  Not sure it is needed here.
    call get_grid_dims(climate%all_grid,nx,ny) ! Normal global grid
    nxo=200 ; nyo=100                          ! Example grid used for orographic output

   call example_climate (climate,  &
                         precip,   &
                         temp,     &
                         real(time,glc_r8))

!-----------------------------------------------------------------------
!
!  Take one GLINT time step 
!
!-----------------------------------------------------------------------

!whl - It should be possible to call glint without the optional arguments
!whl   such as zonwind and merwind.  However, subroutine glint will
!whl   crash if these arguments are not present.  Fix glint later, but
!whl   include the arguments for now.
      
   call glint (ice_sheet,                  &
               time,                       &
               temp,                       &
               precip,                     &
               orog,                       &
               output_flag     = out,      &
               albedo          = albedo,   &
               ice_frac        = ice_frac, &
               water_out       = fw,       &
               water_in        = fw_in,    &
               total_water_in  = twin,     &
               total_water_out = twout,    &
               ice_volume      = ice_vol)

     if (time > climate%total_years*climate%hours_in_year) then
        call set_time_flag(cpl_stop_now,.true.)
        write(6,*) 'Last time step; model should quit now'
        call shr_sys_flush(6)
     endif

     time = time + climate%climate_tstep   !lipscomb - just for now

!-----------------------------------------------------------------------
!
!  return if coupler has sent "stop now" signal
!
!-----------------------------------------------------------------------

   if (lcoupled .and. check_time_flag(cpl_stop_now) ) RETURN

!-----------------------------------------------------------------------
!
!  update timestep counter, set corresponding model time, set
!  time-dependent logical switches to determine program flow.
!
!-----------------------------------------------------------------------
!lipscomb - uncomment later
!!   call time_manager (lcoupled)


!-----------------------------------------------------------------------
!EOC

   end subroutine glc_run

!***********************************************************************

 end module glc_RunMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
