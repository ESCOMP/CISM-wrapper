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
!  Adapted by William Lipscomb from step_mod.F90 in POP 2.0 and from 
!   glint_example.F90 in GLIMMER
!
! !USES:

!lipscomb - These are from POP.
!lipscomb - to do - Some of these use statements may not be needed.
   use glc_kinds_mod
   use glc_timers
   use glc_time_management, only:  thour, tday,                      &
       time_to_do, freq_opt_nstep, time_manager, check_time_flag,    &
       init_time_flag, check_time_flag_freq, check_time_flag_freq_opt, eod
   use shr_sys_mod
   use glc_communicate, only: my_task, master_task
   use glc_io_types, only: stdout
   use glc_coupled, only: lcoupled

!lipscomb - added this one
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

   integer (i4) ::   &
      cpl_stop_now      ,&! flag id for stop_now flag
      tavg_flag           ! flag to access tavg frequencies

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
   use glc_global_fields   !lipscomb - to do - specify what fields are used
   use glc_glint, only: glc_glint_driver
   use glimmer_log
   use glint_global_interp
   use glint_example_clim

!lipscomb - debug
   use glc_constants, only: verbose, itest, jjtest

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local or common variables:
!
!-----------------------------------------------------------------------

!lipscomb - These variables are from POP.

   logical, save ::    &
      first_call = .true.,        &! flag for initializing timers
      first_global_budget = .true.

!lipscomb - The rest are from GLIMMER.
 
  character(fname_length) ::  & 
     paramfile   ,&! Name of the top-level configuration file
     climatefile   ! Name of climate configuration file
 
  ! Scalars which hold information about the global grid
 
  integer (i4) ::  &
     nx,ny        ,&! Size of global glc_grid 
     nxo,nyo        ! Size of global orography grid

  ! Scalar model outputs
 
  real(r8) ::      & 
     twin         ,&! Timestep-integrated input water flux (kg) 
     twout        ,&! Timestep-integrated output water flux (kg) 
     ice_vol        ! Total ice volume (m^3) 
 
  ! Other variables
 
  logical ::  &
     l_ice_tstep  ,&! true if ice timestep was done
     out            ! output flag

  integer (i4) ::  & 
     i,j            ! Array index counters 

!lipscomb-debug
   integer :: ig, jg, n

!-----------------------------------------------------------------------
!
!  if this is the first call to glc_run, start some timers 
!lipscomb - to do - timers to be added? 
!
!-----------------------------------------------------------------------

!lipscomb - debug
     if (verbose) then
        write(6,*) ' ' 
        write(6,*) 'In glc_run, first_call =', first_call
        call shr_sys_flush(6)
     endif

   if (first_call) then
      ! this line should set cpl_stop_now = 1 (flag id index)
      cpl_stop_now  = init_time_flag('stop_now',default=.false.)
      tavg_flag     = init_time_flag('tavg')      
      first_call = .false.
   endif

!-----------------------------------------------------------------------
!
!  Get climate information
!-----------------------------------------------------------------------

!lipscomb - debug
    if (verbose) then
       write (6,*) 'Run glint, time(days) =', tday
       call shr_sys_flush(6)
    endif

!lipscomb - This is from GLIMMER.  I think it is not needed here.

!!!    call get_grid_dims(climate%all_grid,nx,ny) ! Normal global grid
!!!    nxo=200 ; nyo=100                          ! Example grid used for orographic output

!!!   call example_climate (climate,  &
!!!                         precip,   &
!!!                         temp,     &
!!!                         thour))

!-----------------------------------------------------------------------
!
!  Take one GLINT time step 
!  Note: For SMB scheme, tsfc = ground surface temperature (Celsius)
!                        qice = flux of new glacier ice (kg/m^2s)
!
!        For PDD scheme, tsfc = 2m reference temperature (Celsius)
!                        qice = precipitation (kg/m^2/s)
!-----------------------------------------------------------------------

!lipscomb - Modify for restarts?  Do not call unless coupler fluxes from CLM are real.
!lipscomb - return ice_tstep?
     if (glc_smb) then

!lipscomb - debug
         if (verbose) then
            write(6,*) 'Surface mass balance is passed in'
            write(6,*) 'Call glc_glint_driver'
            call shr_sys_flush(6)
         endif

         call glc_glint_driver (ice_sheet,       nint(thour),   &
                                tsfc,            qice,    &
                                topo,                     &
                                gfrac,           gthck,   &
                                gtopo,           ghflx,   &
                                groff,           l_ice_tstep)

!lipscomb - debug

         if (verbose) then
            write(6,*) ' '
            write(6,*) 'Global fields from GLINT:'
            do n = 1, glc_nec
               ig = itest
               jg = jjtest   ! N to S global indexing as in GLINT
               write(6,*) ' '
               write(6,*) 'i, j, n =', ig, jg, n
               write(6,*) 'gfrac(n) =', gfrac(ig,jg,n)
               write(6,*) 'gthck(n) =', gthck(ig,jg,n)
               write(6,*) 'gtopo(n) =', gtopo(ig,jg,n)
               write(6,*) 'ghflx(n) =', ghflx(ig,jg,n)
               write(6,*) 'groff(n) =', groff(ig,jg,n)
            enddo
         endif

     else    ! use PDD scheme in GLIMMER

!lipscomb - to do - Need to test this option

!lipscomb - debug
       if (verbose) then 
          write(6,*) 'Using positive-degree-day scheme'
          write(6,*) 'Call glint driver'
          write(6,*) 'This has not been tested!'
          call shr_sys_flush(6)
       endif

!lipscomb - to do - need to return gtopo; not sure about gthck and ghflx 
         call glint (ice_sheet,                  &
                     nint(thour),                &
                     temp,                       &
                     precip,                     &
                     orog,                       &
                     output_flag     = out,      &
                     ice_frac        = ice_frac, &
                     water_out       = fw,       &
                     water_in        = fw_in,    &
                     total_water_in  = twin,     &
                     total_water_out = twout,    &
                     ice_volume      = ice_vol)

     endif   ! glc_smb

     if (thour > climate%total_years*climate%hours_in_year) then
        call set_time_flag(cpl_stop_now,.true.)
        write(6,*) 'Last time step; model should quit now'
        call shr_sys_flush(6)
     endif

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

   call time_manager (lcoupled)

!lipscomb - debug
   if (verbose) then
      write(6,*) 'Called time manager, end of glc_run'
      write(6,*) 'New thour =', thour
      call shr_sys_flush(6)
   endif

!-----------------------------------------------------------------------
!EOC

   end subroutine glc_run

!***********************************************************************

 end module glc_RunMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
