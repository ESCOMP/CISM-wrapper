!lipscomb - Uncomment references to lcoupled?

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

   use glc_kinds_mod
   use glc_time_management, only:  thour, time_manager, check_time_flag, init_time_flag
   use shr_sys_mod
   use glc_communicate, only: my_task, master_task
   use glc_constants, only: stdout, glc_nec, glc_smb

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
   use glc_global_fields    !lipscomb - to do - specify what fields are used
   use glimmer_log
   use glint_global_interp
   use glint_example_clim

!lipscomb - debug
   use glimmer_paramets, only: itest, jjtest

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

!lipscomb - debug
   integer :: ig, jg, n

!-----------------------------------------------------------------------
!
!  if this is the first call to glc_run, start some timers 
!lipscomb - to do - timers to be added? 
!
!-----------------------------------------------------------------------

   if (first_call) then
      write(stdout,*) 'In glc_run, first_call =', first_call
      ! this line should set cpl_stop_now = 1 (flag id index)
      cpl_stop_now  = init_time_flag('stop_now',default=.false.)
      tavg_flag     = init_time_flag('tavg')      
      first_call = .false.
   endif

!-----------------------------------------------------------------------
!
!  Get climate information
!-----------------------------------------------------------------------

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
            write(stdout,*) 'Call glint, thour =', thour
            call shr_sys_flush(stdout)

!lipscomb - debug

            write(stdout,*) ' '
            write(stdout,*) 'Global fields from CLM to GLINT'
            do n = 1, glc_nec
               ig = itest
               jg = jjtest   ! N to S global indexing as in GLINT
               write(stdout,*) ' '
               write(stdout,*) 'i, j, n =', ig, jg, n
               write(stdout,*) 'tsfc(n) =', tsfc(ig,jg,n)
               write(stdout,*) 'topo(n) =', topo(ig,jg,n)
               write(stdout,*) 'qice(n) =', qice(ig,jg,n)
            enddo

         call glint (ice_sheet,       nint(thour),   &
                     temp,            precip,  &     ! temp, precip, orog are set to zero
                     orog,                     &
                     ice_tstep = l_ice_tstep,  & 
                     ccsm_smb_in = glc_smb,    & 
                     tsfc  = tsfc,     qice  = qice,    &
                     topo  = topo,                      &
                     gfrac = gfrac,    gtopo = gtopo,   &
                     grofi = grofi,    grofl = grofl,   &
                     ghflx = ghflx)

!lipscomb - debug

            write(stdout,*) ' '
            write(stdout,*) 'Global fields from GLINT to CLM:'
            do n = 1, glc_nec
               ig = itest
               jg = jjtest   ! N to S global indexing as in GLINT
               write(stdout,*) ' '
               write(stdout,*) 'i, j, n =', ig, jg, n
               write(stdout,*) 'gfrac(n) =', gfrac(ig,jg,n)
               write(stdout,*) 'gtopo(n) =', gtopo(ig,jg,n)
!               write(stdout,*) 'grofi(n) =', grofi(ig,jg,n)
!               write(stdout,*) 'grofl(n) =', grofl(ig,jg,n)
!               write(stdout,*) 'ghflx(n) =', ghflx(ig,jg,n)
            enddo

     else    ! use PDD scheme in GLIMMER

!lipscomb - to do - Need to test this option

!lipscomb - debug
          write(stdout,*) 'Using positive-degree-day scheme'
          write(stdout,*) 'Call glint driver'
          write(stdout,*) 'This has not been tested!'

         call glint (ice_sheet,                  &
                     nint(thour),                &
                     tsfc(:,:,1),                &  ! 2-m air temp
                     qice(:,:,1),                &  ! precip
                     orog,                       &
                     output_flag     = out,      &
                     ice_frac        = ice_frac, &
                     water_out       = fw,       &
                     water_in        = fw_in,    &
                     total_water_in  = twin,     &
                     total_water_out = twout,    &
                     ice_volume      = ice_vol)

     endif   ! glc_smb

!-----------------------------------------------------------------------
!
!  return if coupler has sent "stop now" signal
!
!-----------------------------------------------------------------------

!!   if (lcoupled .and. check_time_flag(cpl_stop_now) ) RETURN

!-----------------------------------------------------------------------
!
!  update timestep counter, set corresponding model time, set
!  time-dependent logical switches to determine program flow.
!
!-----------------------------------------------------------------------

   call time_manager

!lipscomb - debug
      write(stdout,*) 'Called time manager after glc timestep'
      write(stdout,*) 'New thour =', thour
      call shr_sys_flush(stdout)

!-----------------------------------------------------------------------
!EOC

   end subroutine glc_run

!***********************************************************************

 end module glc_RunMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
