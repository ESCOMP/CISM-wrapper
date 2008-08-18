!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP
! !ROUTINE: glc
! !INTERFACE:

#ifdef SINGLE_EXEC
 subroutine ccsm_glc()
#else
 program glc
#endif

! !DESCRIPTION:
!  This is the main driver for the CCSM ice sheet model component.
!
! !REVISION HISTORY:
!  SVN:$Id: POP.F90 2290 2006-10-25 18:23:10Z njn01 $
!  Adapted by William Lipscomb from POP.F90

! !USES:

#ifdef SINGLE_EXEC
   use MPH_module, only : MPH_get_argument
#endif

   use glc_InitMod, only: glc_initialize
   use glc_RunMod, only: glc_run
   use glc_FinalMod, only: glc_final
   use glc_kinds_mod
   use glc_communicate, only: my_task, master_task
   use glc_exit_mod
   use glc_domain, only: distrb_info
   use glc_timers
   use glc_ErrorMod   
   use glc_time_management, only: init_time_flag, check_time_flag, sigAbort,    &
       nsteps_run, stdout, sigExit, exit_glc, set_time_flag
   use glc_coupled, only: recv_from_coupler, send_to_coupler, lcoupled,  &
                          coupled_freq_iopt, coupled_freq
!!   use glc_output, only: output_driver  

!lipscomb - debug
   use shr_sys_mod
   use glc_constants, only: verbose

   implicit none

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (i4) :: &
!!!      timer_total,       &! timer number for total time
!!!      timer_step,        &! timer number for step
!!!      timer_out,         &! timer number for output driver
      ierr,              &! error flag
!lipscomb - added fcpl_ts here
      fcpl_ts,           &! flag id for coupled step
      fstop_now           ! flag id for stop_now flag

   integer (i4) :: &
      errorCode         ! error code


#ifdef SINGLE_EXEC
   integer (int_kind) :: &
      nThreads

   call MPH_get_argument("THREADS", nThreads, "glc")
#endif

!-----------------------------------------------------------------------
!
!  initialize the model run
!
!-----------------------------------------------------------------------

   errorCode = glc_Success

!lipscomb - debug
   if (verbose) then
      write(stdout,*) 'GLC: Initialize'
      call shr_sys_flush(6)
   endif

   call glc_initialize(errorCode)

   fstop_now = init_time_flag('stop_now')
   fcpl_ts   = init_time_flag('coupled_ts',                  &
                               freq_opt = coupled_freq_iopt,  &
                               freq     = coupled_freq)

!-----------------------------------------------------------------------
!
!  start up the main timer
!
!-----------------------------------------------------------------------

!lipscomb - debug
   if (verbose) then
      write(stdout,*) 'GLC: timer_total, timer_step =', timer_total, timer_step
      call shr_sys_flush(6)
   endif

!!!   call get_glc_timer(timer_step,'STEP'  , 1, distrb_info%nprocs)
!!!   call get_glc_timer(timer_total,'TOTAL' , 1, distrb_info%nprocs)
   call glc_timer_start(timer_total)

!-----------------------------------------------------------------------
!
!  advance the model in time
!
!-----------------------------------------------------------------------

   if (verbose) then
      write(stdout,*) 'GLC: Begin main loop'
      call shr_sys_flush(6)
   endif

   advance: do while (.not. check_time_flag(fstop_now))

!lipscomb - debug
    if (verbose) then
       write(stdout,*) 'GLC timestep, nsteps_run =', nsteps_run
       call shr_sys_flush(6)
    endif

!-----------------------------------------------------------------------
!  send and receive state variables at appropriate intervals
!-----------------------------------------------------------------------

!lipscomb - Change to ifdef?
      if (lcoupled) then

         if (check_time_flag(fcpl_ts)) then

!lipscomb - debug
            if (verbose) then
               write(stdout,*) 'GLC: Send message to coupler'
               call shr_sys_flush(6)
            endif

            call glc_timer_stop  (timer_recv_to_send)
            call glc_timer_start (timer_send_to_cpl)

            call send_to_coupler

         endif  ! coupling step

         if (check_time_flag(fcpl_ts) .or. nsteps_run==0) then

            if (verbose) then
               write(stdout,*) 'GLC: Recv message from coupler'
               call shr_sys_flush(6)
            endif

            call glc_timer_stop  (timer_send_to_cpl) 
            call glc_timer_start (timer_send_to_recv)
            call glc_timer_stop  (timer_send_to_recv) 
            call glc_timer_start (timer_recv_from_cpl)

            call recv_from_coupler

         call glc_timer_stop  (timer_recv_from_cpl)
         call glc_timer_start (timer_recv_to_send)

         endif ! coupling step or 1st step

      endif   ! lcoupled

!-----------------------------------------------------------------------
!  run the model 
!-----------------------------------------------------------------------

      if (verbose) then
         write(stdout,*) 'GLC: Take a step'
         call shr_sys_flush(6)
      endif

      call glc_timer_start(timer_step)
      call glc_run
      call glc_timer_stop(timer_step)

      if (verbose) then
         write(stdout,*) 'GLC: Took step'
         call shr_sys_flush(6)
      endif

      if (check_time_flag(fstop_now)) exit advance

!-----------------------------------------------------------------------
!
!     write restart dumps and archiving
!
!-----------------------------------------------------------------------

!lipscomb - to do - May want to uncomment later
!!      call glc_timer_start(timer_out)
!!      call output_driver
!!      call glc_timer_stop (timer_out)

   enddo advance

   if (verbose) then
      write(stdout,*) 'GLC: Exiting'
      call shr_sys_flush(6)
   endif

!-----------------------------------------------------------------------
!
!  write an end restart if we are through the stepping loop 
!  without an error
!
!-----------------------------------------------------------------------

!lipscomb - to be added?

!-----------------------------------------------------------------------
!
!  print timing information and clean up various environments if 
!  they have been used
!
!-----------------------------------------------------------------------

   call glc_timer_stop(timer_total)

   call glc_final(errorCode)

!-----------------------------------------------------------------------
!EOC

#ifdef SINGLE_EXEC
 end subroutine ccsm_glc
#else
 end program glc
#endif

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
