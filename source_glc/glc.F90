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
!  WHL, May 2007: Adapted from POP.F90 in POP 2.0.

! !USES:

#ifdef SINGLE_EXEC
   use MPH_module, only : MPH_get_argument
#endif


   use glc_InitMod, only: glc_initialize
   use glc_RunMod, only: glc_run
   use glc_FinalMod, only: glc_final
   use glc_kinds_mod, only: int_kind, i4, r8
   use glc_communicate, only: my_task, master_task
   use glc_exit_mod
   use glc_domain, only: distrb_info
   use glc_timers, only: glc_timer_print_all, get_glc_timer, glc_timer_start, glc_timer_stop
   use glc_Kindsmod
   use glc_ErrorMod   
   use glc_time_management, only: init_time_flag, check_time_flag, sigAbort,    &
       nsteps_run, stdout, sigExit, exit_glc, set_time_flag
   use glc_forcing_coupled, only: lcoupled
   use glc_output, only: output_driver  

   implicit none

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      timer_total,       &! timer number for total time
      timer_step,        &! timer number for step
      timer_out,         &! timer number for output driver
      ierr,              &! error flag
      fstop_now           ! flag id for stop_now flag

   integer (i4) :: &
      errorCode         ! error code


#ifdef SINGLE_EXEC
   integer (int_kind) :: &
      nThreads
   write (*,*) 'whl - Starting glc'
   call MPH_get_argument("THREADS", nThreads, "glc")
#ifdef _OPENMP
   call OMP_SET_NUM_THREADS(nThreads)
#endif
#endif

!-----------------------------------------------------------------------
!
!  initialize the model run
!
!-----------------------------------------------------------------------

   errorCode = glc_Success

   call glc_initialize(errorCode)

   fstop_now = init_time_flag('stop_now')

!-----------------------------------------------------------------------
!
!  start up the main timer
!
!-----------------------------------------------------------------------

   call get_glc_timer(timer_step,'STEP',1,distrb_info%nprocs)
   call get_glc_timer(timer_out,'OUTPUT',1,distrb_info%nprocs)

   call get_glc_timer(timer_total,'TOTAL',1,distrb_info%nprocs)
   call glc_timer_start(timer_total)


!-----------------------------------------------------------------------
!
!  advance the model in time
!
!-----------------------------------------------------------------------

   advance: do while (.not. check_time_flag(fstop_now))

      call glc_timer_start(timer_step)

      call glc_run

      call glc_timer_stop(timer_step)

      if (lcoupled .and. check_time_flag(fstop_now)) exit advance

!-----------------------------------------------------------------------
!
!     write restart dumps and archiving
!
!-----------------------------------------------------------------------

      call glc_timer_start(timer_out)
      call output_driver
      call glc_timer_stop (timer_out)

   enddo advance

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
