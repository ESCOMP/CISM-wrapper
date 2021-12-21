!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_InitMod

!BOP
! !MODULE: glc_InitMod
! !DESCRIPTION:
!  This module contains the glc initialization method and initializes
!  everything needed by a glc simulation.  Primarily it is a driver
!  that calls individual initialization routines for each glc module.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  SVN:$Id: POP_InitMod.F90 808 2006-04-28 17:06:38Z njn01 $
!  Adapted by William Lipscomb from POP_InitMod.F90

! !USES:

   use glc_kinds_mod
   use glc_communicate, only: my_task, master_task
   use glc_broadcast,   only: broadcast_scalar, broadcast_array
   use glc_time_management, only: iyear0, imonth0, iday0, elapsed_days0,  &
                                  iyear,  imonth,  iday,  elapsed_days,   &
                                  ihour,  iminute, isecond, nsteps_total, &
                                  ymd2eday, eday2ymd, runtype
   use glc_constants, only: stdout, zero_gcm_fluxes_for_all_icesheets, test_coupling, enable_frac_overrides, &
                            max_icesheets, num_icesheets, icesheet_names
   use glc_io,        only: glc_io_read_restart_time
   use glc_files,     only: nml_filename
   use glc_exit_mod, only : exit_glc, sigAbort
   use shr_kind_mod,  only: CL=>SHR_KIND_CL
   use shr_sys_mod, only: shr_sys_flush

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_initialize

!EOP
!BOC
!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------

   character(*), parameter :: icesheet_unset = 'UNSET'

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_initialize
! !INTERFACE:

 subroutine glc_initialize(EClock)

! !DESCRIPTION:
!  This routine is the initialization driver that initializes a glc run
!  by calling individual module initialization routines.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !USES:
   use glad_main

   use glc_fields, only: allocate_cpl_bundles, glc_allocate_fields, ice_sheet, cpl_bundles
   use glc_override_frac, only: init_glc_frac_overrides
   use glc_constants
   use glc_communicate, only: init_communicate
   use glc_time_management, only: init_time1, init_time2, dtt, ihour
   use glimmer_log
   use glc_route_ice_runoff, only: set_routing
   use glc_history, only : allocate_history, glc_history_init, glc_history_write
   use glc_indexing, only : allocate_indices, init_indices_one_icesheet, get_nx, get_ny, get_nzocn
   use shr_file_mod, only : shr_file_getunit, shr_file_freeunit
   use esmf, only : ESMF_Clock

! !INPUT/OUTPUT PARAMETERS:

   type(ESMF_Clock),     intent(in)    :: EClock

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

  character(fname_length) ::  &
      paramfile_base        ! Base name of the top-level configuration file (actual param
                            ! file names are this base name plus ".icesheet.config" for
                            ! the given ice sheet).

  character(fname_length), allocatable :: paramfiles(:)  ! Actual param file names

  character(fname_length), allocatable :: &
      cesm_restart_files(:)  ! Names of the files to be used for a restart (one per ice sheet)

  character(CL) :: &
       ice_flux_routing  ! Code for how solid ice should be routed to ocean or sea ice

  ! Scalars which hold information about the global grid --------------

  integer (i4) ::  &
      i,j              ! Array index counters

  integer (i4) :: &
       ns              ! Ice sheet instance number

  integer (i4) :: &
      nml_error        ! namelist i/o error flag

  integer (i4) :: &
      nhour_glad      ! number of hours since start of complete glad/CISM run

  integer (i4) :: &
       nhour_glad_this_icesheet ! nhour_glad for the current ice sheet, for comparison

  integer (i4) :: &
       av_start_time_restart  ! glad averaging start time

  integer (i4) :: &
       av_start_time_restart_this_icesheet ! av_start_time_restart for the current ice sheet, for comparison

  integer (i4) :: &
       days_this_year  ! days since beginning of year

  integer (i4) :: &
       forcing_start_time

  logical :: &
      cesm_restart = .false. ! Logical flag to pass to CISM, telling it to restart
                             ! from a CESM restart

  logical :: &
      cism_debug   = .false. ! Logical flag to pass to CISM, telling it to output extra
                             ! debug diagnostics

  integer :: unit      ! fileunit passed to CISM
  integer :: nml_in    ! namelist file unit number

  integer :: climate_tstep  ! climate time step (hours)

  integer, parameter :: days_in_year = 365

  namelist /cism_params/  paramfile_base, num_icesheets, icesheet_names, &
       cism_debug, ice_flux_routing, &
       test_coupling, enable_frac_overrides

! TODO - Write version info?
!-----------------------------------------------------------------------
!  write version information to output log after output redirection
!-----------------------------------------------------------------------
!!   if (my_task == master_task) then
!!      write(stdout,blank_fmt)
!!      write(stdout,ndelim_fmt)
!!      write(stdout,blank_fmt)
!!      write(stdout,'(a)') ' GLC version xxx '
!!      write(stdout,blank_fmt)
!!      call shr_sys_flush(stdout)
!!   endif

!-----------------------------------------------------------------------
!
!  compute time step and initialize time-related quantities
!
!-----------------------------------------------------------------------

   call init_time1

!-----------------------------------------------------------------------
!
!  output delimiter to log file
!
!-----------------------------------------------------------------------

   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      call shr_sys_flush (stdout)
   endif

!--------------------------------------------------------------------
! Initialize ice sheet model, grid, and coupling.
! The following code is largely based on CISM.
!-----------------------------------------------------------------------

   paramfile_base  = 'unknown_paramfile'
   icesheet_names(:) = icesheet_unset

   if (my_task == master_task) then
      open (newunit=nml_in, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nml_in, nml=cism_params,iostat=nml_error)
      end do
      if (nml_error == 0) close(nml_in)
   endif
   call broadcast_scalar(nml_error, master_task)
   if (nml_error /= 0) then
      call exit_glc(sigAbort,'ERROR reading cism_params nml')
   endif

   if (my_task == master_task) then
      call check_icesheet_names(num_icesheets, icesheet_names)
   end if

   call broadcast_scalar(paramfile_base,    master_task)
   call broadcast_scalar(num_icesheets,     master_task)
   call broadcast_array (icesheet_names,    master_task)
   call broadcast_scalar(cism_debug,        master_task)
   call broadcast_scalar(ice_flux_routing,  master_task)
   call broadcast_scalar(test_coupling,     master_task)
   call broadcast_scalar(enable_frac_overrides, master_task)
   call set_routing(ice_flux_routing)

   if (my_task == master_task) then
      write(stdout,*) 'test_coupling:   ', test_coupling
      write(stdout,*) 'enable_frac_overrides: ', enable_frac_overrides
      write(stdout,*) 'icesheet_names: ', icesheet_names(1:num_icesheets)
   end if

   if (verbose .and. my_task==master_task) then
      write (stdout,*) 'paramfile_base =   ', paramfile_base
      write (stdout,*) 'dtt =', dtt
      call shr_sys_flush(stdout)
   endif

   ! Set climate time step

   climate_tstep = nint(dtt/3600._r8)   ! convert from sec to integer hours

   if (verbose .and. my_task==master_task) then
      write (stdout,*) 'climate_tstep (hr) =', climate_tstep
      write (stdout,*) 'Set glimmer_unit =', stdout
      write (stdout,*) 'Initialize glad'
   endif

  ! Set glimmer_unit for diagnostic output from CISM. (Log file is already open)
!  call open_log(unit=101)

  call set_glimmer_unit(stdout)

  ! Initialize the ice sheet model

  nhour_glad = 0     ! number of hours glad has run since start of complete simulation
                     ! must be set to correct value if reading from a restart file

  if (enable_frac_overrides) then
     call init_glc_frac_overrides()
  end if

  ! if this is a continuation run, then set up to read restart file and get the restart time
  allocate(cesm_restart_files(num_icesheets))
  cesm_restart_files(:) = ' '
  if (runtype == 'continue') then
     cesm_restart = .true.

     ! Read info for the first ice sheet
     call glc_io_read_restart_time(icesheet_names(1), nhour_glad, av_start_time_restart, cesm_restart_files(1))

     ! For other ice sheets, read cesm_restart_file; for nhour_glad and
     ! av_start_time_restart, just compare with the first to ensure consistency
     do ns = 2, num_icesheets
        call glc_io_read_restart_time(icesheet_names(ns), nhour_glad_this_icesheet, &
             av_start_time_restart_this_icesheet, cesm_restart_files(ns))
        if (nhour_glad_this_icesheet /= nhour_glad .or. &
             av_start_time_restart_this_icesheet /= av_start_time_restart) then
           write(stdout,*) 'Inconsistency between ice sheets in nhour_glad and/or av_start_time_restart:'
           write(stdout,*) 'Values for ', trim(icesheet_names(1)), ':'
           write(stdout,*) 'nhour_glad = ', nhour_glad
           write(stdout,*) 'av_start_time_restart = ', av_start_time_restart
           write(stdout,*) 'Values for ', trim(icesheet_names(ns)), ':'
           write(stdout,*) 'nhour_glad = ', nhour_glad_this_icesheet
           write(stdout,*) 'av_start_time_restart = ', av_start_time_restart_this_icesheet
           call exit_glc(sigAbort, 'ERROR: Inconsistency between ice sheets in nhour_glad and/or av_start_time_restart')
        end if
     end do

     call ymd2eday (iyear0, imonth0, iday0, elapsed_days0)
     elapsed_days = elapsed_days0 + nhour_glad/24
     call eday2ymd(elapsed_days, iyear, imonth, iday)
     ihour = 0
     iminute = 0
     isecond = 0
     nsteps_total = nhour_glad / climate_tstep
     if (verbose .and. my_task==master_task) then
        write(stdout,*) 'Successfully read restart, nhour_glad =', nhour_glad
        write(stdout,*) 'Initial eday/y/m/d:', elapsed_days0, iyear0, imonth0, iday0
        write(stdout,*) 'eday/y/m/d after restart:', elapsed_days, iyear, imonth, iday
        write(stdout,*) 'nsteps_total =', nsteps_total
        write(stdout,*) 'Initialize glad:'
     endif
  endif

  if (verbose .and. my_task==master_task) then
     write(stdout,*) 'Initialize glad, nhour_glad =', nhour_glad
  endif

  unit = shr_file_getUnit()

  allocate(paramfiles(num_icesheets))
  do ns = 1, num_icesheets
     paramfiles(ns) = trim(paramfile_base) // "." // trim(icesheet_names(ns)) // ".config"
  end do
  call glad_initialize(ice_sheet,                            &
                       climate_tstep,                        &
                       paramfiles,                           &
                       daysinyear = days_in_year,            &
                       start_time = nhour_glad,             &
                       gcm_restart = cesm_restart,           &
                       gcm_debug = cism_debug,               &
                       gcm_fileunit = unit)

  if (cesm_restart) then
     forcing_start_time = av_start_time_restart
  else if (test_coupling) then
     ! This assumes that (1) when test_coupling is true, we take a mass balance time step
     ! at the start of every day, and (2) we're starting at the start of a day (i.e.,
     ! ihour0 = iminute0 = isecond0 = 0).
     forcing_start_time = nhour_glad
  else
     ! BUG(wjs, 2017-04-08, https://github.com/NCAR/CISM/issues/1) This assumes that mass
     ! balance time steps always occur on the year boundary - so the current mass balance
     ! time step started at the beginning of the current year. We'd need to generalize
     ! this to allow mid-year mass balance time steps.
     !
     ! This also assumes that ihour0, iminute0 and isecond0 are all 0
     call ymd2eday(year=0, month=imonth0, day=iday0, eday=days_this_year)
     forcing_start_time = nhour_glad - days_this_year * 24
  end if

  call allocate_indices(num_icesheets)
  call allocate_cpl_bundles(num_icesheets)
  zero_gcm_fluxes_for_all_icesheets = .true.  ! true until we find at least one ice sheet for which it is false
  do ns = 1, num_icesheets
     call glad_initialize_instance(ice_sheet, instance_index = ns, &
          gcm_restart_file = cesm_restart_files(ns), &
          my_forcing_start_time = forcing_start_time, &
          test_coupling = test_coupling)

     ! Initialize global to local index translation for this ice sheet instance
     call init_indices_one_icesheet(instance_index = ns, params = ice_sheet)

     call glc_allocate_fields(instance_index = ns, nx = get_nx(ns), ny = get_ny(ns), &
          nzocn = get_nzocn(ns))

     if (.not. ice_sheet%instances(ns)%zero_gcm_fluxes) then
        zero_gcm_fluxes_for_all_icesheets = .false.
     end if

     associate( &
          tsfc                => cpl_bundles(ns)%tsfc, &
          qsmb                => cpl_bundles(ns)%qsmb, &
          salinity            => cpl_bundles(ns)%salinity, &
          tocn                => cpl_bundles(ns)%tocn, &
          ice_covered         => cpl_bundles(ns)%ice_covered, &
          topo                => cpl_bundles(ns)%topo, &
          rofi                => cpl_bundles(ns)%rofi, &
          rofl                => cpl_bundles(ns)%rofl, &
          hflx                => cpl_bundles(ns)%hflx, &
          ice_sheet_grid_mask => cpl_bundles(ns)%ice_sheet_grid_mask)

     tsfc(:,:) = 0._r8
     qsmb(:,:) = 0._r8

     ! For now, hard-code salinity and tocn to reasonable constant values, until we have the
     ! necessary ocean coupling in place
     !
     ! TODO(wjs, 2021-06-25) change these to 0 or some other place-holder value once we have
     ! the coupling in place
     salinity(:,:,:) = 35._r8
     tocn(:,:,:) = 274._r8

     call glad_get_initial_outputs(ice_sheet, instance_index = ns, &
          ice_covered = ice_covered, &
          topo = topo, &
          rofi = rofi, &
          rofl = rofl, &
          hflx = hflx, &
          ice_sheet_grid_mask = ice_sheet_grid_mask)

     end associate
  end do

  call glad_initialization_wrapup(ice_sheet)

!TODO - Implement PDD option

   call shr_file_freeunit(unit)

! Do the following:
! For each instance, convert ice_sheet%instances(i)%glide_time to hours and compare to nhour_glad.
! If different: Reset params%instances(i)%next_time, params%start_time, params%next_av_start
! Do this here or in initialise_glad?

  ! If restarting (nhour_glad > 0), recompute the year, month, and day
  ! By default, iyear0 = imonth0 = iday0 = 1 (set in namelist file)
  ! Assume that ihour0 = iminute0 = isecond0 = 0
  ! Note that glad does not handle leap years

  ! Set the message level (1 is the default - only fatal errors)
  ! N.B. Must do this after initialization

  call glimmer_set_msg_level(6)

!-----------------------------------------------------------------------
!
!  finish computing time-related quantities after restart info
!  available (including iyear, imonth, and iday)
!
!-----------------------------------------------------------------------

   call init_time2

!-----------------------------------------------------------------------
!
!  initialize history output
!
!-----------------------------------------------------------------------

   call allocate_history(num_icesheets)
   do ns = 1, num_icesheets
      call glc_history_init(instance_index = ns, &
           instance_name = icesheet_names(ns), &
           instance = ice_sheet%instances(ns))
   end do

!-----------------------------------------------------------------------
!
!  do initial history output
!
!-----------------------------------------------------------------------

   if (.not. cesm_restart) then
      do ns = 1, num_icesheets
         call glc_history_write(ns, ice_sheet%instances(ns), EClock, initial_history=.true.)
      end do
   end if

!-----------------------------------------------------------------------
!
!  output delimiter to log file
!
!-----------------------------------------------------------------------

   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,'(" End of GLC initialization")')
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      call shr_sys_flush (stdout)
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_initialize

!***********************************************************************

!***********************************************************************
!BOP
! !IROUTINE: check_icesheet_names
! !INTERFACE:

 subroutine check_icesheet_names(num_icesheets, icesheet_names)

   ! !DESCRIPTION:
   ! Do some error checking on the num_icesheets and icesheet_names namelist inputs
   !
   ! Abort with error message if any issue is found

   ! !INPUT/OUTPUT PARAMETERS:
   integer, intent(in) :: num_icesheets
   character(*), intent(in) :: icesheet_names(:)
   !EOP
   !BOC
   !-----------------------------------------------------------------------
   !  local variables
   !-----------------------------------------------------------------------
   integer :: i

   if (size(icesheet_names) /= max_icesheets) then
      ! In contrast to the other checks in this subroutine, this one indicates a
      ! programming error rather than a user error. We check it here, though, because
      ! other logic below relies on the fact that size(icesheet_names) == max_icesheets.
      write(stdout,*) 'Expect icesheet_names array size to be max_icesheets'
      write(stdout,*) 'max_icesheets = ', max_icesheets
      write(stdout,*) 'size(icesheet_names) = ', size(icesheet_names)
      call exit_glc(sigAbort, 'ERROR: Expect icesheet_names array size to be max_icesheets')
   end if

   if (num_icesheets > max_icesheets) then
      write(stdout,*) 'num_icesheets exceeds hard-coded max_icesheets'
      write(stdout,*) 'num_icesheets: ', num_icesheets
      write(stdout,*) 'max_icesheets: ', max_icesheets
      write(stdout,*) "Increase CISM's max_icesheets parameter and rerun"
      call exit_glc(sigAbort, 'ERROR: num_icesheets exceeds hard-coded max_icesheets')
   end if

   do i = 1, num_icesheets
      if (icesheet_names(i) == icesheet_unset) then
         write(stdout,*) 'Not all icesheet names are set'
         write(stdout,*) 'num_icesheets = ', num_icesheets
         write(stdout,*) 'icesheet_names = ', icesheet_names
         call exit_glc(sigAbort, 'ERROR: Not all icesheet names are set')
      end if
   end do

   do i = num_icesheets+1, max_icesheets
      if (icesheet_names(i) /= icesheet_unset) then
         write(stdout,*) 'Too many icesheet names are set'
         write(stdout,*) 'num_icesheets = ', num_icesheets
         write(stdout,*) 'icesheet_names = ', icesheet_names
         call exit_glc(sigAbort, 'ERROR: Too many icesheet names are set')
      end if
   end do

 end subroutine check_icesheet_names

 end module glc_InitMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
