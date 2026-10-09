!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_io

!BOP
! !MODULE: glc_io

! !DESCRIPTION:
!  Contains routines for specialized glc IO
!
! !REVISION HISTORY:
!
! !USES:

   use glc_time_management, only: iyear, imonth, iday, ihour, iminute, isecond, &
                                  cesm_date_stamp, elapsed_days, elapsed_days0
   use glc_communicate,     only: my_task, master_task
   use glimmer_ncdf,        only: add_output, delete_output, nc_errorhandle, glimmer_nc_output
   use glc_broadcast,       only: broadcast_scalar
   use glimmer_ncio,        only: glimmer_nc_checkwrite, &
                                  glimmer_nc_createfile
   use glimmer_global,      only: fname_length
   use glc_constants
   use glc_kinds_mod
   use esmf,                only: ESMF_Clock, ESMF_Time, ESMF_ClockGet, ESMF_TimeGet, &
                                  ESMF_TimeSet, ESMF_TimeInterval, ESMF_TimeIntervalGet, &
                                  ESMF_Calendar, ESMF_SUCCESS, operator(-)
   use shr_cal_mod,         only: shr_cal_ymd2date
   use shr_sys_mod
   use shr_kind_mod,        only: CL=>SHR_KIND_CL, CX=>SHR_KIND_CX, &
                                  IN=>SHR_KIND_IN
   use shr_file_mod,        only: shr_file_getunit, shr_file_freeunit
   use netcdf

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_io_read_restart_time,         &
             glc_io_write_hfile,               &
             glc_io_init_tavg_interval,        &
             glc_io_reset_tavg_interval,       &
             glc_io_write_restart

! !PRIVATE MEMBER DATA:

   ! Units of the 'time' variable in CISM history and restart files, following CESM conventions:
   ! 'days since 0001-01-01 00:00:00'. The time values are computed from the CESM clock
   ! (see glc_io_days_since_ref), so time_ref_year and time_units must be consistent with that function.
   ! Note: CISM's own time variable, internal_time, keeps CISM's units (common_years since 0000-01-01).
   integer,          parameter :: time_ref_year = 1
   character(len=*), parameter :: time_units = 'days'

!EOP
!BOC
!EOC
!***********************************************************************
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_io_read_restart_time
! !INTERFACE:

   subroutine glc_io_read_restart_time(icesheet_name, nhour_glad, av_start_time_restart, yr, mon, day, tod, filename)

    use glc_files, only : get_rpointer_filename

    implicit none
    character(len=*),        intent(in)  :: icesheet_name
    integer(IN),             intent(in)  :: yr
    integer(IN),             intent(in)  :: mon
    integer(IN),             intent(in)  :: day
    integer(IN),             intent(in)  :: tod
    integer(IN),             intent(out) :: nhour_glad
    integer(IN),             intent(out) :: av_start_time_restart
    character(fname_length), intent(out) :: filename

    ! local variables
    character(fname_length) :: filename0
    integer(IN)             :: rst_elapsed_days  !
    integer(IN)             :: ptr_unit          ! unit for pointer file
    integer(IN)             :: rst_unit          ! unit for restart file
    integer(IN)             :: status            !
    integer(IN)             :: modelymd, fileymd, filetod
!-----------------------------------------------------------------------

    if (my_task == master_task) then
       modelymd = yr*10000+mon*100+day
       ! get restart filename from rpointer file
       ptr_unit = shr_file_getUnit()
       open(ptr_unit,file=get_rpointer_filename(icesheet_name, yr, mon, day, tod, .true.))
       read(ptr_unit,'(a)') filename0
       filename = trim(filename0)
       close(ptr_unit)
       write(stdout,*) &
            'glc_io_read_restart_time: using dumpfile for restart = ', filename
       call shr_sys_flush(stdout)
       call shr_file_freeunit(ptr_unit)

       ! read time from the restart file, since CISM needs this to initialize
       rst_unit = shr_file_getUnit()
       status = nf90_open(filename,0,rst_unit)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'av_start_time_restart', av_start_time_restart)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'cesmYMD', fileymd)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'cesmTOD', filetod)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_close(rst_unit)
       call nc_errorhandle(__FILE__,__LINE__,status)
       if(fileymd .ne. modelymd .or. filetod .ne. tod) then
          call shr_sys_abort('glc_io_read_restart_time: ERROR time mismatch in file: '//trim(filename))
       endif
    end if

    call broadcast_scalar (filename        , master_task)
    call broadcast_scalar (rst_elapsed_days, master_task)
    call broadcast_scalar (av_start_time_restart, master_task)

    ! calculate nhour_glad for return
    nhour_glad = rst_elapsed_days * 24

  end subroutine glc_io_read_restart_time

!***********************************************************************
!BOP
! !IROUTINE: glc_io_write_hfile
! !INTERFACE:

  subroutine glc_io_write_hfile(instance, oc, tag, icesheet_name, EClock, history_frequency_metadata)

    ! Write one CESM history file (e.g., h0i) for the CISM output object oc.
    !
    ! The object oc comes from a [CF output] section in the CISM config file (written by
    ! buildnml) with external_control = .true. and one_file_per_write = .true. Thus CISM
    ! never writes this object on its own: the wrapper decides when to write it, and each
    ! write creates a new file with one time slice, using a CESM file name and CESM metadata.
    ! The object persists between writes, so time-average sums and time bounds carry over
    ! from one file to the next.
    !
    ! The sequence of calls follows the one documented in CISM (see NAME_io_writeall in
    ! ncdf_template.F90.in): glimmer_nc_newfile, glimmer_nc_createfile, *_io_create,
    ! (CESM global attributes), glide_nc_filldvars, glimmer_nc_write_timeslice, *_io_write,
    ! *_avg_reset, glimmer_nc_closefile.
    !
    ! history_frequency_metadata gives the text for the time_period_freq global attribute.
    ! If it is absent (e.g., for the initial h0i file), there is no time_period_freq attribute.

    use glad_type
    use glide_io, only : glide_io_create, glide_io_write, glide_avg_reset
    use glad_io, only : glad_io_create, glad_io_write
    use glide_nc_custom, only: glide_nc_filldvars
    use glimmer_ncio, only: glimmer_nc_newfile, glimmer_nc_write_timeslice, glimmer_nc_closefile

    implicit none

    type(glad_instance)     , intent(inout)        :: instance
    type(glimmer_nc_output) , pointer              :: oc             ! CISM output object for this history stream
    character(len=*)        , intent(in)           :: tag            ! history stream, e.g. 'h0i'
    character(len=*)        , intent(in)           :: icesheet_name
    type(ESMF_Clock)        , intent(in)           :: EClock
    character(len=*)        , intent(in), optional :: history_frequency_metadata

    ! local variables
    character(CL) :: filename
    integer(IN)   :: cesmYMD           ! cesm model date
    integer(IN)   :: cesmTOD           ! cesm model sec
    integer(IN)   :: cesmYR            ! cesm model year
    integer(IN)   :: cesmMON           ! cesm model month
    integer(IN)   :: cesmDAY           ! cesm model day
    integer(IN)   :: glcYMD            ! cism model date
    integer(IN)   :: glcTOD            ! cism model sec
    integer(IN)   :: rst_elapsed_days  !
    integer(IN)   :: status            !
    type(ESMF_TIME) :: CurrentTime
    integer       :: rc
!-----------------------------------------------------------------------

    ! figure out history filename
    call ESMF_ClockGet(EClock, currTime=CurrentTime, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_write_hfile")

    call ESMF_TimeGet( CurrentTime, yy=cesmYR, mm=cesmMON, dd=cesmDAY, s=cesmTOD, rc=rc )
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_write_hfile")

    call shr_cal_ymd2date(cesmYR, cesmMON, cesmDAY, cesmYMD)

    if (tag == 'h0a') then
       ! Time-average file: name it with the year of the average (the previous year),
       ! since the file is written at the start of the next year (e.g., h0a.1861 at 1862-01-01).
       filename = glc_filename(icesheet_name, cesmYR-1, cesmMON, cesmDAY, cesmTOD, tag)
    else
       filename = glc_filename(icesheet_name, cesmYR, cesmMON, cesmDAY, cesmTOD, tag)
    end if

    if (my_task == master_task) then
       write(stdout,*) 'glc_io_write_hfile: writing history filename= ', trim(filename)
       call shr_sys_flush(stdout)
    endif

    ! Set up the output object for a new file, and create the file and its variables
    call glimmer_nc_newfile(oc, filename)
    call glimmer_nc_createfile(oc, instance%model, external_baseline_year=time_ref_year, &
         external_time_units=time_units)
    call glide_io_create(oc, instance%model, instance%model)
    call glad_io_create(oc, instance%model, instance)

    if (my_task == master_task) then
       ! write time to the file
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmYMD', cesmYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmTOD', cesmTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcYMD', glcYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcTOD', glcTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       rst_elapsed_days = elapsed_days - elapsed_days0
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)

       ! The following piece of metadata is needed to follow a CESM convention
       if (present(history_frequency_metadata)) then
          status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'time_period_freq', &
               history_frequency_metadata)
          call nc_errorhandle(__FILE__,__LINE__,status)
       end if

       ! Another piece of metadata needed to follow a CESM convention
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'model_doi_url', &
            model_doi_url)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    ! Fill the dimension variables (this also leaves define mode)
    call glide_nc_filldvars(oc, instance%model)

    ! Write the time variables (and time bounds, for time-average files), then the fields
    call glimmer_nc_write_timeslice(oc, instance%model, instance%glide_time, &
         glc_io_days_since_ref(CurrentTime))
    call glide_io_write(oc, instance%model)
    call glad_io_write(oc, instance)

    ! Start a new averaging period for any time-average fields
    ! Note: Glad currently has no tavg fields, so there is no glad_avg_reset.
    if (oc%do_averages) then
       call glide_avg_reset(oc, instance%model)
    end if

    ! Close the file; the output object persists for the next write
    call glimmer_nc_closefile(oc)

  end subroutine glc_io_write_hfile


!***********************************************************************
!BOP
! !IROUTINE: glc_io_init_tavg_interval
! !INTERFACE:

  subroutine glc_io_init_tavg_interval(instance, oc, EClock, cesm_restart, skip_first_write)

    ! Set up the first averaging interval for a time-average history stream (h0a), and determine
    ! whether the first file should be skipped because it would cover only part of a year.
    !
    ! * Continue run: CISM restores the averaging state (running sums, total time, and the start
    !   of the interval in internal and external time units) from the restart file, so the
    !   average continues exactly (oc%tavg_restored = .true.); nothing more is needed here.
    !   If the restart file has no averaging state (it was written by an older version of CISM),
    !   CISM starts a new interval with zero sums, and here we set the start of the interval to
    !   the beginning of the current year. (Older versions did not allow restart files to be
    !   written in the middle of an averaging interval with nonzero sums.)
    ! * Startup, hybrid or branch run: a new averaging interval starts now, with zero sums.
    !   Note: Evolving hybrid and branch runs start CISM from the refcase restart file as a
    !   standard restart (restart = 1), so CISM may have restored the refcase's averaging state;
    !   it is discarded here. If the run does not start at the beginning of a year, the first
    !   averaging interval would cover only part of a year, so the first h0a file is skipped
    !   (skip_first_write = .true.).

    use glad_type
    use glimmer_ncio, only: glimmer_nc_checkwrite_init

    implicit none

    type(glad_instance)     , intent(inout) :: instance
    type(glimmer_nc_output) , pointer     :: oc                ! CISM output object for the h0a stream
    type(ESMF_Clock)        , intent(in)  :: EClock
    logical                 , intent(in)  :: cesm_restart      ! true for a continue run
    logical                 , intent(out) :: skip_first_write  ! true if the first h0a file should be skipped

    ! local variables
    type(ESMF_Time)     :: CurrentTime, StartOfYear
    type(ESMF_Calendar) :: calendar
    integer             :: yr, mon, day, tod
    integer             :: rc
    real(r8)            :: external_start_time
!-----------------------------------------------------------------------

    call ESMF_ClockGet(EClock, currTime=CurrentTime, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_init_tavg_interval: ESMF_ClockGet")
    call ESMF_TimeGet(CurrentTime, yy=yr, mm=mon, dd=day, s=tod, calendar=calendar, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_init_tavg_interval: ESMF_TimeGet")

    if (cesm_restart .and. oc%tavg_restored) then
       ! Keep the averaging state restored by CISM
       skip_first_write = .false.
    elseif (cesm_restart) then
       ! Restart file without averaging state: keep CISM's internal start time,
       ! and set the external start time to the beginning of the current year
       call ESMF_TimeSet(StartOfYear, yy=yr, mm=1, dd=1, s=0, calendar=calendar, rc=rc)
       if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_init_tavg_interval: ESMF_TimeSet")
       external_start_time = glc_io_days_since_ref(StartOfYear)
       call glimmer_nc_checkwrite_init(oc, oc%nc%processed_time, external_time=external_start_time)
       skip_first_write = .false.
    else
       ! Start a new averaging interval now, with zero sums
       call glc_io_reset_tavg_interval(instance, oc, EClock)
       skip_first_write = .not. (mon == 1 .and. day == 1 .and. tod == 0)
    end if

  end subroutine glc_io_init_tavg_interval

!***********************************************************************
!BOP
! !IROUTINE: glc_io_reset_tavg_interval
! !INTERFACE:

  subroutine glc_io_reset_tavg_interval(instance, oc, EClock)

    ! Start a new averaging interval now, without writing a file: reset the time-average sums,
    ! and set the start of the interval to the current time. This is used at initialization
    ! (except in continue runs), and to skip the first, partial-year h0a file of a run that
    ! does not start at the beginning of a year.

    use glad_type
    use glide_io, only : glide_avg_reset
    use glimmer_ncio, only: glimmer_nc_checkwrite_init

    implicit none

    type(glad_instance)     , intent(inout) :: instance
    type(glimmer_nc_output) , pointer       :: oc     ! CISM output object for the h0a stream
    type(ESMF_Clock)        , intent(in)    :: EClock

    ! local variables
    type(ESMF_Time) :: CurrentTime
    integer         :: rc
!-----------------------------------------------------------------------

    call ESMF_ClockGet(EClock, currTime=CurrentTime, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_reset_tavg_interval: ESMF_ClockGet")

    if (oc%do_averages) then
       call glide_avg_reset(oc, instance%model)
    end if
    call glimmer_nc_checkwrite_init(oc, instance%glide_time, &
         external_time=glc_io_days_since_ref(CurrentTime))

  end subroutine glc_io_reset_tavg_interval

!***********************************************************************
!BOP
! !IROUTINE: glc_io_write_restart
! !INTERFACE:

   subroutine glc_io_write_restart(instance, icesheet_name, EClock)

    use glc_files           , only : get_rpointer_filename
    use glad_type
    use glide_io            , only : glide_io_create, glide_io_write
    use glad_io             , only : glad_io_create, glad_io_write
    use glide_nc_custom     , only : glide_nc_filldvars
    use glad_main           , only : glad_okay_to_restart
    use glad_input_averages , only : get_av_start_time

    implicit none
    type(glad_instance), intent(inout) :: instance
    character(len=*)   , intent(in)    :: icesheet_name
    type(ESMF_Clock),     intent(in)    :: EClock

    ! local variables
    type(glimmer_nc_output),  pointer :: oc => null()
    character(CL) :: filename
    integer(IN)   :: cesmYMD           ! cesm model date
    integer(IN)   :: cesmTOD           ! cesm model sec
    integer(IN)   :: cesmYR            ! cesm model year
    integer(IN)   :: cesmMON           ! cesm model month
    integer(IN)   :: cesmDAY           ! cesm model day
    integer(IN)   :: glcYMD            ! cism model date
    integer(IN)   :: glcTOD            ! cism model sec
    integer(IN)   :: rst_elapsed_days  !
    integer(IN)   :: ptr_unit          ! unit for pointer file
    integer(IN)   :: status            !
    type(ESMF_TIME) :: CurrentTime
    integer         :: rc
!-----------------------------------------------------------------------

    ! Note: The restart file includes the averaging state of each time-average history stream
    !       (e.g., h0a), so a restart can occur in the middle of an averaging interval.
    !       History files are written (in glc_run) before the restart file, so the saved state
    !       is the state after any h0a file written at this time, and after the sums were reset.

    if (.not. glad_okay_to_restart(instance)) then
       if (my_task == master_task) then
          write(stdout,*) 'ERROR: Attempt to write a restart file at an invalid time'
          write(stdout,*) 'This can occur if GLC_AVG_PERIOD is shorter than the mass balance time step,'
          write(stdout,*) 'and if you are trying to write a restart file in the middle of a mass balance time step.'
          write(stdout,*) '(This is because CISM does not save the accumulated input fields when you restart'
          write(stdout,*) 'in the middle of a mass balance time step.)'
          write(stdout,*) 'For example, this problem can occur for GLC_AVG_PERIOD=glc_coupling_period,'
          write(stdout,*) 'when the glc coupling period is 1 day, and the mass balance time step is 1 year,'
          write(stdout,*) 'if you try to write a restart file mid-year.'
          write(stdout,*) 'The solution is generally to set GLC_AVG_PERIOD=yearly if you want'
          write(stdout,*) 'to be able to write mid-year restart files.'
       end if
       call shr_sys_abort('glc_io_write_restart: Attempt to write a restart file at an invalid time')
    end if

    ! figure out restart filename
    call ESMF_ClockGet(EClock, currTime=CurrentTime, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_write_restart")

    call ESMF_TimeGet( CurrentTime, yy=cesmYR, mm=cesmMON, dd=cesmDAY, s=cesmTOD, rc=rc )
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_write_restart")

    call shr_cal_ymd2date(cesmYR, cesmMON, cesmDAY, cesmYMD)

    filename = glc_filename(icesheet_name, cesmYR, cesmMON, cesmDAY, cesmTOD, 'restart')

    if (my_task == master_task) then
       write(stdout,*) &
            'glc_io_write_restart: calling dumpfile for restart filename= ', filename
       call shr_sys_flush(stdout)
    endif

    allocate(oc)
    oc%freq          = 1
    oc%append        = .false.
    oc%default_xtype = NF90_DOUBLE
    oc%nc%filename   = ''
    oc%nc%filename   = trim(filename)
    oc%nc%vars       = ' restart '
    oc%nc%vars_copy  = oc%nc%vars
!jw TO DO: fill out the rest of the metadata
!jw    oc%metadata%title =
!jw    oc%metadata%institution =
!jw    oc%metadata%source =
!jw    oc%metadata%history =
!jw    oc%metadata%references =
!jw    oc%metadata%comment =

    ! create the output unit
    call glimmer_nc_createfile(oc, instance%model, external_baseline_year=time_ref_year, &
         external_time_units=time_units)
    call glide_io_create(oc, instance%model, instance%model)
    call glad_io_create(oc, instance%model, instance)

    if (my_task == master_task) then
       ! write time to the file
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmYMD', cesmYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmTOD', cesmTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcYMD', glcYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcTOD', glcTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       rst_elapsed_days = elapsed_days - elapsed_days0
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'av_start_time_restart', &
            get_av_start_time(instance%glad_inputs))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    call glide_nc_filldvars(oc, instance%model)
    call glimmer_nc_checkwrite(oc, instance%model, forcewrite=.true., &
         time=instance%glide_time, &
         external_time = glc_io_days_since_ref(CurrentTime))
    call glide_io_write(oc, instance%model)
    call glad_io_write(oc, instance)

    if (my_task == master_task) then
       status = nf90_close(oc%nc%id)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    oc => null()
!jw TO DO: figure out why deallocate statement crashes the code
!jw    deallocate(oc)

    ! write pointer to restart file
    if (my_task == master_task) then
       ptr_unit = shr_file_getUnit()
       open(ptr_unit,file=get_rpointer_filename(icesheet_name, cesmYR, cesmMON, cesmDAY, cesmTOD, .false.))
       write(ptr_unit,'(a)') filename
       close(ptr_unit)
       call shr_file_freeunit(ptr_unit)
    endif

  end subroutine glc_io_write_restart

!***********************************************************************
!BOP
! !IROUTINE: glc_io_days_since_ref
! !INTERFACE:
  function glc_io_days_since_ref(CurrentTime) result(days)

    ! Return the number of days from time_ref_year-01-01 00:00:00 to CurrentTime,
    ! in the calendar of CurrentTime (e.g., noleap). This is the value of the 'time' variable
    ! in CISM history and restart files, with units 'days since 0001-01-01 00:00:00'.
    ! The value is computed from the CESM clock, so it does not accumulate roundoff error,
    ! and any time of day is included as a fraction of a day.

    implicit none

    type(ESMF_Time), intent(in) :: CurrentTime
    real(r8) :: days

    ! local variables
    type(ESMF_Time)         :: RefTime
    type(ESMF_TimeInterval) :: elapsed
    type(ESMF_Calendar)     :: calendar
    integer                 :: rc

    call ESMF_TimeGet(CurrentTime, calendar=calendar, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_days_since_ref: ESMF_TimeGet")

    call ESMF_TimeSet(RefTime, yy=time_ref_year, mm=1, dd=1, s=0, calendar=calendar, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_days_since_ref: ESMF_TimeSet")

    elapsed = CurrentTime - RefTime

    call ESMF_TimeIntervalGet(elapsed, d_r8=days, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_days_since_ref: ESMF_TimeIntervalGet")

  end function glc_io_days_since_ref

!***********************************************************************
! BOP
!
! !ROUTINE: glc_filename
!
! !INTERFACE:
  character(CL) function glc_filename( reg_spec, yr_spec, mon_spec, day_spec, sec_spec, file_type )
!
! !DESCRIPTION: Create a filename from a filename specifier. Interpret filename specifier
! string with:
! %c for case
! %i for instance suffix (in the CESM multi-instance/ensemble sense)
! %y for year
! %m for month
! %d for day
! %s for second
! %r for ice sheet name (elsewhere referred to as ice sheet instance)
! %% for the "%" character
! If the filename specifier has spaces " ", they will be trimmed out
! of the resulting filename.
!
! !USES:
    use glc_time_management, only: runid
    use glc_ensemble       , only: get_inst_suffix
!
! !INPUT/OUTPUT PARAMETERS:
  character(len=*) ,      intent(in) :: reg_spec  ! Simulation region (e.g., Greenland vs. Antarctica)
  integer          ,      intent(in) :: yr_spec   ! Simulation year
  integer          ,      intent(in) :: mon_spec  ! Simulation month
  integer          ,      intent(in) :: day_spec  ! Simulation day
  integer          ,      intent(in) :: sec_spec  ! Seconds into current simulation day
  character(len=*) ,      intent(in) :: file_type ! file type: 'h0i', 'h0a' or 'restart'
!
! EOP
!
  integer       :: i, n           ! Loop variables
  character(CL) :: region         ! Simulation region
  integer       :: year           ! Simulation year
  integer       :: month          ! Simulation month
  integer       :: day            ! Simulation day
  integer       :: ncsec          ! Seconds into current simulation day
  character(CX) :: string         ! Temporary character string
  character(CL) :: format         ! Format character string
  character(CL) :: filename_spec  ! cism filename specifier

  !---------------------------------------------------------------------------
  ! Determine what the file tpye is and set the filename specifier accordingly
  !---------------------------------------------------------------------------

  filename_spec = ' '
  if (file_type.eq.'h0i') then
     ! Instantaneous history file. The date in the file name is the time of the snapshot,
     ! without hours or seconds (as in CTSM). For example, a file named h0i.1862-01-01
     ! holds the state at 1862-01-01 00:00, i.e., at the end of year 1861.
     ! The initial history file (written in initialization) is also an h0i file, named
     ! with the start date of the run.
     filename_spec = '%c.cism%i.%r.h0i.%y-%m-%d'
  else if (file_type.eq.'h0a') then
     ! Annual-average history file, named with the year of the average (e.g., h0a.1861).
     ! The caller passes the year of the average, not the current year.
     filename_spec = '%c.cism%i.%r.h0a.%y'
  else if (file_type.eq.'restart') then
     filename_spec = '%c.cism%i.%r.r.%y-%m-%d-%s'
  else
     call shr_sys_abort ('glc_filename: file_type specifier is invalid')
  endif

  !-----------------------------------------------------------------
  ! Determine year, month, day and sec to put in filename
  !-----------------------------------------------------------------

 if ( len_trim(filename_spec) == 0 )then
     call shr_sys_abort ('glc_filename: filename specifier is empty')
  end if
  if ( index(trim(filename_spec)," ") /= 0 )then
     call shr_sys_abort ('glc_filename: filename specifier can not contain a space:'//trim(filename_spec))
  end if

  region = reg_spec
  year  = yr_spec
  month = mon_spec
  day   = day_spec
  ncsec = sec_spec

  ! Go through each character in the filename specifier and interpret if special string

  i = 1
  glc_filename = ''
  string = ''
  do while ( i <= len_trim(filename_spec) )
     if ( filename_spec(i:i) == "%" )then
        i = i + 1
        select case( filename_spec(i:i) )
        case( 'c' )   ! runid
           string = trim(runid)
        case( 'r' )   ! region
           string = trim(region)
        case( 'i' )   ! instance suffix
           call get_inst_suffix(string)
        case( 'y' )   ! year
           if ( year > 99999   ) then
              format = '(i6.6)'
           else if ( year > 9999    ) then
              format = '(i5.5)'
           else
              format = '(i4.4)'
           end if
           write(string,format) year
        case( 'm' )   ! month
           write(string,'(i2.2)') month
        case( 'd' )   ! day
           write(string,'(i2.2)') day
        case( 's' )   ! second
           write(string,'(i5.5)') ncsec
        case( '%' )   ! percent character
           string = "%"
        case default
           call shr_sys_abort ('glc_filename: Invalid expansion character: '//filename_spec(i:i))
        end select
     else
       n = index( filename_spec(i:), "%" )
        if ( n == 0 ) n = len_trim( filename_spec(i:) ) + 1
        if ( n == 0 ) exit
        string = filename_spec(i:n+i-2)
        i = n + i - 2
     end if
     if ( len_trim(glc_filename) == 0 )then
        glc_filename = trim(string)
     else
        if ( (len_trim(glc_filename)+len_trim(string)) >= CL )then
           call shr_sys_abort ('glc_filename Resultant filename too long')
        end if
        glc_filename = trim(glc_filename) // trim(string)
     end if
     i = i + 1
  end do
  if ( len_trim(glc_filename) == 0 )then
     call shr_sys_abort ('glc_filename: Resulting filename is empty')
  end if

  ! add ".nc" to tail end
  glc_filename = trim(glc_filename) // '.nc'

end function glc_filename

end module glc_io
