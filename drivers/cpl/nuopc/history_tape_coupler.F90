module history_tape_coupler

  ! Defines a class for controlling history frequency based on the coupler's history
  ! frequency - this does not have any history frequency controlled by cism - it is
  ! tied to the coupler history frequency

  use history_tape_base , only : history_tape_base_type
  use glimmer_ncdf      , only : glimmer_nc_output
  use ESMF              , only : ESMF_Time

  implicit none
  private
  save

  ! The coupler's history alarm is shared by all coupler history tapes (e.g., one per ice sheet).
  ! The first tape that finds the alarm ringing turns it off, and records the clock time here.
  ! Any other tape that checks at the same clock time (i.e., in the same coupling step) then
  ! also writes. Without this, only the first ice sheet would write history files.
  type(ESMF_Time) :: last_ring_time        ! clock time at which the alarm last rang
  logical :: have_ring_time = .false.      ! true once last_ring_time has been set

  public :: history_tape_coupler_type
  type, extends(history_tape_base_type) :: history_tape_coupler_type
     private
   contains
     ! Logical function saying whether it's time to write a history file
     procedure :: is_time_to_write_hist

     ! Function returning a string describing the history frequency
     procedure :: history_frequency_string
  end type history_tape_coupler_type

  interface history_tape_coupler_type
     module procedure constructor
  end interface history_tape_coupler_type

contains

  !-----------------------------------------------------------------------
  function constructor(icesheet_name, tag, oc)
    !
    ! !DESCRIPTION:
    ! Creates a history_tape_coupler_type object
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    type(history_tape_coupler_type) :: constructor  ! function result

    ! Name of this ice sheet
    character(len=*), intent(in) :: icesheet_name

    ! History stream (e.g., 'h0i')
    character(len=*), intent(in) :: tag

    ! CISM output object for this history stream
    type(glimmer_nc_output), pointer :: oc

    !-----------------------------------------------------------------------
  
    call constructor%set_icesheet_name(icesheet_name)
    call constructor%set_output(tag, oc)
  end function constructor

  !-----------------------------------------------------------------------
  logical function is_time_to_write_hist(this, Eclock)
    !
    ! !DESCRIPTION:
    ! Returns true if it is time to write the history tape associated with this controller.
    !
    ! !USES:
    use ESMF, only : ESMF_Clock, ESMF_Alarm, ESMF_ClockGetAlarm, ESMF_ClockGet
    use ESMF, only : ESMF_AlarmIsRinging, ESMF_AlarmRingerOff, operator(==)
    use ESMF, only : ESMF_LOGERR_PASSTHRU, ESMF_END_ABORT, ESMF_Finalize
    use ESMF, only : ESMF_LogFoundERror
    !
    ! !ARGUMENTS:
    class(history_tape_coupler_type) , intent(in) :: this
    type(ESMF_Clock), intent(in) :: EClock
    !
    ! local variables
    type(ESMF_Alarm) :: alarm
    type(ESMF_Time)  :: currTime
    integer :: rc
    !-----------------------------------------------------------------------

    call ESMF_ClockGet(EClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__,file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

    call ESMF_ClockGetAlarm(Eclock, alarmname='alarm_history', alarm=alarm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__,file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

    if (ESMF_AlarmIsRinging(alarm)) then
       call ESMF_AlarmRingerOff( alarm, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__,file=__FILE__)) then
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
       end if
       last_ring_time = currTime
       have_ring_time = .true.
       is_time_to_write_hist = .true.
    else if (have_ring_time) then
       ! Another tape (e.g., for another ice sheet) already turned off the alarm in this
       ! coupling step; write if this is the same time at which the alarm rang
       is_time_to_write_hist = (currTime == last_ring_time)
    else
       is_time_to_write_hist = .false.
    end if

  end function is_time_to_write_hist

  !-----------------------------------------------------------------------
  function history_frequency_string(this)
    !
    ! !DESCRIPTION:
    ! Returns a string representation of this history frequency
    !
    ! TODO(wjs, 2015-02-17) This needs to be implemented. It is currently difficult (or
    ! impossible) to extract the frequency information from the coupler. Hopefully this
    ! will become easier once the coupler implements the necessary functionality for
    ! metadata on its own history files.
    !
    ! !ARGUMENTS:
    character(len=:), allocatable :: history_frequency_string  ! function result
    class(history_tape_coupler_type), intent(in) :: this

    !-----------------------------------------------------------------------

    history_frequency_string = '(matches coupler history frequency)'

  end function history_frequency_string
  
end module history_tape_coupler
