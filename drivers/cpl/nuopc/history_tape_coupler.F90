module history_tape_coupler

  ! Defines a class for controlling history frequency based on the coupler's history
  ! frequency - this does not have any history frequency controlled by cism - it is
  ! tied to the coupler history frequency

  use history_tape_base , only : history_tape_base_type

  implicit none
  private
  save

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
  function constructor(history_vars)
    !
    ! !DESCRIPTION:
    ! Creates a history_tape_coupler_type object
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    type(history_tape_coupler_type) :: constructor  ! function result

    ! List of variables to write to file
    character(len=*), intent(in) :: history_vars

    !-----------------------------------------------------------------------
  
    call constructor%set_history_vars(history_vars)
  end function constructor

  !-----------------------------------------------------------------------
  logical function is_time_to_write_hist(this, Eclock)
    !
    ! !DESCRIPTION:
    ! Returns true if it is time to write the history tape associated with this controller.
    !
    ! !USES:
    use ESMF, only : ESMF_Clock, ESMF_Alarm, ESMF_ClockGetAlarm
    use ESMF, only : ESMF_AlarmIsRinging, ESMF_AlarmRingerOff 
    use ESMF, only : ESMF_LOGERR_PASSTHRU, ESMF_END_ABORT, ESMF_Finalize
    use ESMF, only : ESMF_LogFoundERror
    !
    ! !ARGUMENTS:
    class(history_tape_coupler_type) , intent(in) :: this
    type(ESMF_Clock), intent(in) :: EClock
    !
    ! local variables
    type(ESMF_Alarm) :: alarm
    integer :: rc
    !-----------------------------------------------------------------------

    call ESMF_ClockGetAlarm(Eclock, alarmname='alarm_history', alarm=alarm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__,file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

    if (ESMF_AlarmIsRinging(alarm)) then
       call ESMF_AlarmRingerOff( alarm, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__,file=__FILE__)) then
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
       end if
       is_time_to_write_hist = .true.
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
