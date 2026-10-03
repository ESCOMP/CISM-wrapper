module history_tape_base

  ! This module defines an abstract base class to implement a single history tape.

  use glimmer_ncdf, only : glimmer_nc_output
  use glc_constants, only : icesheet_name_len

  implicit none
  private
  save

  public :: history_tape_base_type
  type, abstract :: history_tape_base_type
     private

     character(len=icesheet_name_len) :: icesheet_name

     ! History stream (e.g., 'h0i'), used in file names and time flag names
     character(len=8) :: tag

     ! CISM output object for this history stream. This object comes from a [CF output] section
     ! in the CISM config file (written by buildnml) with name = tag, external_control = .true.
     ! and one_file_per_write = .true. It persists for the whole run, so any time-average
     ! sums and time bounds carry over from one history file to the next.
     type(glimmer_nc_output), pointer :: oc => null()

   contains
     ! ------------------------------------------------------------------------
     ! Public methods
     ! ------------------------------------------------------------------------
     procedure :: write_history     ! write history, if it's time to do so
     procedure :: set_icesheet_name ! set the icesheet name for this history tape
     procedure :: get_icesheet_name ! get the icesheet name for this history tape
     procedure :: set_output        ! set the history stream tag and CISM output object

     ! ------------------------------------------------------------------------
     ! The following are public simply because they need to be overridden by derived
     ! classes. They should not be called directly by clients.
     ! ------------------------------------------------------------------------
     ! Logical function saying whether it's time to write a history file
     procedure(is_time_to_write_hist_interface), deferred :: is_time_to_write_hist

     ! Function returning a string describing the history frequency
     procedure(history_frequency_string_interface), deferred :: history_frequency_string
  end type history_tape_base_type

  abstract interface
     
     logical function is_time_to_write_hist_interface(this, EClock)
       use esmf, only : ESMF_Clock
       import :: history_tape_base_type

       class(history_tape_base_type), intent(in) :: this
       type(ESMF_Clock), intent(in) :: EClock
     end function is_time_to_write_hist_interface

     function history_frequency_string_interface(this)
       import :: history_tape_base_type

       character(len=:), allocatable :: history_frequency_string_interface
       class(history_tape_base_type), intent(in) :: this
     end function history_frequency_string_interface

  end interface

contains

  !-----------------------------------------------------------------------
  subroutine write_history(this, instance, EClock, initial_history)
    !
    ! !DESCRIPTION:
    ! Write a CISM history file, if it's time to do so.
    !
    ! This routine should be called every time step. It will return without doing
    ! anything if it isn't yet time to write a history file.
    !
    ! If initial_history is present and true, that means that we're writing a history file
    ! in initialization. This is written regardless of the check for whether it's time to
    ! do so. It is a regular file of this history stream (e.g., h0i), named with the start date.
    !
    ! !USES:
    use glc_io, only : glc_io_write_hfile
    use glad_type, only : glad_instance
    use esmf, only: ESMF_Clock
    !
    ! !ARGUMENTS:
    class(history_tape_base_type), intent(inout) :: this
    type(glad_instance), intent(inout) :: instance
    type(ESMF_Clock),     intent(in)    :: EClock
    logical, intent(in), optional :: initial_history
    !
    ! !LOCAL VARIABLES:
    logical :: l_initial_history   ! local version of initial_history

    character(len=*), parameter :: subname = 'write_history'
    !-----------------------------------------------------------------------

    l_initial_history = .false.
    if (present(initial_history)) then
       l_initial_history = initial_history
    end if

    if (l_initial_history) then
       ! The initial file has no time_period_freq attribute
       call glc_io_write_hfile(instance, this%oc, trim(this%tag), this%icesheet_name, EClock)
    else if (this%is_time_to_write_hist(EClock)) then
       call glc_io_write_hfile(instance, this%oc, trim(this%tag), this%icesheet_name, EClock, &
            history_frequency_metadata = this%history_frequency_string())
    end if

  end subroutine write_history

  !-----------------------------------------------------------------------
  subroutine set_icesheet_name(this, icesheet_name)
    !
    ! !DESCRIPTION:
    ! Set the icesheet name for this history tape
    !
    ! !ARGUMENTS:
    class(history_tape_base_type), intent(inout) :: this
    character(len=*), intent(in) :: icesheet_name
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'set_icesheet_name'
    !-----------------------------------------------------------------------

    this%icesheet_name = icesheet_name

  end subroutine set_icesheet_name

  !-----------------------------------------------------------------------
  function get_icesheet_name(this) result(icesheet_name)
    !
    ! !DESCRIPTION:
    ! Get the icesheet name for this history tape
    !
    ! !ARGUMENTS:
    class(history_tape_base_type), intent(in) :: this
    character(len=:), allocatable :: icesheet_name  ! function result
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'get_icesheet_name'
    !-----------------------------------------------------------------------

    icesheet_name = this%icesheet_name

  end function get_icesheet_name
  
  !-----------------------------------------------------------------------
  subroutine set_output(this, tag, oc)
    !
    ! !DESCRIPTION:
    ! Set the history stream tag (e.g., 'h0i') and the CISM output object for this history tape
    !
    ! !ARGUMENTS:
    class(history_tape_base_type), intent(inout) :: this
    character(len=*), intent(in) :: tag
    type(glimmer_nc_output), pointer :: oc
    !
    ! !LOCAL VARIABLES:
    character(len=*), parameter :: subname = 'set_output'
    !-----------------------------------------------------------------------

    this%tag = tag
    this%oc => oc

  end subroutine set_output

end module history_tape_base
