module history_tape_base

  ! This module defines an abstract base class to implement a single history tape.

  use glad_type, only : len_history_vars
  use glimmer_ncdf, only : glimmer_nc_output
  use glc_constants, only : icesheet_name_len

  implicit none
  private
  save

  public :: history_tape_base_type
  type, abstract :: history_tape_base_type
     private

     character(len=icesheet_name_len) :: icesheet_name

     ! Names of CISM variables to be output in cesm history files
     !
     ! COMPILER_BUG(wjs, 2015-02-19, pgi 14.7) Ideally, this would be an allocatable
     ! character variable. But when written that way, it gets filled with garbage by pgi
     ! 14.7. So for now, I'm using a declared maximum length together with a check that
     ! it's not being set to greater than its length.
     character(len=len_history_vars) :: history_vars

     ! This output structure is accessible to CISM throughout the run, and is
     ! used to accumulate and average the time-average ("tavg") output fields.
     !
     ! NOTE(wjs, 2021-09-24) By storing this in the history_tape object, we ensure that we
     ! have a separate oc_tavg_helper for each ice sheet (since there is a separate
     ! history_tape object for each ice sheet). If we ever had multiple history tapes per
     ! ice sheet, I'm not sure off-hand if we'd want to have a single oc_tavg_helper for
     ! the ice sheet or a separate one for each history tape.
     type(glimmer_nc_output), pointer :: oc_tavg_helper => null()

   contains
     ! ------------------------------------------------------------------------
     ! Public methods
     ! ------------------------------------------------------------------------
     procedure :: write_history     ! write history, if it's time to do so
     procedure :: set_icesheet_name ! set the icesheet name for this history tape
     procedure :: get_icesheet_name ! get the icesheet name for this history tape
     procedure :: set_history_vars  ! set the list of history variables

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
    ! do so, with a different extension than standard history files.
    !

    ! !USES:
    use glc_io, only : glc_io_write_history, glc_io_write_history_tavg_helper
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

       call glc_io_write_history(instance, this%icesheet_name, EClock, &
            this%history_vars, this%oc_tavg_helper, initial_history = .true.)

    else if (this%is_time_to_write_hist(EClock)) then

       call glc_io_write_history(instance, this%icesheet_name, EClock, &
            this%history_vars, this%oc_tavg_helper, initial_history = .false., &
            history_frequency_metadata = this%history_frequency_string())

    end if

    ! This subroutine manages an auxiliary output structure that is used to accumulate
    ! time-average fields in history files.
    call glc_io_write_history_tavg_helper(instance, this%oc_tavg_helper, &
         this%icesheet_name, this%history_vars)

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
  subroutine set_history_vars(this, history_vars)
    !
    ! !DESCRIPTION:
    ! Set the list of history variables
    !
    ! !USES:
    use glc_exit_mod, only : exit_glc, sigAbort
    use glc_constants, only : stdout
    !
    ! !ARGUMENTS:
    class(history_tape_base_type), intent(inout) :: this
    character(len=*), intent(in) :: history_vars
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'set_history_vars'
    !-----------------------------------------------------------------------

    if (len_trim(history_vars) > len(this%history_vars)) then
       write(stdout,*) subname//' ERROR: too-long history vars: <', trim(history_vars), '>'
       call exit_glc(sigAbort, subname//' ERROR: too-long history vars')
    end if
       
    this%history_vars = trim(history_vars)
    
  end subroutine set_history_vars

end module history_tape_base
