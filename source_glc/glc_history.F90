!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module glc_history

  !BOP
  ! !MODULE: glc_history

  ! !DESCRIPTION:
  ! Contains routines for handling history output.
  !
  ! Usage:
  !
  ! - In initialization, call glc_history_init
  !
  ! - Every time through the run loop, call glc_history_write
  !
  ! !USES:
  use glc_kinds_mod
  use history_tape_base , only : history_tape_base_type
  use glc_exit_mod      , only : exit_glc, sigAbort
  use glc_constants     , only : stdout
  
  implicit none
  private
  save

  ! !PUBLIC ROUTINES:
  public :: allocate_history  ! allocate the array containing the history tape objects
  public :: glc_history_init  ! initialize the history_tape instance
  public :: glc_history_write ! write to history file, if it's time to do so
  
  ! !PRIVATE MODULE VARIABLES:

  ! There is an array of history tape objects, one per ice sheet. This does *not*
  ! currently allow for more than one history tape for a given ice sheet.

  ! In order to have an array of history_tape_base_type objects, with each one
  ! potentially having a different runtime type, we need this container type so we can
  ! have an array of objects of this container type.
  type :: history_tape_container
     private
     class(history_tape_base_type), allocatable :: history_tape
  end type history_tape_container

  type(history_tape_container), allocatable :: history_tapes(:)

contains

  !------------------------------------------------------------------------
  ! PUBLIC ROUTINES
  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine allocate_history(num_icesheets)
    !
    ! !DESCRIPTION:
    ! Allocate the array containing the history tape objects
    !
    ! !ARGUMENTS:
    integer, intent(in) :: num_icesheets ! number of ice sheet instances in this run
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'allocate_history'
    !-----------------------------------------------------------------------

    allocate(history_tapes(num_icesheets))

  end subroutine allocate_history

  !-----------------------------------------------------------------------
  subroutine glc_history_init(instance_index, instance_name, instance)
    !
    ! !DESCRIPTION:
    ! Initialize the history_tape instance for one ice sheet instance
    !
    ! Should be called once per ice sheet, in model initialization
    !
    ! !USES:
    use glad_type, only : glad_instance
    use glc_time_management, only : freq_opt_nyear
    use history_tape_standard, only : history_tape_standard_type
    use history_tape_coupler, only : history_tape_coupler_type
    !
    ! !ARGUMENTS:
    integer(i4), intent(in) :: instance_index     ! index of current ice sheet
    character(len=*), intent(in) :: instance_name ! name of current ice sheet
    type(glad_instance), intent(in) :: instance
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'glc_history_init'
    !-----------------------------------------------------------------------

    select case (instance%history_option)
    case ('nyears')
       allocate(history_tapes(instance_index)%history_tape, &
            source = history_tape_standard_type( &
            icesheet_name = instance_name, &
            history_vars = instance%esm_history_vars, &
            freq_opt = freq_opt_nyear, &
            freq = instance%history_frequency))
    case ('coupler')
       allocate(history_tapes(instance_index)%history_tape, &
            source = history_tape_coupler_type( &
            icesheet_name = instance_name, &
            history_vars = instance%esm_history_vars))
    case default
       write(stdout,*) subname//' ERROR: Unhandled history_option: ', trim(instance%history_option)
       call exit_glc(sigAbort, subname//' ERROR: Unhandled history_option')
    end select
       
  end subroutine glc_history_init

  !-----------------------------------------------------------------------
  subroutine glc_history_write(instance_index, instance, EClock, initial_history)
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
    use glad_type, only : glad_instance
    use esmf, only: ESMF_Clock
    !
    ! !ARGUMENTS:
    integer(i4), intent(in) :: instance_index     ! index of current ice sheet
    type(glad_instance), intent(inout) :: instance
    type(ESMF_Clock),     intent(in)    :: EClock
    logical, intent(in), optional :: initial_history
    !-----------------------------------------------------------------------

    call history_tapes(instance_index)%history_tape%write_history(instance, EClock, initial_history)
    
  end subroutine glc_history_write

end module glc_history
