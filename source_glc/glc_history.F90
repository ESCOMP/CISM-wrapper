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

  ! This needs to have the target attribute so we can point to it in glc_history_write;
  ! that is needed to work around a pgi compiler bug.
  type(history_tape_container), allocatable, target :: history_tapes(:)

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
    use glimmer_ncdf, only : glimmer_nc_output
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

    type(glimmer_nc_output), pointer :: oc   ! CISM output object for the h0i history stream

    character(len=*), parameter :: subname = 'glc_history_init'
    !-----------------------------------------------------------------------

    ! Find the CISM output object for the instantaneous (h0i) history stream.
    ! If there is none (e.g., the history variable list is empty), no h0i files are written.
    oc => find_history_output(instance, 'h0i', time_average = .false.)
    if (.not. associated(oc)) then
       write(stdout,*) subname//': no h0i history output for ice sheet ', trim(instance_name)
       return
    end if

    ! Note: history_option and history_frequency apply to the h0i stream.
    select case (instance%history_option)
    case ('nyears')
       allocate(history_tapes(instance_index)%history_tape, &
            source = history_tape_standard_type( &
            icesheet_name = instance_name, &
            tag = 'h0i', &
            oc = oc, &
            freq_opt = freq_opt_nyear, &
            freq = instance%history_frequency))
    case ('coupler')
       allocate(history_tapes(instance_index)%history_tape, &
            source = history_tape_coupler_type( &
            icesheet_name = instance_name, &
            tag = 'h0i', &
            oc = oc))
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

    class(history_tape_base_type), pointer :: htape_ptr
    !-----------------------------------------------------------------------

    ! If this ice sheet has no history stream, there is nothing to write
    if (.not. allocated(history_tapes(instance_index)%history_tape)) return

    ! COMPILER_BUG(wjs, 2021-10-18, pgi20.1) With a straightforward call like this:
    !     call history_tapes(instance_index)%history_tape%write_history(instance, EClock, initial_history)
    ! pgi20.1 fails with:
    !     /tmp/pgf90PFtg7F9Be42q.ll:1034:16: error: use of undefined type named 'struct.BSS4'
    !         %20 = bitcast %struct.BSS4* @.BSS4 to i8*, !dbg !14930
    ! Adding this pointer indirection prevents this compiler error
    htape_ptr => history_tapes(instance_index)%history_tape
    call htape_ptr%write_history(instance, EClock, initial_history)
    
  end subroutine glc_history_write

  !------------------------------------------------------------------------
  ! PRIVATE ROUTINES
  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------
  function find_history_output(instance, tag, time_average) result(oc)
    !
    ! !DESCRIPTION:
    ! Find the CISM output object for a history stream (e.g., 'h0i'), and check that it is
    ! set up correctly. Returns a null pointer if there is no such object.
    !
    ! The object comes from a [CF output] section in the CISM config file, written by buildnml,
    ! with name = tag. With one_file_per_write = .true., CISM saves the name in base_filename.
    ! CISM never writes this object on its own (external_control = .true.); instead, the
    ! wrapper writes it, one file per write.
    !
    ! !USES:
    use glad_type, only : glad_instance
    use glimmer_ncdf, only : glimmer_nc_output
    !
    ! !ARGUMENTS:
    type(glad_instance), intent(in) :: instance
    character(len=*), intent(in) :: tag            ! history stream, e.g. 'h0i'
    logical, intent(in) :: time_average            ! true if the stream holds time-average fields
    type(glimmer_nc_output), pointer :: oc         ! function result
    !
    ! !LOCAL VARIABLES:
    type(glimmer_nc_output), pointer :: p
    character(len=*), parameter :: subname = 'find_history_output'
    !-----------------------------------------------------------------------

    oc => null()
    p => instance%model%funits%out_first
    do while (associated(p))
       if (trim(p%base_filename) == tag) then
          if (associated(oc)) then
             write(stdout,*) subname//' ERROR: more than one CF output section with name = ', tag
             call exit_glc(sigAbort, subname//' ERROR: duplicate history stream '//tag)
          end if
          oc => p
       end if
       p => p%next
    end do

    if (associated(oc)) then
       if (.not. (oc%external_control .and. oc%one_file_per_write)) then
          write(stdout,*) subname//' ERROR: history stream ', tag, &
               ' must have external_control = .true. and one_file_per_write = .true.'
          call exit_glc(sigAbort, subname//' ERROR: bad settings for history stream '//tag)
       end if
       ! Cross-check: time-average fields must go in the time-average stream only
       if (oc%do_averages .neqv. time_average) then
          write(stdout,*) subname//' ERROR: history stream ', tag, ' has do_averages = ', &
               oc%do_averages, ', expected ', time_average
          call exit_glc(sigAbort, subname//' ERROR: wrong kind of fields in history stream '//tag)
       end if
    end if

  end function find_history_output

end module glc_history
