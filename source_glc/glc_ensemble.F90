!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_ensemble

!BOP
! !MODULE: glc_ensemble

! !DESCRIPTION:
!  Contains data and routines for running an ensemble with multiple, independent
!  instances of GLC. This module is also used in the standard (non-ensemble) case.
!
!  This should not be confused with the ability to have multiple instances of cism running
!  in different places (e.g., Greenland & Antarctica). Confusingly, both are referred to
!  as "instances". For the most part (though maybe not 100% consistently), variables with
!  the full word "instance" in their name refer to different ice sheets, whereas variables
!  with the abbreviated "inst" refer to these multi-instance ensemble members.
!
! !REVISION HISTORY:
!  Created by Bill Sacks
!
! !USES:

   use shr_kind_mod, only : IN=>SHR_KIND_IN

   implicit none
   private

! !PUBLIC MEMBER FUNCTIONS:

   public :: set_inst_vars
   public :: write_inst_vars
   public :: get_inst_suffix
   public :: get_inst_name

! !PRIVATE DATA MEMBERS:

   integer          , private :: inst_index    ! number of current instance (e.g., 1)
   character(len=16), private :: inst_name     ! full name of current instance (e.g., GLC_0001)
   character(len=16), private :: inst_suffix   ! character string associated with instance number
                                               ! (e.g., "_0001", or "" for the single-instance case)

   logical, private :: initialized = .false.   ! has the module data been initialized?

!EOP

!***********************************************************************
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: set_inst_vars
! !INTERFACE:
   subroutine set_inst_vars(inst_index_in, inst_name_in, inst_suffix_in )
!
! !DESCRIPTION:
! Set instance variables; this should be done in model initialization
!
! !USES:
     use shr_sys_mod , only : shr_sys_abort
!
! !ARGUMENTS:
     integer(IN)      , intent(in) :: inst_index_in
     character(len=*) , intent(in) :: inst_name_in
     character(len=*) , intent(in) :: inst_suffix_in
!
! !LOCAL VARIABLES:
     character(len=*), parameter :: subname = 'set_inst_vars'
!EOP
!-----------------------------------------------------------------------

     if (initialized) then
        ! Need to write to unit=* because stdout hasn't necessarily been initialized yet
        write(*,*) subname, ' ERROR: module data have already been initialized'
        call shr_sys_abort()
     end if
     
     inst_name   = inst_name_in
     inst_index  = inst_index_in
     inst_suffix = inst_suffix_in

     initialized = .true.

   end subroutine set_inst_vars

!***********************************************************************
!BOP
! !IROUTINE: get_inst_suffix
! !INTERFACE:
   subroutine get_inst_suffix(inst_suffix_out)
!
! !DESCRIPTION:
! Return the instance suffix
!
! !USES:
     use glc_constants, only : stdout
     use shr_sys_mod  , only : shr_sys_abort
!
! !ARGUMENTS:
     character(len=*), intent(out) :: inst_suffix_out ! instance suffix
!
! !LOCAL VARIABLES:
     character(len=*), parameter :: subname = 'get_inst_suffix'
!EOP
!-----------------------------------------------------------------------

     if (.not. initialized) then
        write(stdout,*) subname, ' ERROR: instance variables have not been initialized'
        call shr_sys_abort()
     end if

     if (len_trim(inst_suffix) > len(inst_suffix_out)) then
        write(stdout,*) subname, ' ERROR: output argument too small to hold inst_suffix'
        call shr_sys_abort()
     end if

     inst_suffix_out = inst_suffix
   end subroutine get_inst_suffix

!***********************************************************************
!BOP
! !IROUTINE: get_inst_name
! !INTERFACE:
   subroutine get_inst_name(inst_name_out)
!
! !DESCRIPTION:
! Return the instance name
!
! !USES:
     use glc_constants, only : stdout
     use shr_sys_mod  , only : shr_sys_abort
!
! !ARGUMENTS:
     character(len=*), intent(out) :: inst_name_out ! instance name
!
! !LOCAL VARIABLES:
     character(len=*), parameter :: subname = 'get_inst_name'
!EOP
!-----------------------------------------------------------------------

     if (.not. initialized) then
        write(stdout,*) subname, ' ERROR: instance variables have not been initialized'
        call shr_sys_abort()
     end if

     if (len_trim(inst_name) > len(inst_name_out)) then
        write(stdout,*) subname, ' ERROR: output argument too small to hold inst_name'
        call shr_sys_abort()
     end if

     inst_name_out = inst_name
   end subroutine get_inst_name

!***********************************************************************
!BOP
! !IROUTINE: write_inst_vars
! !INTERFACE:
   subroutine write_inst_vars
!
! !DESCRIPTION:
! Write instance variables to stdout
!
! !USES:
     use glc_constants, only : stdout
     use shr_sys_mod  , only : shr_sys_abort

!
! !ARGUMENTS:
!
! !LOCAL VARIABLES:
     character(len=*), parameter :: subname = 'write_inst_vars'
!EOP
!-----------------------------------------------------------------------

     if (.not. initialized) then
        write(stdout,*) subname, ' ERROR: instance variables have not been initialized'
        call shr_sys_abort()
     end if

     write(stdout,*) 'inst_name: ', inst_name
     write(stdout,*) 'inst_index: ', inst_index
     write(stdout,*) 'inst_suffix: ', inst_suffix

   end subroutine write_inst_vars

!***********************************************************************

 end module glc_ensemble
