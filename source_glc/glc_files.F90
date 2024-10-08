!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_files

!BOP
! !MODULE: glc_files

! !DESCRIPTION:
!  Manage file names of some special files: namelists, restart pointer file, etc.
!
! !REVISION HISTORY:
!
! !USES:

   use shr_kind_mod,        only: CL=>SHR_KIND_CL

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: set_filenames
   public :: get_rpointer_filename

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

  character(CL), public ::  &
      nml_filename  , & ! namelist input file name
      ionml_filename    ! model IO namelist file name

!EOP
!BOC
!EOC
!***********************************************************************
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: set_filenames
! !INTERFACE:
   subroutine set_filenames
!
! !DESCRIPTION:
! Set module variables that give various file names
!
! This should be done in model initialization, after the ensemble-related variables are
! initialized.
!
! !USES:
     use glc_ensemble, only : get_inst_suffix
!
! !ARGUMENTS:
!
! !LOCAL VARIABLES:
     character(len=16) :: inst_suffix
!EOP
!-----------------------------------------------------------------------
     
     call get_inst_suffix(inst_suffix)

     nml_filename = 'cism_in'//trim(inst_suffix)
     ionml_filename = 'glc_modelio.nml'//trim(inst_suffix)

   end subroutine set_filenames

!***********************************************************************
!BOP
! !IROUTINE: get_rpointer_filename
! !INTERFACE:
   function get_rpointer_filename(icesheet_name, yr, mon, day, tod, toread) result(rpointer_filename)
!
! !DESCRIPTION:
! Get the filename for the rpointer file for the given ice sheet
!
! !USES:
     use glc_ensemble, only : get_inst_suffix
!
! !ARGUMENTS:
     character(len=CL) :: rpointer_filename
     character(len=*), intent(in) :: icesheet_name
     integer, intent(in) :: yr
     integer, intent(in) :: mon
     integer, intent(in) :: day
     integer, intent(in) :: tod
     logical, intent(in) :: toread
!
! !LOCAL VARIABLES:
     character(len=16) :: inst_suffix
     character(len=17) :: timestamp
     logical :: exists
     !EOP
!-----------------------------------------------------------------------

     call get_inst_suffix(inst_suffix)
     write(timestamp,'(i4.4,a,i2.2,a,i2.2,a,i5.5)') '.',yr,'-',mon,'-',day,'-',tod
     ! NOTE(wjs, 2021-09-20) In other places, we put the ice sheet name *after* the instance
     ! number. However, I feel like there may be some code that assumes that, for rpointer
     ! files, the instance number comes last. In case my recollection is right about that
     ! (or in case anyone introduces code with that assumption), I am putting the instance
     ! number at the end of the rpointer file name.
     rpointer_filename = 'rpointer.glc.'//trim(icesheet_name)//trim(inst_suffix)//timestamp
     if(toread) then
        inquire(file=trim(rpointer_filename), exist=exists)
        if(.not. exists) then
           rpointer_filename = 'rpointer.glc.'//trim(icesheet_name)//trim(inst_suffix)
        endif
     endif
     
   end function get_rpointer_filename

end module glc_files
