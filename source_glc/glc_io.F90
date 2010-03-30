!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_io

!BOP
! !MODULE: glc_io

! !DESCRIPTION:
!  Contains the routine for stepping the model forward one timestep
!
! !REVISION HISTORY:
!  SVN:$Id: step_mod.F90 2019 2006-09-29 22:00:15Z njn01 $
!  Adapted by William Lipscomb from step_mod.F90 in POP 2.0 and from
!   glint_example.F90 in GLIMMER
!
! !USES:

   use glc_time_management, only:  ccsm_date_stamp
   use glc_constants
   use glc_kinds_mod

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_io_create_suffix_ccsm

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

!EOP
!BOC
!EOC
!***********************************************************************
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_io_create_suffix_ccsm
! !INTERFACE:

   subroutine glc_io_create_suffix_ccsm(model)
    !*FD open all netCDF files for output
    use glide_types
    implicit none
    type(glide_global_type) :: model

    ! local variables
    type(glimmer_nc_output), pointer :: oc

    character (char_len) :: &
      char_temp,            &! temp character space
      ccsm_date_string,     &
      file_suffix


!-----------------------------------------------------------------------
!   clear character strings
!-----------------------------------------------------------------------

    file_suffix = char_blank
    char_temp   = char_blank

    char_temp   = 'ymds'

    call ccsm_date_stamp (ccsm_date_string, char_temp)

    file_suffix = trim(ccsm_date_string)//'.nc'

    oc=>model%funits%out_first
    do
       oc%nc%filename=trim(oc%nc%filename)//'.'//trim(file_suffix)
       write(*,*) 'jw filename = ', trim(oc%nc%filename)
       if (.not.associated(oc%next)) exit
       oc => oc%next
    end do

  end subroutine glc_io_create_suffix_ccsm

end module glc_io
