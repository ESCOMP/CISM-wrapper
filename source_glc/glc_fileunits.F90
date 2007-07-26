!=======================================================================
!BOP
!
! !MODULE: glc_fileunits
!
! !DESCRIPTION:
!
! Defines unit numbers for files opened for reading or writing
!
! !REVISION HISTORY:
!  SVN:$Id: ice_fileunits.F90 58 2007-03-29 15:56:53Z eclare $
!
! author: Elizabeth C. Hunke and William H. Lipscomb, LANL
!
! !INTERFACE:
!
      module glc_fileunits
!
! !USES:
      use glc_kinds_mod
!
!EOP
!=======================================================================

      implicit none
      save

      character (len=char_len) :: &
         diag_type               ! 'stdout' or 'file'

      integer (kind=int_kind), parameter :: &
         nu_grid        = 11, &  ! grid file
         nu_kmt         = 12, &  ! land mask file
         nu_nml         = 21, &  ! namelist input file
         nu_forcing     = 49, &  ! forcing data file
         nu_dump        = 50, &  ! dump file for restarting
         nu_restart     = 50, &  ! restart input file
         nu_rst_pointer = 52, &  ! pointer to latest restart file
         nu_history     = 53, &  ! binary history output file
         nu_hdr         = 54     ! header file for binary history output

      integer (kind=int_kind) :: &
         nu_diag                 ! diagnostics output file

      character (6), parameter :: &
         nml_filename = 'glc_in' ! namelist input file name

!=======================================================================

      end module glc_fileunits

!=======================================================================
