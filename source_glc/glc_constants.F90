!=======================================================================
!BOP
!
! !MODULE: glc_constants - constants used by glc modules
!
  module glc_constants

! !DESCRIPTION:
!
! This module contains constants used by glc modules.
!
! Note that many of the required parameters are contained
! in glimmer_physcon and glimmer_params.
! Many of the parameters defined here are standard constants in POP.
!
! !REVISION HISTORY:
!  Author: William Lipscomb, LANL

! !USES:

  use glc_kinds_mod
  use shr_const_mod, only: radius=> SHR_CONST_REARTH,&
        	 	   tkfrz=>  SHR_CONST_TKFRZ

!lipscomb - Previously, stdout was defined in glc_constants.
!           Moved to glimmer_paramets so that it can be accessed from
!            CISM source code as well as glc source code.
!           CISM does most of its standard output by calling the
!            write_log subroutine, which has a private output index
!            called glimmer_unit, but it is convenient sometimes to
!            write diagnostics directly to stdout.  
!           In CESM runs, glimmer_unit is set to stdout at initialization. 

  use glimmer_paramets, only: stdout
!EOP
!=======================================================================

  implicit none
  public

  include 'netcdf.inc'

  ! ------------------------------------------------------------------------
  ! which ice sheets are included in this run
  ! ------------------------------------------------------------------------

  integer, parameter :: max_icesheets = 10

  integer :: num_icesheets = 0  ! number of ice sheets in this run

  integer, parameter :: icesheet_name_len = 32
  character(icesheet_name_len) :: icesheet_names(max_icesheets)

   !-----------------------------------------------------------------
   ! elevation class info
   !-----------------------------------------------------------------

  logical, parameter :: verbose = .false.

  logical ::   &
     glc_smb              ! if true, get surface mass balance from CLM via coupler
                          ! (in multiple elevation classes)
                          ! if false, use PDD scheme in CISM
                          ! set in glc_cpl_indices_set

   !-----------------------------------------------------------------
   ! other info controlling this run
   !-----------------------------------------------------------------

  logical :: &
       zero_gcm_fluxes_for_all_icesheets = .false.  ! If zero_gcm_fluxes is true for all ice sheets

  logical :: &
       test_coupling   = .false.  ! If true, force frequent coupling for testing purposes

  logical :: &
       enable_frac_overrides = .false. ! If true, enable overrides in glc_override_frac

   !-----------------------------------------------------------------
   !  common formats for formatted output
   !-----------------------------------------------------------------

   integer (i4), public :: &
!!    stdout,            &! reserved unit for standard output
                          ! see note above
      stderr              ! reserved unit for standard error

   character (1), parameter, public :: &
      char_delim = ','
 
   character (9), parameter, public :: &
      delim_fmt  = "(72('-'))",         &
      ndelim_fmt = "(72('='))"

   character (5), parameter, public :: &
      blank_fmt = "(' ')"

   character (char_len), public ::  &
      char_blank          ! empty character string

   !-----------------------------------------------------------------
   ! numbers
   !-----------------------------------------------------------------
 
   real (r8), parameter, public :: &
      c0     =    0.0_r8   ,&
      c1     =    1.0_r8

   !-----------------------------------------------------------------
   ! miscellaneous info
   !-----------------------------------------------------------------

   character (char_len_long) :: model_doi_url

!EOP
!

!------------------------------------------------------------------------

  end module glc_constants

!------------------------------------------------------------------------
