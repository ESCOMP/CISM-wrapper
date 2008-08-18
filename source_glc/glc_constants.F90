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
! in glimmer_physcon and glimmer_params.  The ones defined here
! are mostly some standard constants used in POP and CICE.
!
! !REVISION HISTORY:
!  Author: William Lipscomb, LANL

! !USES:

  use glc_kinds_mod
  use shr_const_mod

!EOP
!=======================================================================

  implicit none

  include 'netcdf.inc'

   !-----------------------------------------------------------------
   ! test point for debugging
   !-----------------------------------------------------------------

!lipscomb - debug
  integer(i4), parameter ::  gtest = 1864   ! test grid cell, global index
   integer(i4), parameter ::  &
      itest = 84, jtest = 42,  &      ! in Greenland (T31)
                 jjtest = 49 - jtest  ! reversed for N to S indexing (T31)

   integer(i4), parameter ::  &
      itest_local = 24, jtest_local = 45  ! Greenland local grid

!lipscomb - for debugging
   logical :: verbose = .true.   ! if true, write diagnostics useful for debugging
!!   logical :: verbose = .false.   ! if true, write diagnostics useful for debugging

   !-----------------------------------------------------------------
   ! physical constants
   !-----------------------------------------------------------------

!lipscomb - Add an ifdef here?
   real(r8) :: radius = SHR_CONST_REARTH  ,&! radius of earth (m)
                                            ! = 6.37122e6
               tkfrz  = SHR_CONST_TKFRZ     ! freezing temp of water (K)
                                            ! = 273.15
   !-----------------------------------------------------------------
   ! parameters for downscaling
   !-----------------------------------------------------------------
!lipscomb - This should be consistent with the value in CLM (in clm_atmlnd.F90)
!lipscomb - to do - Make this a shared constant?

   real(r8), parameter :: lapse = 0.0065_r8   ! atm lapse rate, deg/km

!lipscomb - The remaining constants are from POP

   !-----------------------------------------------------------------
   ! numbers
   !-----------------------------------------------------------------
 
   real (r8), parameter, public :: &
      c0     =    0.0_r8   ,&
      c1     =    1.0_r8   ,&
      c2     =    2.0_r8   ,&
      c3     =    3.0_r8   ,&
      c4     =    4.0_r8   ,&
      c5     =    5.0_r8   ,&
      c8     =    8.0_r8   ,&
      c10    =   10.0_r8   ,&
      c16    =   16.0_r8   ,&
      c100   =  100.0_r8   ,&
      c1000  = 1000.0_r8   ,&
      c1p5   =    1.5_r8   ,&
      p333   = c1/c3       ,&
      p5     = 0.500_r8    ,&
      p25    = 0.250_r8    ,&
      p125   = 0.125_r8    ,&
      p001   = 0.001_r8    ,&
      eps    = 1.0e-10_r8  ,&
      eps2   = 1.0e-20_r8  ,&
      bignum = 1.0e+30_r8  ,&
      pi     = 3.14159265358979_r8,&
      pi2    = c2*pi       ,&
      pih    = p5*pi       ,&
      radian = 180.0_r8/pi

   real (r4), parameter, public ::       &
      undefined_nf_r4  = NF_FILL_FLOAT,  &
      undefined        = -12345._r4
 
   integer (int_kind), parameter, public ::   &
      undefined_nf_int = NF_FILL_INT
 
   !-----------------------------------------------------------------
   ! location of fields for staggered grids
   !-----------------------------------------------------------------
 
   integer (int_kind), parameter :: &   
      field_loc_unknown  =  0, & 
      field_loc_noupdate = -1, & 
      field_loc_center   =  1, & 
      field_loc_NEcorner =  2, & 
      field_loc_Nface    =  3, & 
      field_loc_Eface    =  4, &
      field_loc_Wface    =  5
 
 
   !-----------------------------------------------------------------
   ! field type attribute - necessary for handling
   ! changes of direction across tripole boundary
   !-----------------------------------------------------------------
 
   integer (int_kind), parameter :: &   
      field_type_unknown  =  0, & 
      field_type_noupdate = -1, & 
      field_type_scalar   =  1, & 
      field_type_vector   =  2, & 
      field_type_angle    =  3

   !-----------------------------------------------------------------
   !  common formats for formatted output
   !-----------------------------------------------------------------
 
   character (1), parameter, public :: &
      char_delim = ','
 
   character (9), parameter, public :: &
      delim_fmt  = "(72('-'))",         &
      ndelim_fmt = "(72('='))"

   character (5), parameter, public :: &
      blank_fmt = "(' ')"

   character (char_len), public ::  &
      char_blank          ! empty character string

!EOP
!

!------------------------------------------------------------------------

  end module glc_constants

!------------------------------------------------------------------------
