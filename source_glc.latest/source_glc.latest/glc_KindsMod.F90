!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_KindsMod

!BOP
! !MODULE: glc_KindsMod
!
! !DESCRIPTION:
!  This module defines default numerical data types for all common data
!  types like integer, character, logical, real4 and real8.
!
! !USERDOC:
!  Users should not need to adjust anything in this module.  If various
!  character strings like long paths to files exceed the default
!  character length, the default value may be increased.
!
! !REFDOC:
!  This module is supplied to provide consistent data representation
!  across machine architectures.  It is meant to replace the old
!  Fortran double precision and real *X declarations that were
!  implementation-specific.
!  Users should not need to adjust anything in this module.  If various
!  character strings like long paths to files exceed the default
!  character length, the default value may be increased.
!
! !REVISION HISTORY:
!  SVN:$Id: POP_KindsMod.F90 808 2006-04-28 17:06:38Z njn01 $
!  glc version copied directly from POP_KindsMod.F90
!
! !USES:
!  uses no other modules

   implicit none
   private
   save

! !DEFINED PARAMETERS:

   integer, parameter, public ::                &
      glc_CharLength  = 100                    ,&
      glc_Logical     = kind(.true.)           ,&
      glc_i4          = selected_int_kind(6)   ,&
      glc_i8          = selected_int_kind(13)  ,&
      glc_r4          = selected_real_kind(6)  ,&
      glc_r8          = selected_real_kind(13)

!EOP
!BOC
!EOC
!***********************************************************************

 end module glc_KindsMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
