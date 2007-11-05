!=======================================================================
!BOP
!
! !MODULE: glc_domain_size
!
! !DESCRIPTION:
!
! Defines the global domain size for the glc component
! Might be better to read in nx_global and ny_global, allocate arrays dynamically
!
! !REVISION HISTORY:
!  SVN:$Id: ice_domain_size.F90 66 2007-05-02 16:52:51Z dbailey $
!
! author William H. Lipscomb, LANL
!
! !INTERFACE:
!
      module glc_domain_size
!
! !USES:
!
      use glc_kinds_mod
!
!EOP
!=======================================================================

      implicit none
      save

!lipscomb - These are hardwired for now.
!lipscomb - Would it be better to read them in at runtime?
      integer (i4), parameter :: &
!!!        nx_global = NXGLOB    , & ! i-axis size
!!!        ny_global = NYGLOB    , & ! j-axis size
        nx_global = 96     , & ! i-axis size, T31
        ny_global = 48         ! j-axis size, T31
!!!        nx_global = 128     , & ! i-axis size, T42
!!!        ny_global = 64          ! j-axis size, T42
!!!        nx_global = 144     , & ! i-axis size, FV1.9x2.5
!!!        ny_global = 96          ! j-axis size, FV1.9x2.5
!!!        nx_global = 288     , & ! i-axis size, FV1x1.25
!!!        ny_global = 180         ! j-axis size, FV1x1.25

      integer (kind=int_kind), parameter :: &
!!!        block_size_x = BLCKX  , & ! size of block in first horiz dimension
!!!        block_size_y = BLCKY      ! size of block in second horiz dimension
!lipscomb - hardwired for now; should be defined at runtime?
        block_size_x = nx_global  , & ! size of block in first horiz dimension
        block_size_y = ny_global      ! size of block in second horiz dimension

   !*** The model will inform the user of the correct
   !*** values for the parameter below.  A value higher than
   !*** necessary will not cause the code to fail, but will
   !*** allocate more memory than is necessary.  A value that
   !*** is too low will cause the code to exit.  
   !*** A good initial guess is found using
   !*** max_blocks = (nx_global/block_size_x)*(ny_global/block_size_y)/
   !***               num_procs
 
      integer (kind=int_kind), parameter :: &
!!!        max_blocks = MXBLCKS      ! max number of blocks per processor
!lipscomb - hardwired for now
        max_blocks = 1      ! max number of blocks per processor

!=======================================================================

      end module glc_domain_size

!=======================================================================
