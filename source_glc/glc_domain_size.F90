!=======================================================================
!BOP
!
! !MODULE: glc_domain_size
!
! !DESCRIPTION:
!
! Defines the global domain size and number of categories and layers.
! Code originally based on domain_size.F in POP
!
! !REVISION HISTORY:
!  SVN:$Id: ice_domain_size.F90 66 2007-05-02 16:52:51Z dbailey $
!
! authors Elizabeth C. Hunke and William H. Lipscomb, LANL
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

      integer (kind=int_kind), parameter :: &
!!!        nx_global = NXGLOB    , & ! i-axis size
!!!        ny_global = NYGLOB    , & ! j-axis size
!lipscomb - hardwired for Greenland for now
        nx_global =  76       , & ! i-axis size
        ny_global = 141       , & ! j-axis size
        nilyr     =  10       , & ! number of ice layers
        ntrcr     =   2           ! number of tracers
                                  ! 1 = surface temperature
                                  ! 2 = ice age?

      integer (kind=int_kind), parameter :: &
!!!        block_size_x = BLCKX  , & ! size of block in first horiz dimension
!!!        block_size_y = BLCKY      ! size of block in second horiz dimension
!lipscomb - hardwired for now
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
