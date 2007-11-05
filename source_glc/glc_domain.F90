!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_domain

!BOP
! !MODULE: glc_domain
!
! !DESCRIPTION:
!  This module contains the model domain and routines for initializing
!  the domain.  It also initializes the decompositions and
!  distributions across processors/threads by calling relevant
!  routines in the block, distribution modules.
!
! !REVISION HISTORY:
!  SVN:$Id: ice_domain.F90 66 2007-05-02 16:52:51Z dbailey $
!
! author: Phil Jones, LANL
!         Adapted from POP by William H. Lipscomb, LANL
!
! !USES:
!
   use glc_kinds_mod
   use glc_constants
   use glc_communicate
   use glc_broadcast
   use glc_blocks
   use glc_distribution
   use glc_exit_mod
   use glc_fileunits
   use glc_boundary
   use glc_domain_size

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS

   public  :: init_domain_blocks ,&
              init_domain_distribution

! !PUBLIC DATA MEMBERS:

   integer (int_kind), public :: &
      nblocks            ! actual number of blocks on this processor

   integer (int_kind), dimension(:), pointer, public :: &
      blocks_glc         ! block ids for local blocks

   type (distrb), public :: &
      distrb_info        ! block distribution info

   type (bndy), public :: &
      bndy_info          !  ghost cell update info

   logical (log_kind), public :: &
      ltripole_grid      ! flag to signal use of tripole grid

!EOP
!BOC
!-----------------------------------------------------------------------
!
!   module private variables - for the most part these appear as
!   module variables to facilitate sharing info between init_domain1
!   and init_domain2.
!
!-----------------------------------------------------------------------

    character (char_len) :: &
       distribution_type,   &! method to use for distributing blocks
       ew_boundary_type,    &! type of domain bndy in each logical
       ns_boundary_type      !    direction (ew is i, ns is j)

    integer (int_kind) :: &
       nprocs                ! num of processors

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: init_domain_blocks
! !INTERFACE:

 subroutine init_domain_blocks

! !DESCRIPTION:
!  This routine reads in domain information and calls the routine
!  to set up the block decomposition.
!
! !REVISION HISTORY:
!  same as module

! !USES:
!
   use glc_global_reductions
!
!EOP
!BOC
!----------------------------------------------------------------------
!
!  local variables
!
!----------------------------------------------------------------------

   integer (int_kind) :: &
      nml_error          ! namelist read error flag

!----------------------------------------------------------------------
!
!  input namelists
!
!----------------------------------------------------------------------

   namelist /domain_nml/ nprocs, &
                         distribution_type,  &
                         ew_boundary_type,   &
                         ns_boundary_type

!----------------------------------------------------------------------
!
!  read domain information from namelist input
!
!----------------------------------------------------------------------

#if (defined SEQ_MCT) || (defined CCSM)
   nprocs = get_num_procs()
#else
   nprocs = -1
#endif
   distribution_type = 'cartesian'
   ew_boundary_type  = 'cyclic'
   ns_boundary_type  = 'closed'

   if (my_task == master_task) then
      open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nu_nml, nml=domain_nml,iostat=nml_error)
	 if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
      end do
      if (nml_error == 0) close(nu_nml)
   endif

   call broadcast_scalar(nml_error, master_task)
   if (nml_error /= 0) then
      call exit_glc(sigAbort, 'glc: error reading domain_nml')
   endif

#if (!defined SEQ_MCT) || (!defined CCSM)
   call broadcast_scalar(nprocs,            master_task)
#endif
   call broadcast_scalar(distribution_type, master_task)
   call broadcast_scalar(ew_boundary_type,  master_task)
   call broadcast_scalar(ns_boundary_type,  master_task)

!----------------------------------------------------------------------
!
!  perform some basic checks on domain
!
!----------------------------------------------------------------------

   if (trim(ns_boundary_type) == 'tripole') then
      ltripole_grid = .true.
   else
      ltripole_grid = .false.
   endif

   if (nx_global < 1 .or. ny_global < 1) then
      !***
      !*** domain size zero or negative
      !***
      call exit_glc(sigAbort, 'glc: Invalid domain: size < 1') ! no domain
#if (!defined SEQ_MCT) || (!defined CCSM)
   else if (nprocs /= get_num_procs()) then
      !***
      !*** input nprocs does not match system (eg MPI) request
      !***
      call exit_glc(sigAbort, 'glc: Input nprocs not same as system request')
#endif
   else if (nghost < 1) then
      !***
      !*** must have at least 1 layer of ghost cells
      !***
      call exit_glc(sigAbort, 'glc: Not enough ghost cells allocated')
   endif

!----------------------------------------------------------------------
!  notify global_reductions whether tripole grid is being used
!----------------------------------------------------------------------

   call init_global_reductions (ltripole_grid)

!----------------------------------------------------------------------
!
!  compute block decomposition and details
!
!----------------------------------------------------------------------

   call create_blocks(nx_global, ny_global, trim(ew_boundary_type), &
                                            trim(ns_boundary_type))

!----------------------------------------------------------------------
!
!  Now we need grid info before proceeding further
!  Print some domain information
!
!----------------------------------------------------------------------

   if (my_task == master_task) then
     write(nu_diag,'(/,a18,/)')'Domain Information'
     write(nu_diag,'(a26,i6)') '  Horizontal domain: nx = ',nx_global
     write(nu_diag,'(a26,i6)') '                     ny = ',ny_global
     write(nu_diag,'(a26,i6)') '  Block size:  nx_block = ',nx_block
     write(nu_diag,'(a26,i6)') '               ny_block = ',ny_block
     write(nu_diag,'(a29,i6)') '  Processors: ', nprocs
     write(nu_diag,'(a31,a9)') '  Distribution: ', &
                                  trim(distribution_type)
     write(nu_diag,'(a25,i2,/)')'  Number of ghost cells: ', nghost
   endif

!----------------------------------------------------------------------
!EOC

 end subroutine init_domain_blocks

!***********************************************************************
!BOP
! !IROUTINE: init_domain_distribution
! !INTERFACE:

 subroutine init_domain_distribution(KMTG)

! !DESCRIPTION:
!  This routine calls appropriate setup routines to distribute blocks
!  across processors and defines arrays with block ids for any local
!  blocks. Information about ghost cell update routines is also
!  initialized here through calls to the appropriate boundary routines.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   real (r8), dimension(nx_global,ny_global), intent(in) :: &
      KMTG             ! global topography

!lipscomb - comment on KMTG 
! For POP and CICE, KMTG is used to distinguish ocean points from land points.  
! Here, KMTG should be an array whose values are <= 0 at points where 
!  ice sheets are unlikely to be present (e.g., the deep ocean).
! Might have to rewrite this subroutine.  For ont thing, not sure it makes
!  sense to have KMTG on the global grid.
! The safest thing is to pass in an array with KMTG > eps everywhere.
! Then no blocks will be removed.  Can worry later about efficient load balancing.
!
!EOP
!BOC
!----------------------------------------------------------------------
!
!  local variables
!
!----------------------------------------------------------------------

   character (char_len) :: outstring

   integer (int_kind), parameter :: &
      max_work_unit=10     ! quantize the work into values from 1,max

   integer (int_kind) :: &
      i,j,k,n            ,&! dummy loop indices
      ig,jg              ,&! global indices
      count1, count2     ,&! dummy counters
      work_unit          ,&! size of quantized work unit
      nblocks_tmp        ,&! temporary value of nblocks
      nblocks_max          ! max blocks on proc

   integer (int_kind), dimension(:), allocatable :: &
      nglc               ,&! number of "likely" ice sheet points per block
      work_per_block       ! number of work units per block

   type (block) :: &
      this_block           ! block information for current block

!----------------------------------------------------------------------
!
!  estimate the amount of work per processor using the topography
!
!----------------------------------------------------------------------

   allocate(nglc(nblocks_tot))

   nglc = 0
   do n=1,nblocks_tot
      this_block = get_block(n,n)
      !do i=this_block%ib,this_block%ie
      !	  ig = this_block%i_glob(i)
      !   jg = this_block%j_glob(j)
      !   if (KMTG(ig,jg) > eps .and.                        &
      !       (ULATG(ig,jg) < shlat/rad_to_deg .or.          &
      !        ULATG(ig,jg) > nhlat/rad_to_deg) )            & 
      !	      nocn(n) = nocn(n) + 1
      !end do
      !end do
      !do j=1,ny_block
      do j=this_block%jlo,this_block%jhi
         if (this_block%j_glob(j) > 0) then
            do i=1,nx_block
               if (this_block%i_glob(i) > 0) then
	          ig = this_block%i_glob(i)
                  jg = this_block%j_glob(j)
                  if (KMTG(ig,jg) > eps) nglc(n) = nglc(n) + 1
               endif
            end do
         endif
      end do

      !*** with array syntax, we actually do work on all points in block,
      !*** so where the block is not completely ice-free, reset nglc
      !*** to be the full size of the block

      ! NOTE - If array syntax use is limited, commenting out the next line
      !        might improve performance.
      if (nglc(n) > 0) nglc(n) = nx_block*ny_block

   end do

   work_unit = maxval(nglc)/max_work_unit + 1

   !*** find number of work units per block

   allocate(work_per_block(nblocks_tot))

   where (nglc > 0)
     work_per_block = nglc/work_unit + 1
   elsewhere
     work_per_block = 0
   end where
   deallocate(nglc)

!----------------------------------------------------------------------
!
!  determine the distribution of blocks across processors
!
!----------------------------------------------------------------------

   distrb_info = create_distribution(distribution_type, &
                                     nprocs, work_per_block)

   deallocate(work_per_block)

!----------------------------------------------------------------------
!
!  allocate and determine block id for any local blocks
!
!----------------------------------------------------------------------

   call create_local_block_ids(blocks_glc, distrb_info)

   if (associated(blocks_glc)) then
      nblocks = size(blocks_glc)
   else
      nblocks = 0
   endif
   nblocks_max = 0
   do n=0,distrb_info%nprocs - 1
     nblocks_tmp = nblocks
     call broadcast_scalar(nblocks_tmp, n)
     nblocks_max = max(nblocks_max,nblocks_tmp)
   end do

   if (nblocks_max > max_blocks) then
     write(outstring,*) &
         'glc: no. blocks exceed max: increase max to', nblocks_max
     call exit_glc(sigAbort, trim(outstring))
   else if (nblocks_max < max_blocks) then
     write(outstring,*) &
         'glc: no. blocks too large: decrease max to', nblocks_max
     if (my_task == master_task) write(nu_diag,*) trim(outstring)
   endif

!----------------------------------------------------------------------
!
!  Set up ghost cell updates for each distribution.
!  Boundary types are cyclic, closed, or tripole. 
!
!----------------------------------------------------------------------

   ! update ghost cells on all four boundaries
   call create_boundary(bndy_info, distrb_info,     &
                        trim(ns_boundary_type),     &
                        trim(ew_boundary_type),     &
                        nx_global, ny_global)

!----------------------------------------------------------------------
!EOC

 end subroutine init_domain_distribution

!***********************************************************************

 end module glc_domain

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
