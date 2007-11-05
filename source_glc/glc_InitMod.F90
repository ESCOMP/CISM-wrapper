!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_InitMod

!BOP
! !MODULE: glc_InitMod
! !DESCRIPTION:
!  This module contains the glc initialization method and initializes
!  everything needed by a glc simulation.  Primarily it is a driver
!  that calls individual initialization routines for each glc module.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  SVN:$Id: POP_InitMod.F90 808 2006-04-28 17:06:38Z njn01 $
!  WHL, May 2007: Adapted from POP_InitMod.F90 in POP 2.0

! !USES:

   use glc_kinds_mod
   use glc_ErrorMod
   use glc_io_types, only : nml_in, nml_filename, init_io
   use shr_msg_mod,  only : shr_msg_chdir, shr_msg_dirio

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_initialize

!EOP
!BOC
!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_initialize
! !INTERFACE:

 subroutine glc_initialize(errorCode)

! !DESCRIPTION:
!  This routine is the initialization driver that initializes a glc run 
!  by calling individual module initialization routines.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !USES:
   use glint_main
   use glint_example_clim
   use glc_global_fields, only: glc_allocate_global, climate, ice_sheet,   &
                                ice_frac, orog_out, temp, precip, orog,    &
                                tsfc, topo, qice, nec
   use glc_global_fields, only: albedo, lats_orog, lons_orog ! to be removed?
   use glc_global_fields, only: time, coverage, cov_orog  ! to be removed?
   use glc_global_grid,   only: init_glc_grid, glc_grid, glint_grid
   use glimmer_log
   use glc_constants
   use glc_communicate, only: init_communicate
   use glc_coupled, only: init_coupled

!lipscomb - new subroutine 
   use glc_glint, only: glc_glint_initialize

!lipscomb - just for now
   use glc_coupled, only: l_coupglc

!lipscomb - debug
   use shr_sys_mod, only: shr_sys_flush

! !INPUT/OUTPUT PARAMETERS:

   integer (i4), intent(inout) :: &
      errorCode              ! Returns an error code if any init fails

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

  character(fname_length) ::  &
      paramfile    ,&! Name of the top-level configuration file
      climatefile    ! Name of climate configuration file
 
  ! Scalars which hold information about the global grid --------------
 
  integer (i4) ::  &
      nx,ny,        &! Size of global glc grid
      nxo,nyo        ! Size of global orography grid
 
  integer (i4) ::  &
      i,j            ! Array index counters

  integer (i4) :: &
      nml_error      ! namelist i/o error flag

  namelist /files_nml/  climatefile, paramfile
 
!-----------------------------------------------------------------------
!  initialize return flag
!-----------------------------------------------------------------------

   ErrorCode = glc_Success

!--------------------------------------------------------------------
! CCSM-specific stuff to change the current working directory
!--------------------------------------------------------------------

   call shr_msg_chdir('glc')

!-----------------------------------------------------------------------
!  initialize message-passing or other communication protocol
!-----------------------------------------------------------------------

!lipscomb - debug
   write(6,*) 'Init communication'

   call init_communicate

!--------------------------------------------------------------------
! CCSM-specific stuff to redirect stdin,stdout
!--------------------------------------------------------------------

   call shr_msg_dirio('glc')
   !when parallel, this is usually done only for master_task

!-----------------------------------------------------------------------
! Initialize GLINT
!-----------------------------------------------------------------------

!lipscomb - Need another way of getting climatefile and paramfile 
 
!jw     ! These are the default inputs
!jw     Print*,'Enter name of climate configuration file:'
!jw     read*,climatefile
!jw     Print*,'Enter name of ice model configuration file:'
!jw     read*,paramfile

   climatefile  = 'unknown_climatefile'
   paramfile    = 'unknown_paramfile'

!lipscomb - debug
   write(6,*) 'Read namelist file'

   open (nml_in, file=nml_filename, status='old',iostat=nml_error)
   if (nml_error /= 0) then
      nml_error = -1
   else
      nml_error =  1
   endif
   do while (nml_error > 0)
      read(nml_in, nml=files_nml,iostat=nml_error)
   end do
   if (nml_error == 0) close(nml_in)

!lipscomb - debug
   write(6,*) 'Got namelist file'
   write(6,*) 'climatefile = ', climatefile
   write(6,*) 'paramfile =   ', paramfile
   call shr_sys_flush(6)


!lipscomb - Replace ifdef with logical if, at least for now
!!!#ifdef COUPGLC
 if (l_coupglc) then

!lipscomb - debug
   write (6,*) 'GLC will exchange fields with the coupler'
   call shr_sys_flush(6)

  ! Initialize glc_grid (glc_grid, used for coupling)

!lipscomb - debug
   write (6,*) 'Get glc_grid'
   call shr_sys_flush(6)

   call init_glc_grid

!lipscomb - debug
   write (6,*) 'Get glint grid'
   call shr_sys_flush(6)

   ! initialize GLINT grid (glint_grid, used for upscaling/downscaling)
 
   nx = glc_grid%nx
   ny = glc_grid%ny

   allocate(glint_grid)

   glint_grid%nx = nx
   glint_grid%ny = ny 

   allocate(glint_grid%lats(ny))
   allocate(glint_grid%lons(nx))
   allocate(glint_grid%lat_bound(ny+1))
   allocate(glint_grid%lon_bound(nx+1))
   allocate(glint_grid%box_areas(nx,ny))

   glint_grid%lons       (:) = glc_grid%lons(:)
   glint_grid%lon_bound  (:) = glc_grid%lon_bound(:)
   glint_grid%box_areas(:,:) = glc_grid%box_areas(:,:)

   ! switch latitude indices
   ! latitude is S to N on glc_grid, N to S on all_grid
   do j = 1, ny
      glint_grid%lats(j) = glc_grid%lats(ny-j+1)
   enddo

   do j = 1, ny+1
      glint_grid%lat_bound(j) = glc_grid%lat_bound(ny-j+2)
   enddo

!lipscomb - debug
   write (6,*) 'Initialize coupling'
   call shr_sys_flush(6)

!lipscomb - set values of climate derived type

  ! Initialize coupling

   call init_coupled

!!!#else
 else   ! not coupled

!lipscomb - debug
   write (6,*) 'GLC will read data from climate files'
   write (6,*) 'This part still needs to be tested'
   write (6,*) 'Initialize climate'

  ! Initialize climate, including grid
 
   call glex_clim_init (climate,climatefile)

   glint_grid = climate%all_grid

   nx = glint_grid%nx
   ny = glint_grid%ny

   ! initialize glc_grid

   allocate(glc_grid)

   glc_grid%nx = nx
   glc_grid%ny = ny 

   allocate(glc_grid%lats(ny))
   allocate(glc_grid%lons(nx))
   allocate(glc_grid%lat_bound(ny+1))
   allocate(glc_grid%lon_bound(nx+1))
   allocate(glc_grid%box_areas(nx,ny))

   glc_grid%lons      (:) = glint_grid%lons(:)
   glc_grid%lon_bound (:) = glint_grid%lon_bound(:)
   glc_grid%box_areas(:,:) = glint_grid%box_areas(:,:)

   ! switch latitude indices
   ! latitude is S to N on glc_grid, N to S on all_grid
   do j = 1, ny
      glc_grid%lats(j) = glint_grid%lats(ny-j+1)
   enddo

   do j = 1, ny+1
      glc_grid%lat_bound(j) = glint_grid%lat_bound(ny-j+2)
   enddo

!lipscomb - debug
  write (6,*) ' '
  write (6,*) 'Read climate file'
  i = itest
  j = jtest
  write (6,*) 'Test points: i, j =', i, j
  write (6,*) 'pclim =',  climate%pclim_load(i,j,1)
  write (6,*) 'stclim =', climate%stclim_load(i,j,1)
  write (6,*) 'orog =',   climate%orog_load(i,j)
  call shr_sys_flush(6)

!!!#endif
 endif  ! l_coupglc

  ! start logging
  call open_log(unit=101, fname=logname(climatefile))  
 
!lipscomb - debug
   write (6,*) 'Allocate global arrays'
   call shr_sys_flush(6)

  ! Allocate global arrays
  call glc_allocate_global(nx, ny, nec)

!lipscomb - Initialize tsfc, qice, topo?
  ! Initialize arrays
 
  tsfc(:,:,:)   = c0
  topo(:,:,:)   = c0
  qice(:,:,:)   = c0

  temp(:,:)     = c0
  precip(:,:)   = c0
  albedo(:,:)   = c0
  orog_out(:,:) = c0

!lipscomb - if coupled?   
!!!  orog = real(climate%orog_clim)                    ! Put orography where it belongs

  ! Calculate example orographic latitudes
 
!lipscomb - Set nxo = nx and nyo = ny for now
!!!  nxo=200 ; nyo=100                  ! Example grid used for orographic output
  nxo = nx
  nyo = ny

  do j=1,nyo
     lats_orog(j)=-(180.0/nyo)*j+90.0+(90.0/nyo)
  enddo
 
  ! Calculate example orographic longitudes
 
  do i=1,nxo
     lons_orog(i)=(360.0/nxo)*i-(180.0/nxo)
  enddo
 
  ! Initialize the ice model

!lipscomb - Get tstep and daysinyear for coupled case
!lipscomb - anything else to pass?
 
!lipscomb - debug
   write (6,*) 'Initialize glint'
   call shr_sys_flush(6)

  call glc_glint_initialize(ice_sheet,             &
                            glint_grid,            &
                            climate%climate_tstep, &
                            (/paramfile/),         &
                            orog       = orog_out, &
                            daysinyear = climate%days_in_year)
 
   write(*,*) 'Initialized glint'
   call shr_sys_flush(6)

  ! Set the message level (1 is the default - only fatal errors)
  ! N.B. Must do this after initialization
 
  call glimmer_set_msg_level(6)
 
  ! Get coverage maps for the ice model instances
  !lipscomb - Is GLIMMER supposed to be able to do this? 
  if (glint_coverage_map(ice_sheet,coverage,cov_orog) /= 0) then
     call write_log('Unable to get coverage maps',GM_FATAL,__FILE__,__LINE__)
     stop
  endif

  ! Initialize time  !lipscomb - Remove later and get time from CCSM

   time = climate%climate_tstep   !lipscomb - just for now

   write(*,*) 'Initialized time:', time
   call shr_sys_flush(6)


!-----------------------------------------------------------------------
!EOC

 end subroutine glc_initialize

!***********************************************************************

 end module glc_InitMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
