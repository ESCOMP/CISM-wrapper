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

   use glc_KindsMod
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
   use glc_params
   use glimmer_log
   use glc_constants
   use glc_communicate, only: init_communicate

!lipscomb - debug
   use shr_sys_mod, only: shr_sys_flush

! !INPUT/OUTPUT PARAMETERS:

   integer (glc_i4), intent(inout) :: &
      errorCode              ! Returns an error code if any init fails

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------
!lipscomb - Not sure that all of these are needed here.
!           Some may be needed only for glc_run.

  character(fname_length) ::  &
      paramfile   ,&! Name of the top-level configuration file
      climatefile   ! Name of climate configuration file
 
  ! Scalars which hold information about the global grid --------------
 
  integer (glc_i4) ::  &
     nx,ny        ,&! Size of normal global grid
     nxo,nyo        ! Size of global orography grid
 
  integer (glc_i4) ::  &
     i,j            ! Array index counters

  integer (glc_i4) :: &
     nml_error      ! namelist i/o error flag

!lipscomb - debug
  integer (glc_i4), parameter :: itest = 130, jtest = 85

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

   write(*,*) 'jw getting ready to read namelist file'

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

   write(*,*) 'jw read namelist file'
   write(*,*) 'jw: climatefile = ', climatefile
   write(*,*) 'jw: paramfile =   ', paramfile
   call shr_sys_flush(6)

  ! Initialise climate
 
  call glex_clim_init (climate,climatefile)
 
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

  ! Set dimensions of global grids
  !lipscomb - Get these from glc_domain_size?
 
  call get_grid_dims(climate%all_grid,nx,ny) ! Normal global grid
  nxo=200 ; nyo=100                          ! Example grid used for orographic output
 
  ! start logging
  call open_log(unit=101, fname=logname(climatefile))  
 
  ! Allocate arrays appropriately
!lipscomb - These should be deallocated somewhere
 
  allocate(temp(nx,ny),precip(nx,ny),orog(nx,ny))
  allocate(orog_out(nxo,nyo),albedo(nx,ny),ice_frac(nx,ny),fw(nx,ny),fw_in(nx,ny))
  allocate(lats_orog(nyo),lons_orog(nxo))
  allocate(coverage(nx,ny),cov_orog(nxo,nyo))

  ! Initialise array contents
 
  temp     = c0
  precip   = c0
  albedo   = c0
  orog_out = c0

  orog = real(climate%orog_clim)                    ! Put orography where it belongs
 
  ! Set up global grids
 
  ! Calculate example orographic latitudes
 
  do j=1,nyo
     lats_orog(j)=-(180.0/nyo)*j+90.0+(90.0/nyo)
  enddo
 
  ! Calculate example orographic longitudes
 
  do i=1,nxo
     lons_orog(i)=(360.0/nxo)*i-(180.0/nxo)
  enddo
 
  ! Initialise the ice model
 
  call initialise_glint(ice_sheet, &
                        climate%all_grid%lats, &
                        climate%all_grid%lons, &
                        climate%climate_tstep, &
                        (/paramfile/), &
                        orog       = orog_out, &
                        ice_frac   = ice_frac, &
                        albedo     = albedo,   &
                        orog_longs = lons_orog, &
                        orog_lats  = lats_orog, &
                        daysinyear = climate%days_in_year)
 
  ! Set the message level (1 is the default - only fatal errors)
  ! N.B. Must do this after initialisation
 
  call glimmer_set_msg_level(6)
 
  ! Get coverage maps for the ice model instances
  !lipscomb - Is GLIMMER supposed to be able to do this? 
  if (glint_coverage_map(ice_sheet,coverage,cov_orog).ne.0) then
     call write_log('Unable to get coverage maps',GM_FATAL,__FILE__,__LINE__)
     stop
  endif

  ! Initialize time  !lipscomb - Remove later and get time from CCSM

  time = climate%climate_tstep   !lipscomb - just for now

   write(*,*) 'Initialized glint'
   call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_initialize

!***********************************************************************

 end module glc_InitMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
