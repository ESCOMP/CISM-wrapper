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
!  Adapted by William Lipscomb from POP_InitMod.F90

! !USES:

   use glc_kinds_mod
   use glc_ErrorMod
   use glc_communicate, only: my_task, master_task
! lipscomb - added the following to change the start date after restart
   use glc_time_management, only: iyear0, imonth0, iday0, elapsed_days0,  &
                                  iyear,  imonth,  iday,  elapsed_days,   &
                                  ihour,  iminute, isecond, nsteps_total, &
                                  ymd2eday, eday2ymd 
! lipscomb - glc timer calls have been commented out
!!!   use glc_timers
    use glc_glint, only: glc_glint_initialize

!lipscomb - debug
   use glc_constants, only: verbose, nml_in, nml_filename, stdout
   use glc_exit_mod
 
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
                                tsfc, topo, qice, glc_nec
   use glc_global_fields, only: albedo, lats_orog, lons_orog ! to be removed?
   use glc_global_fields, only: time, coverage, cov_orog  ! to be removed?
   use glc_global_grid,   only: init_glc_grid, glc_grid, glint_grid
   use glimmer_log
   use glc_constants
   use glc_communicate, only: init_communicate
   use glc_time_management, only: init_time1, init_time2, dtt, ihour
!!   use glc_timers
   use glc_glint, only: glc_glint_initialize

!lipscomb - not sure we need init_global_reductions
!   use glc_global_reductions, only: init_global_reductions

!lipscomb - debug
   use glc_global_grid, only: region_mask
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

  integer (i4) :: &
      nhour_glint    ! number of hours since start of complete glint/glimmer run

  logical, parameter :: coupglc = .true.   ! temporary, in place of ifdef

  namelist /files_nml/  climatefile, paramfile
 
!lipscomb - Many of the calls in this routine are from POP.
!lipscomb - Some are commented out but may be added later.

!-----------------------------------------------------------------------
!  initialize return flag
!-----------------------------------------------------------------------

   ErrorCode = glc_Success

!-----------------------------------------------------------------------
!
!  initialize constants and i/o stuff
!
!-----------------------------------------------------------------------

!lipscomb - to do - Include this call later?
!!   call init_io
 
!-----------------------------------------------------------------------
!
!  write version information to output log after output redirection
!
!-----------------------------------------------------------------------

!lipscomb - to do - Write version info 
!!   if (my_task == master_task) then
!!      write(stdout,blank_fmt)
!!      write(stdout,ndelim_fmt)
!!      write(stdout,blank_fmt)
!!      write(stdout,'(a)') ' GLC version xxx '
!!      write(stdout,blank_fmt)
!!      call shr_sys_flush(stdout)
!!   endif
 
!lipscomb - to do - Init_constants if necessary
!!   call init_constants

!-----------------------------------------------------------------------
!
!  initialize domain and grid
!
!-----------------------------------------------------------------------
 
!lipscomb - to do - Initialize domain and grid.
!                   Currently the grid is based on GLIMMER/GLINT code
!!   call init_domain_blocks
!!   call init_grid1
!!   call init_domain_distribution(KMT_G)
!!   call init_grid2
 
!-----------------------------------------------------------------------
!
!  initialize timers and additional communication routines
!
!-----------------------------------------------------------------------
 
!!   call init_glc_timers

!-----------------------------------------------------------------------
!
!  compute time step and initialize time-related quantities
!
!-----------------------------------------------------------------------
 
   call init_time1
 
!-----------------------------------------------------------------------
!
!  set initial temperature and salinity profiles (includes read of
!  restart file)
!
!-----------------------------------------------------------------------
 
!lipscomb - Restart file is read later, in call to glc_glint_initialize
!!   call init_ts
 
!-----------------------------------------------------------------------
!
!  finish computing time-related quantities after restart info
!  available
!
!-----------------------------------------------------------------------
 
!lipscomb - This call moved to end of subroutine, after restart info is read
!!   call init_time2
 
!-----------------------------------------------------------------------
!
!  initialize diagnostics
!
!-----------------------------------------------------------------------

!lipscomb - to do - Initialize diagnostics?
!!   call init_diagnostics
 
!-----------------------------------------------------------------------
!
!  initialize output; subroutine init_output calls 
!       o init_restart
!       o init_history
!       o init_movie
!       o init_tavg
!
!-----------------------------------------------------------------------
 
!lipscomb - to do - Initialize output?
!!   call init_output

!-----------------------------------------------------------------------
!
!  initialize global budget diagnostics
!
!-----------------------------------------------------------------------

!lipscomb - to do - Initialize budget diagnostics?
!!   call init_budget_diagnostics
 
!-----------------------------------------------------------------------
!
!  write model information into log file
!
!-----------------------------------------------------------------------

!lipscomb - to do - Document constants?  
!!   call document_constants

!-----------------------------------------------------------------------
!
!  output delimiter to log file
!
!-----------------------------------------------------------------------
 
   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,'(" End of GLC initialization")')
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      call shr_sys_flush (stdout)
   endif
 
!--------------------------------------------------------------------
! Initialize ice sheet model, grid, and coupling.
! The following code is largely based on GLIMMER.
!-----------------------------------------------------------------------

   climatefile  = 'unknown_climatefile'
   paramfile    = 'unknown_paramfile'

!lipscomb - debug
   if (verbose) then
      write(stdout,*) 'GLC: Read namelist file'
      call shr_sys_flush(stdout) 
   endif

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
   if (verbose) then
      write(stdout,*) 'climatefile = ', climatefile
      write(stdout,*) 'paramfile =   ', paramfile
      write (stdout,*) 'dtt =', dtt
      call shr_sys_flush(stdout)
   endif

!lipscomb - to do - Uncomment the ifdef at some point?
!!#ifdef COUPGLC
  if (coupglc) then   ! temporary, in place of ifdef

!lipscomb - debug
   if (verbose) then
      write (stdout,*) 'GLC will exchange fields with the coupler'
      call shr_sys_flush(stdout)
   endif

  ! Initialize glc_grid (glc_grid, used for coupling)

!lipscomb - debug
   if (verbose) then
      write (stdout,*) 'Initialize glc_grid'
      call shr_sys_flush(stdout)
   endif

   call init_glc_grid

   if (verbose) then
      write (stdout,*) 'Initialize glint grid'
      call shr_sys_flush(stdout)
   endif

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
   ! latitude is S to N on glc_grid, N to S on glint_grid
   do j = 1, ny
      glint_grid%lats(j) = glc_grid%lats(ny-j+1)
   enddo

   do j = 1, ny+1
      glint_grid%lat_bound(j) = glc_grid%lat_bound(ny-j+2)
   enddo

   ! set values of climate derived type
!lipscomb - to do - Are other values needed?

   climate%climate_tstep = nint(dtt/3600._r8)   ! convert from sec to integer hours

!lipscomb - debug
   if (verbose) then
      write (stdout,*) 'climate_tstep (hr) =', climate%climate_tstep
      write (stdout,*) 'Initialize coupling'
      call shr_sys_flush(stdout)
   endif

!!!#else
 else   ! not coupled (temporary, in place of ifdef)

!lipscomb - debug
   write (stdout,*) 'GLC will read data from climate files'
   write (stdout,*) 'This code has not been tested!'
   write (stdout,*) 'Initialize climate'

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
  write (stdout,*) ' '
  write (stdout,*) 'Read climate file'
  i = itest
  j = jtest
  write (stdout,*) 'Test points: i, j =', i, j
  write (stdout,*) 'pclim =',  climate%pclim_load(i,j,1)
  write (stdout,*) 'stclim =', climate%stclim_load(i,j,1)
  write (stdout,*) 'orog =',   climate%orog_load(i,j)
  call shr_sys_flush(stdout)

!lipscomb - todo - Uncomment the ifdef?
!!!#endif
 endif    ! temporary, in place of ifdef

  ! start logging
  call open_log(unit=101, fname=logname(climatefile))  
 
!lipscomb - debug
  if (verbose) then
     write (stdout,*) 'Allocate global arrays'
     call shr_sys_flush(stdout)
  endif

  ! Allocate global arrays
  call glc_allocate_global(nx, ny, glc_nec)

  ! Initialize global arrays
 
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
 
!lipscomb - to do - Not sure that nxo and nyo are needed.
!                   Set nxo = nx and nyo = ny for now
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

!lipscomb - to do - Include optional argmt ice_dt to inform GLC of the ice dynamics timestep?

  nhour_glint = 0     ! number of hours glint has run since start of complete simulation
                      ! must be set to correct value if reading from a restart file
 
  call glc_glint_initialize(ice_sheet,             &
                            glint_grid,            &
                            climate%climate_tstep, &
                            (/paramfile/),         &
                            orog       = orog_out, &
                            daysinyear = climate%days_in_year,  &
                            start_time = nhour_glint)
 
  if (verbose) then
     write(stdout,*) 'Initialized glint, nhour_glint =', nhour_glint
     call shr_sys_flush(stdout)
  endif

  ! If restarting (nhour_glint > 0), recompute the year, month, and day
  ! Assume that ihour0 = iminute0 = isecond0 = 0
  ! Note that glint does not handle leap years

  if (nhour_glint > 0) then
     call ymd2eday (iyear0, imonth0, iday0, elapsed_days0)
     elapsed_days = elapsed_days0 + nhour_glint/24     
     call eday2ymd(elapsed_days, iyear, imonth, iday)
     ihour = 0
     iminute = 0
     isecond = 0
     nsteps_total = nhour_glint / climate%climate_tstep     
!lipscomb - debug
     write(6,*) 'Initial eday/y/m/d:', elapsed_days0, iyear0, imonth0, iday0
     write (6,*) 'eday/y/m/d after restart:', elapsed_days, iyear, imonth, iday
     write (6,*) 'nsteps_total =', nsteps_total
     call shr_sys_flush(stdout)
  endif

  ! Set the message level (1 is the default - only fatal errors)
  ! N.B. Must do this after initialization
 
  call glimmer_set_msg_level(6)
 
  ! Get coverage maps for the ice model instances

  !lipscomb - to do - Not sure this is needed

  if (glint_coverage_map(ice_sheet,coverage,cov_orog) /= 0) then
     call write_log('Unable to get coverage maps',GM_FATAL,__FILE__,__LINE__)
     stop
  endif

!-----------------------------------------------------------------------
!
!  finish computing time-related quantities after restart info
!  available
!
!-----------------------------------------------------------------------
!lipscomb - This subroutine assumes that iyear, imonth, and iday are known
!           after restart
 
   call init_time2
 
!-----------------------------------------------------------------------
!
!  output delimiter to log file
!
!-----------------------------------------------------------------------
 
   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,'(" End of GLC initialization")')
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      call shr_sys_flush (stdout)
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_initialize

!***********************************************************************

 end module glc_InitMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
