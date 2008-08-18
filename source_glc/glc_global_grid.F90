!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_global_grid

!BOP
! !MODULE: glc_global_grid
!
! !DESCRIPTION:
!  This module contains grid info and routines for setting up the
!  global glc grid quantities.
!  
!  This module is very simple because all we need to do is read in
!   grid size, latitude, and longitude from a file.  This information
!  is needed to receive data from the coupler and pass it to GLINT.  
!
!  Later we may have a more sophisticated, POP-like routine to set up
!  the high-resolution regional ice sheet grid(s).
!
! !REVISION HISTORY:
!  SVN:$Id: grid.F90 808 2006-04-28 17:06:38Z njn01 $
!  Adapted by William Lipscomb from grid.F90 in POP
!
! !USES:

   use glc_kinds_mod
   use glc_communicate
   use glc_constants
   use glc_io_types, only: stdout, nml_in, nml_filename, get_unit, release_unit
   use glc_exit_mod
   use glint_global_grid
   
!lipscomb - debug
   use shr_sys_mod, only: shr_sys_flush

   implicit none
   private
   save

   public  :: init_glc_grid, region_mask, glc_grid, glint_grid

! !PUBLIC DATA MEMBERS:

   type(global_grid), pointer ::   &
      glc_grid        ,&! info (nx, ny, lat, lon, area) for coupling grid (S to N)
      glint_grid        ! info for glint grid (N to S)

   integer(i4), dimension(:,:), allocatable ::  &
      region_mask           ! mask, = 1 for ice sheet regions and 0 elsewhere
 
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  module private data
!
!-----------------------------------------------------------------------

   character (char_len) ::  &
      horiz_grid_opt,       &! option for getting horiz grid info
      horiz_grid_file,      &! input file for reading horiz grid info
      topo_varname,         &! variable name for topography
      region_mask_file       ! input file for region mask

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: init_glc_grid
! !INTERFACE:

 subroutine init_glc_grid

! !DESCRIPTION:
!  Initialize grid quantities
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   namelist /grid_nml/ horiz_grid_opt, horiz_grid_file, topo_varname,  &
                       region_mask_file

   integer (int_kind) :: &
      nml_error           ! namelist i/o error flag

!-----------------------------------------------------------------------
!
!  read input namelist for grid setup options
!
!-----------------------------------------------------------------------

   horiz_grid_opt       = 'unknown_horiz_grid_opt'
   horiz_grid_file      = 'unknown_horiz_grid_file'
   topo_varname         = 'unknown topo varname'
   region_mask_file     = 'unknown_region_mask'

   if (my_task == master_task) then
      open (nml_in, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nml_in, nml=grid_nml,iostat=nml_error)
      end do
      if (nml_error == 0) close(nml_in)
   endif

   if (nml_error /= 0) then
      call exit_glc(sigAbort,'ERROR reading grid_nml')
   endif

   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      write(stdout,blank_fmt)
      write(stdout,*) ' Grid:'
      write(stdout,blank_fmt)
      write(stdout,*) ' grid_nml namelist settings:'
      write(stdout,blank_fmt)
      write(stdout, grid_nml)
   endif

!-----------------------------------------------------------------------
!
!  output grid setup options to log file
!
!-----------------------------------------------------------------------

   if (my_task == master_task) then
      write(stdout,delim_fmt)
      write(stdout,blank_fmt)
      write(stdout,'(a13)') ' Grid options'
      write(stdout,blank_fmt)
   endif

!-----------------------------------------------------------------------
!
!  set up horizontal grid
!
!-----------------------------------------------------------------------

   select case (horiz_grid_opt)
   case ('internal')
      if (my_task == master_task) then
         write(stdout,'(a36)') ' Creating horizontal grid internally'
      endif
      call horiz_grid_internal
   case ('file')
      if (my_task == master_task) then
         write(stdout,*) 'Reading horizontal grid from file:', &
                          trim(horiz_grid_file)
      call shr_sys_flush(6)
      endif
      call read_horiz_grid(horiz_grid_file, topo_varname)
   case default
      call exit_glc(sigAbort,'ERROR: unknown horizontal grid option')
   end select

!-----------------------------------------------------------------------
!
!  set region-masks
!
!-----------------------------------------------------------------------

   if (trim(region_mask_file) /= 'unknown_region_mask') then
      if (my_task == master_task) write(stdout,'(a36,a)') &
         'Region masks initialized from file: ',trim(region_mask_file)
!lipscomb - need a new subroutine if this is to read from file
!!!      call area_masks(region_mask_file,region_info_file)
      call exit_glc(sigAbort, 'No subroutine for reading region mask from file')
   else
      if (my_task == master_task) then
!lipscomb - Include the entire global grid for now
!!!        write(stdout,'(a24)') ' No region masks defined'
         write(6,*) 'Set region_mask = 1 everywhere'

         allocate(region_mask(glc_grid%nx,glc_grid%ny))
         region_mask(:,:) = 1
      endif
   endif

!lipscomb - debug
   print*, 'Leaving init_glc_grid'
   write(6,*) 'itest, jtest, region_mask:', itest, jtest, region_mask(itest,jtest)
   call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 call flushm (stdout)

 end subroutine init_glc_grid

!***********************************************************************
!BOP
! !IROUTINE: horiz_grid_internal
! !INTERFACE:

 subroutine horiz_grid_internal

! !DESCRIPTION:
!  Creates a lat/lon grid with equal spacing in each direction
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      i,j,ig,jg,jm1,n    ! dummy counters

   real (r8) :: &
      dlat, dlon,       &! lat/lon spacing for idealized grid
      lathalf,          &! lat at T points
      xdeg               ! temporary longitude variable

   call exit_glc(sigAbort, 'Internal grid generation not yet enabled')

!lipscomb - Need a new routine here.  Most of POP routine is commented out.
!-----------------------------------------------------------------------
!
!  calculate lat/lon coords of U points
!  long range (-180,180)
!
!-----------------------------------------------------------------------

!      dlon = 360.0_r8/real(nx_global)
!      dlat = 180.0_r8/real(ny_global)

!      allocate (ULAT(nx_global, ny_global), &
!                ULON(nx_global, ny_global))

!      do i=1,nx_global
!         xdeg = i*dlon
!         if (xdeg > 180.0_r8) xdeg = xdeg - 360.0_r8
!         ULON(i,:) =  xdeg/radian
!      enddo

!      do j = 1,ny_global
!         ULAT(:,j)  = (-90.0_r8 + j*dlat)/radian
!      enddo

!-----------------------------------------------------------------------
!
!  calculate grid spacings and other quantities
!  compute here to avoid bad ghost cell values due to dropped land 
!  blocks
!
!-----------------------------------------------------------------------

!      deallocate(ULAT,ULON)

!-----------------------------------------------------------------------
!EOC

 end subroutine horiz_grid_internal

!***********************************************************************
!BOP
! !IROUTINE: read_horiz_grid
! !INTERFACE:

 subroutine read_horiz_grid(horiz_grid_file, topo_varname)

! !DESCRIPTION:
!  Reads horizontal grid and topography from input grid file
!
! !REVISION HISTORY:
!  same as module

! !USES:

   use glint_example_clim
   use glint_global_grid

! !INPUT PARAMETERS:

   character (*), intent(in) :: &
      horiz_grid_file  ,&! filename of file containing grid data
      topo_varname       ! variable name for topography
                         ! assume units are meters
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer(i4) :: i, j            ! indices
   integer(i4) :: nx, ny          ! global grid dimensions

   real(r8), dimension(:,:), pointer ::  &
      glc_topo       ! surface elevation, glc/clm grid

   real(r8) :: &
      latn, lats, lone, lonw   ! lat and lon of cell edges (radians)

!-----------------------------------------------------------------------
!
! Extract global grid and topo information from netCDF topography file.
! For now we use GLINT subroutines to do this.
!
!-----------------------------------------------------------------------

   call read_ncdf(horiz_grid_file, topo_varname, glc_topo, glc_grid)

   nx = glc_grid%nx
   ny = glc_grid%ny

!lipscomb - kludge
!lipscomb - GLIMMER assumes the grid is indexed N to S and therefore sets
!            lat_bound(1) = 90, lat_bound(ny+1) = -90.
!           Reverse that convention here.

   glc_grid%lat_bound(1)    = -90._r8
   glc_grid%lat_bound(ny+1) =  90._r8

!lipscomb - Another kludge - Make sure lon_bound > 0

   do i = 1, nx
      if (glc_grid%lon_bound(i) < 0._r8)   &
          glc_grid%lon_bound(i) = glc_grid%lon_bound(i) + 360._r8
   enddo
 
   ! Make sure elevations are in bounds 

   do j = 1, ny
   do i = 1, nx
 
      if (glc_topo(i,j) < -10000. .or. glc_topo(i,j) > 10000.) then
         write(stdout,*) 'Topo out of bounds: i, j, topo =', i, j, glc_topo(i,j)
         call exit_glc(sigAbort, 'topography out of bounds on glc_grid')
      endif

   enddo
   enddo

   ! compute grid cell area
   ! Note: Global grid is indexed from south to north, so the south edge of cell (i,j+1)
   !       is the north edge of cell (i,j)
!lipscomb - make sure lat_bound and lon_bound have correct dimensions
!lipscomb - convert from degrees to radians

   allocate(glc_grid%box_areas(nx,ny))

   do j = 1, ny
   do i = 1, nx

      latn = glc_grid%lat_bound(j+1) * pi/180._r8
      lats = glc_grid%lat_bound(j)   * pi/180._r8
      latn = p5*pi - latn  ! so lat = 0 at NP, = pi at SP
      lats = p5*pi - lats  
      lone = glc_grid%lon_bound(i+1) * pi/180._r8
      lonw = glc_grid%lon_bound(i)   * pi/180._r8
      if (lone < lonw) lone = lone + c2*pi
      glc_grid%box_areas(i,j) = radius**2 * (cos(latn)-cos(lats)) * (lone-lonw)

      ! Make sure area is positive
      if (glc_grid%box_areas(i,j) <= c0) then
         write(stdout,*) 'Negative area: i, j, area =', i, j, glc_grid%box_areas(i,j)
!lipscomb - debug
         write(stdout,*) 'latn, lats =', latn, lats
         write(stdout,*) 'cos(latn), cos(lats) =', cos(latn), cos(lats)
         write(stdout,*) 'lone, lonw =', lone, lonw
         write(stdout,*) 'latb(j), latb(j+1) =', glc_grid%lat_bound(j), &
                                                 glc_grid%lat_bound(j+1)
         write(stdout,*) 'lonb(i), lonb(i+1) =', glc_grid%lon_bound(i), &
                                                 glc_grid%lon_bound(i+1)

         call exit_glc(sigAbort, 'Negative gridcell area on glc grid')
      endif

   enddo
   enddo

!lipscomb - debug
   print*, ''
   print*, 'Horizontal grid:'
   print*, 'nx =', nx
   print*, 'ny =', ny
!   print*, 'lats =', glc_grid%lats(:)
!   print*, 'lons =', glc_grid%lons(:)
!   print*, 'lat_bound =', glc_grid%lat_bound(:)
!   print*, 'lon_bound =', glc_grid%lon_bound(:)
   print*, ''
   i = itest
   j = jtest
   print*, 'Test point, i, j, =', itest, jtest
   print*, 'lat, lon =', glc_grid%lats(j), glc_grid%lons(i)
   print*, 'area =', glc_grid%box_areas(i,j)
   print*, 'frac of earth =', glc_grid%box_areas(i,j) / (c4*pi*radius*radius)
   print*, 'Leaving read_horiz_grid'
   call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 end subroutine read_horiz_grid

!***********************************************************************
!lipscomb - Deleted most of the POP version; need a new version
!BOP
! !IROUTINE: area_masks
! !INTERFACE:

 subroutine area_masks(mask_filename,info_filename)

! !DESCRIPTION:
!   This subroutine reads in file with regional area mask
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   character(*), intent(in) :: &
      mask_filename           ,&! name of file containing region masks
      info_filename             ! name of file containing region names

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      k, n,              &! loop counters
      nx, ny,            &! global dimensions
      nu,                &! i/o unit number
      reclength,         &! record length of file
      ioerr,             &! i/o error flag
      region              ! region counter

   integer (int_kind), dimension(:,:), allocatable :: &
      REGION_G            ! global-sized region mask

!-----------------------------------------------------------------------
!
!  read in regional area masks
!
!-----------------------------------------------------------------------

   nx = glc_grid%nx
   nx = glc_grid%ny

   allocate(REGION_MASK(nx,ny))

   call get_unit(nu)
   if (my_task == master_task) then
      allocate(REGION_G(nx,ny))
      inquire (iolength=reclength) REGION_G
      open(nu, file=mask_filename,status='old',form='unformatted', &
               access='direct', recl=reclength, iostat=ioerr)
   endif

   if (ioerr /= 0) call exit_glc(sigAbort, &
                                 'Error opening region mask file')

   if (my_task == master_task) then
      read(nu, rec=1, iostat=ioerr) REGION_G
      close(nu)
   endif
   call release_unit(nu)

   if (ioerr /= 0) call exit_glc(sigAbort, &
                                 'Error reading region mask file')

   if (my_task == master_task) deallocate(REGION_G)

!-----------------------------------------------------------------------
!EOC

 end subroutine area_masks

!***********************************************************************
!***********************************************************************

 end module glc_global_grid

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

