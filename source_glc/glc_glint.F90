
module glc_glint

!lipscomb - new module
! This module contains glint subroutines that have been modified for the
! case that the surface mass balance is computed externally and passed 
! from glc to glint.        
! 
! This module downscales input fields from the global grid (with multiple
! elevation classes) to the local ice sheet grid, and also upscales output
! fields from the local grid to the global grid.
!
  use glimmer_global
  use glint_type
  use glint_global_grid
  use glint_constants

  use glc_constants

!lipscomb - do we have to use nec?
  use glc_global_fields

!lipscomb - Might be better to pass parameters explicitly?
  use glint_main, only: glint_params, splice_field, check_mbts

!lipscomb - for diagnostics
  use glide_diagnostics
 
!lipscomb - debug
  use shr_sys_mod, only: shr_sys_flush
  use glc_exit_mod

!lipscomb - Should these be pointers?  Corresponding glint arrays are pointers.
  real(r8), dimension(:,:,:), allocatable ::  &
     tsfc_av     ,&! averaging array for tsfc
     topo_av     ,&! averaging array for topo
     qice_av       ! averaging array for qice

  real(r8), dimension(:,:,:), allocatable ::   &
     gfrac_temp    ,&! gfrac for a single instance
     gthck_temp    ,&! gthck for a single instance
     gtopo_temp    ,&! gtopo for a single instance
     ghflx_temp      ! ghflx for a single instance

  real(r8), dimension(:,:), allocatable ::   &
     groff_temp      ! groff for a single instance

  private
  public glc_glint_initialize, glc_glint_driver
 
  contains

!================================================================================

  subroutine glc_glint_initialize(params,        &
                                  glint_grid,    &
                                  time_step,     &
                                  paramfile,     &
                                  orog,          &         
!                                  albedo,       &
!                                  ice_frac,     &
!                                  veg_frac,     &
!                                  snowice_frac, &
!                                  snowveg_frac, &
!                                  snow_depth,                 &
!                                  orog_lats,    orog_lons,    &
!                                  orog_latb,    orog_lonb,    &
                                  output_flag,  &
                                  daysinyear,   &
!                                  snow_model,                 &
                                  ice_dt,       &
                                  extraconfigs, &
                                  start_time)
 
    ! Initialize the model
    ! This subroutine is based on subroutine initialise_glint in GLIMMER.
    ! It is designed for use with the glc wrapper which passes an
    !  externally computed mass balance to GLINT.
 
    use glimmer_config
    use glint_initialise
    use glimmer_log

!lipscomb - added these
    use glint_main, only: check_init_args, glint_readconfig, glint_allocate_arrays

    implicit none
 
    ! Subroutine argument declarations --------------------------------------------------------
 
    type(glint_params),              intent(inout) :: params      !*FD parameters to be set
    type(global_grid),               intent(in)    :: glint_grid  !*FD global grid
    integer,                         intent(in)    :: time_step   !*FD Timestep of calling model (hours)
    character(*),dimension(:),       intent(in)    :: paramfile   !*FD array of configuration file names.
    real(rk),dimension(:,:),optional,intent(out)   :: orog        !*FD Initial global orography
!    real(rk),dimension(:,:),optional,intent(out)   :: albedo      !*FD Initial albedo
!    real(rk),dimension(:,:),optional,intent(out)   :: ice_frac    !*FD Initial ice fraction 
!    real(rk),dimension(:,:),optional,intent(out)   :: veg_frac    !*FD Initial veg fraction
!    real(rk),dimension(:,:),optional,intent(out)   :: snowice_frac !*FD Initial snow-covered ice fraction
!    real(rk),dimension(:,:),optional,intent(out)   :: snowveg_frac !*FD Initial snow-covered veg fraction
!    real(rk),dimension(:,:),optional,intent(out)   :: snow_depth  !*FD Initial snow depth 
!    real(rk),dimension(:),  optional,intent(in)    :: orog_lats   !*FD Latitudinal location of gridpoints 
!                                                                  !*FD for global orography output
!    real(rk),dimension(:),  optional,intent(in)    :: orog_lons   !*FD Longitudinal location of gridpoints 
!                                                                  !*FD for global orography output
!    real(rk),dimension(:),  optional,intent(in)    :: orog_latb   !*FD Locations of the latitudinal 
!                                                                  !*FD boundaries of the grid-boxes (orography).
!    real(rk),dimension(:),  optional,intent(in)    :: orog_lonb   !*FD Locations of the longitudinal
!                                                                  !*FD boundaries of the grid-boxes (orography).
    logical,                optional,intent(out)   :: output_flag !*FD Flag to show output set (provided for
                                                                  !*FD consistency)
    integer,                optional,intent(in)    :: daysinyear  !*FD Number of days in the year
!    logical,                optional,intent(out)   :: snow_model  !*FD Set if the mass-balance scheme
                                                                  !    has a snow-depth model
    integer,                optional,intent(out)   :: ice_dt      !*FD Ice dynamics time-step in hours
    type(ConfigData),dimension(:),optional ::  extraconfigs !*FD Additional configuration information - overwrites
                                                                  !*FD config data read from files
    integer,                optional,intent(in)    :: start_time  !*FD Time of first call to glint (hours)
 
    ! Internal variables -----------------------------------------------------------------------
 
    type(ConfigSection), pointer :: global_config, instance_config, section  ! configuration stuff
    character(len=100) :: message                                            ! For log-writing
    character(fname_length),dimension(:),pointer :: config_fnames=>null()    ! array of config filenames
    type(ConfigSection), pointer :: econf

    real(rk),dimension(:,:),allocatable :: orog_temp

    integer :: i, n

    integer,dimension(:),allocatable :: mbts,idts ! Array of mass-balance and ice dynamics timesteps
 
    integer(i4) :: nx, ny  ! global grid dimensions

    real(r8) :: timeyr   ! time in years

!lipscomb - debug
    write(6,*) 'In glc_glint_initialize'
    call shr_sys_flush(6)

    ! Initialise start time and calling model time-step --------------------------------
    ! We ignore t=0 by default 
 
    params%time_step = time_step
    if (present(start_time)) then
       params%start_time = start_time
    else
       params%start_time = time_step
    end if
 
    params%next_av_start = params%start_time
 
    ! Initialise year-length -----------------------------------------------------------
 
    if (present(daysinyear)) then
       call glint_set_year_length(daysinyear)
    end if
 
!lipscomb - debug
    write(6,*) 'time_step (hr)  =', params%time_step
    write(6,*) 'start_time (hr) =', params%start_time
    write(6,*) 'Initialize params%g_grid'
    call shr_sys_flush(6)

!lipscomb - Not sure if we need to define params%g_grid, but do this to be safe
    ! Initialise main global grid -----------------------------------------------------
 
   nx = glint_grid%nx
   ny = glint_grid%ny

!lipscomb - debug
    write(6,*) 'nx, ny =', nx, ny
    call shr_sys_flush(6)

   params%g_grid%nx = nx
   params%g_grid%ny = ny 

   allocate(params%g_grid%lats(ny))
   allocate(params%g_grid%lons(nx))
   allocate(params%g_grid%lat_bound(ny+1))
   allocate(params%g_grid%lon_bound(nx+1))
   allocate(params%g_grid%box_areas(nx,ny))

   params%g_grid%lons       (:) = glint_grid%lons(:)
   params%g_grid%lats       (:) = glint_grid%lats(:)
   params%g_grid%lon_bound  (:) = glint_grid%lon_bound(:)
   params%g_grid%lat_bound  (:) = glint_grid%lat_bound(:)
   params%g_grid%box_areas(:,:) = glint_grid%box_areas(:,:)

    ! Initialise orography grid ------------------------------------
 
!lipscomb - Will not use orography grid; use elevation classes instead
!lipscomb - Still need to define params%g_grid_orog?
!    call check_init_args(orog_lats, orog_lons,    &
!                         orog_latb, orog_lonb)
 
!    if (present(orog_lats) .and. present(orog_lons)) then
!       call new_global_grid(params%g_grid_orog,           &
!                            orog_lons,       orog_lats,   &
!                            lonb=orog_lonb,  latb=orog_latb)
!    else
       call copy_global_grid(params%g_grid,  &
                             params%g_grid_orog)
!    end if
 
!lipscomb - debug
    write(6,*) 'Allocate arrays'
    call shr_sys_flush(6)

    ! Allocate and initialize arrays
 
    allocate(tsfc_av(nx,ny,nec))
    allocate(qice_av(nx,ny,nec))
    allocate(topo_av(nx,ny,nec))

    tsfc_av(:,:,:) = c0
    qice_av(:,:,:) = c0
    topo_av(:,:,:) = c0

    call glint_allocate_arrays(params)

!lipscomb - debug
    write(6,*) 'Initialize maps'
    call shr_sys_flush(6)

    ! ---------------------------------------------------------------
    ! Zero coverage maps and normalisation fields for main grid and
    ! orography grid
    ! ---------------------------------------------------------------
 
    params%total_coverage = c0
    params%total_cov_orog = c0
 
!lipscomb - debug
    write(6,*) 'cov_norm'
    call shr_sys_flush(6)
    params%cov_normalise = c0
    params%cov_norm_orog = c0
 
    ! ---------------------------------------------------------------
    ! Determine how many instances there are, according to what
    ! configuration files we've been provided with
    ! ---------------------------------------------------------------
 
!lipscomb - debug
    write(6,*) 'Determine no. of instances'
    call shr_sys_flush(6)

    if (size(paramfile)==1) then

       ! Load the configuration file into the linked list
       call ConfigRead(paramfile(1),global_config)   

       ! Parse the list
       call glint_readconfig(global_config,params%ninstances,config_fnames,paramfile) 

    else
       params%ninstances = size(paramfile)
       allocate(config_fnames(params%ninstances))
       config_fnames = paramfile
    end if
 
    allocate(params%instances(params%ninstances))
    allocate(mbts(params%ninstances),idts(params%ninstances))
 
!lipscomb - debug
    write(6,*) 'Read config files'
    call shr_sys_flush(6)

    ! ---------------------------------------------------------------
    ! Read config files, and initialise instances accordingly
    ! ---------------------------------------------------------------
 
    call write_log('Reading instance configurations')
    call write_log('-------------------------------')
 
    do i=1,params%ninstances

       call ConfigRead(config_fnames(i),instance_config)
       if (present(extraconfigs)) then
          if (size(extraconfigs)>=i) then
             call ConfigCombine(instance_config,extraconfigs(i))
          end if
       end if

!lipscomb - debug
    write(6,*) 'Call glint_i_initialise, instance =', i
    call shr_sys_flush(6)

!lipscomb - any changes here?
! note - upscale and downscale params computed here
       call glint_i_initialise (instance_config,       &
                                params%instances(i),   &
                                params%g_grid,         &
                                params%g_grid_orog,    &
                                mbts(i),               &
                                idts(i),               &
                                params%need_winds,     &
                                params%enmabal,        &
                                params%start_time,     &
                                params%time_step)
 
       params%total_coverage = params%total_coverage + params%instances(i)%frac_coverage
       params%total_cov_orog = params%total_cov_orog + params%instances(i)%frac_cov_orog
 
       where (params%total_coverage > c0) params%cov_normalise = params%cov_normalise + c1
       where (params%total_cov_orog > c0) params%cov_norm_orog = params%cov_norm_orog + c1
 
!lipscomb - initial ice sheet diagnostics 
       timeyr = c0
       print*, 'Write diagnostics, time (yr)=', timeyr
       call glide_write_diag(params%instances(i)%model, timeyr, itest, jtest)
 
    end do
 
!lipscomb - debug
    write(6,*) 'check mbts'
    call shr_sys_flush(6)

    ! Check that all mass-balance time-steps are the same length and 
    ! assign that value to the top-level variable
 
    params%tstep_mbal = check_mbts(mbts)
    if (present(ice_dt)) then
       ice_dt = check_mbts(idts)
    end if
 
    ! Check that time steps divide into one another appropriately.
 
    if (.not. (mod(params%tstep_mbal,params%time_step)==0)) then
       print*,params%tstep_mbal,params%time_step
       call write_log('The mass-balance timestep must be an integer multiple of the forcing time-step', &
            GM_FATAL,__FILE__,__LINE__)
    end if
 
    ! Check we don't have coverage greater than one at any point.
 
    where (params%total_coverage > c1) params%total_coverage = c1
    where (params%total_cov_orog > c1) params%total_cov_orog = c1
    params%coverage_calculated = .true.
 
    ! Zero optional outputs, if present
 
!lipscomb - not sure if orog should be passed out
    if (present(orog)) orog = c0

!lipscomb - set other fields to zero?
 
    ! Allocate arrays
 
    allocate(orog_temp(nx,ny))

!lipscomb - added these fields
    allocate(gfrac_temp(nx,ny,nec))
    allocate(gthck_temp(nx,ny,nec))
    allocate(gtopo_temp(nx,ny,nec))
    allocate(ghflx_temp(nx,ny,nec))
    allocate(groff_temp(nx,ny))

!lipscomb - debug
    write(6,*) 'Get initial fields and splice'
    call shr_sys_flush(6)

    ! Get initial fields from instances, splice together and return
 
    do i = 1, params%ninstances

!lipscomb - add groff to output?

       call get_glc_upscaled_fields(params%instances(i),             &
                                    gfrac_temp,       gthck_temp,   &
                                    gtopo_temp,       ghflx_temp)

       do n = 1, nec

          gfrac(:,:,n) = splice_field(gfrac(:,:,n),                      &
                                      gfrac_temp(:,:,n),                 &
                                      params%instances(i)%frac_coverage, &
                                      params%cov_normalise)

          gthck(:,:,n) = splice_field(gthck(:,:,n),                      &
                                      gthck_temp(:,:,n),                 &
                                      params%instances(i)%frac_coverage, &
                                      params%cov_normalise)

          gtopo(:,:,n) = splice_field(gtopo(:,:,n),                      &
                                      gtopo_temp(:,:,n),                 &
                                      params%instances(i)%frac_coverage, &
                                      params%cov_normalise)

          ghflx(:,:,n) = splice_field(ghflx(:,:,n),                      &
                                      ghflx_temp(:,:,n),                 &
                                      params%instances(i)%frac_coverage, &
                                      params%cov_normalise)
 
       enddo   ! nec

    end do   ! ninstances
 
    ! Deallocate
 
    deallocate(gfrac_temp)
    deallocate(gthck_temp)
    deallocate(gtopo_temp)
    deallocate(ghflx_temp)
!lipscomb - runoff?
 
!lipscomb - debug
    write(6,*) 'Leaving glc_glint_initialize'
    call shr_sys_flush(6)

  end subroutine glc_glint_initialize
 
!================================================================================
 
  subroutine glc_glint_driver(params,         time,            &
                              tsfc,           qice,            &
                              topo,                            &
                              gfrac,          gthck,           &
                              gtopo,          ghflx,           &
                              groff,          l_ice_tstep)
 
    ! This subroutine is based on subroutine glint.
    ! It is designed for coupling to an external surface mass balance model
    ! (e.g., the glacier surface mass balance model in the land component of CCSM).
    !           
    ! This subroutine does spatial and temporal averaging of input fields
    ! and calls the ice sheet dynamics model when required.
    ! Input fields should be taken as means over the period since the last call.
    !
 
    use glimmer_utils
    use glint_interp
    use glint_timestep
    use glimmer_log

    implicit none
 
    ! Subroutine argument declarations -------------------------------------------------------------
 
    type(glint_params),       intent(inout) :: params   ! parameters for this run
    integer,                  intent(in)    :: time     ! Current model time (hours)
    real(r8),dimension(:,:,:),intent(in)    :: tsfc     ! surface ground temperature (C)
    real(r8),dimension(:,:,:),intent(in)    :: qice     ! flux of glacier ice (kg/m^2/s)
    real(r8),dimension(:,:,:),intent(in)    :: topo     ! surface elevation (m)
 
    real(r8),dimension(:,:,:),intent(inout) :: gfrac    ! ice fractional area [0,1]
    real(r8),dimension(:,:,:),intent(inout) :: gthck    ! ice thickness (m)
    real(r8),dimension(:,:,:),intent(inout) :: gtopo    ! surface elevation (m)
    real(r8),dimension(:,:,:),intent(inout) :: ghflx    ! heat flux (W/m^2, positive down)
    real(r8),dimension(:,:),  intent(inout) :: groff    ! runoff (kg/m^2/s = mm H2O/s)

    logical, optional, intent(out) :: l_ice_tstep       ! Set when an ice timestep has been done, and
                                                        ! output fields have been updated
 
    ! Internal variables -----------------------------------------------------
 
    integer :: i, n
 
    character(250) :: message
 
    integer ::    &
        nx, ny,     &! global grid dimensions
        nec          ! no. of elevation classes

    real(rk) :: timeyr   ! time in years 
 
!lipscomb - debug
    integer :: j, ii, jj
    real(rk) :: lat, lon 
 
    nx  = size(qice,1)
    ny  = size(qice,2)
    nec = size(qice,3)
 
    ! Check we're expecting a call now ---------------------------------------
 
    if (params%new_av) then
       if (time == params%next_av_start) then
          params%av_start_time = time
          params%new_av = .false.
       else
          write(message,*) 'Unexpected calling of GLINT at time ',time
          call write_log(message,GM_FATAL,__FILE__,__LINE__)
       end if
    else
       if (mod(time-params%av_start_time,params%time_step) /= 0) then
          write(message,*) 'Unexpected calling of GLINT at time ',time
          call write_log(message,GM_FATAL,__FILE__,__LINE__)
       end if
    end if
 
    ! Reset ice_tstep flag
 
    l_ice_tstep = .false.

    ! accumulate sums
!lipscomb - is it necessary to average topo? 

    tsfc_av(:,:,:) = tsfc_av(:,:,:) + tsfc(:,:,:) 
    qice_av(:,:,:) = qice_av(:,:,:) + qice(:,:,:) 
    topo_av(:,:,:) = topo_av(:,:,:) + topo(:,:,:)
    
    ! increment step counter
 
    params%av_steps = params%av_steps + 1
 
    ! ---------------------------------------------------------
    ! If this is a mass balance timestep, prepare global fields, 
    ! and do a timestep for each model instance.
    ! ---------------------------------------------------------
 
    if (time - params%av_start_time + params%time_step > params%tstep_mbal) then
 
       write(message,*) &
            'Incomplete forcing of GLINT mass-balance time-step detected at time ', time
       call write_log(message,GM_FATAL,__FILE__,__LINE__)
 
    else if (time - params%av_start_time + params%time_step == params%tstep_mbal) then
 
       ! Calculate averages by dividing by number of steps elapsed
       ! since last model timestep.
 
        tsfc_av(:,:,:) = tsfc_av(:,:,:) / real(params%av_steps,r8)
        qice_av(:,:,:) = qice_av(:,:,:) / real(params%av_steps,r8)
        topo_av(:,:,:) = topo_av(:,:,:) / real(params%av_steps,r8)
 
       ! Calculate total ice accumulated or ablated - multiply by time since last model timestep
!lipscomb - This assumes time is in hours
       qice_av(:,:,:) = qice_av(:,:,:) * params%tstep_mbal * hours2seconds
  
!lipscomb - diagnostic print
       print*, 'Take an ice timestep, time (yr) =', time/(24.0_r8*365._r8) 
       call shr_sys_flush(6)
 
       ! Do a timestep for each instance
 
       do i=1,params%ninstances

          call glc_glint_ice_tstep(time,         params%instances(i),  &
                                   tsfc_av,      qice_av,              &
                                   topo_av,                            &
                                   gfrac_temp,   gthck_temp,           &
                                   gtopo_temp,   ghflx_temp,           &
                                   groff_temp,   l_ice_tstep)
                                  
          ! Add this contribution to the global output

          do n = 1, nec

             gfrac(:,:,n) = splice_field(gfrac(:,:,n),                      &
                                         gfrac_temp(:,:,n),                 &
                                         params%instances(i)%frac_coverage, &
                                         params%cov_normalise)

             gthck(:,:,n) = splice_field(gthck(:,:,n),               &
                                         gthck_temp(:,:,n),                 &
                                         params%instances(i)%frac_coverage, &
                                         params%cov_normalise)

             gtopo(:,:,n) = splice_field(gtopo(:,:,n),               &
                                         gtopo_temp(:,:,n),                 &
                                         params%instances(i)%frac_coverage, &
                                         params%cov_normalise)

             ghflx(:,:,n) = splice_field(ghflx(:,:,n),               &
                                         ghflx_temp(:,:,n),                 &
                                         params%instances(i)%frac_coverage, &
                                         params%cov_normalise)
 
          enddo   ! nec

!lipscomb - add runoff?
 
!          if (present(water_out)) water_out = splice_field(water_out, &
!               wout_temp, params%instances(i)%frac_coverage, params%cov_normalise)
 
          ! Add total water variables to running totals
 
!          if (present(total_water_in))  total_water_in  = total_water_in  + twin_temp
!          if (present(total_water_out)) total_water_out = total_water_out + twout_temp
!          if (present(ice_volume))      ice_volume      = ice_volume      + icevol_temp
 
!lipscomb - ice sheet diagnostics 
          if (mod(params%instances(i)%model%numerics%timecounter,  &
                  params%instances(i)%model%numerics%ndiag) == 0)  then
             timeyr = real(time,r8)/8760.0_r8 
             print*, 'Write diagnostics, time (yr)=', timeyr 
             call glide_write_diag(params%instances(i)%model, timeyr, itest, jtest)
          endif 
 
       enddo   ! instances
 
       ! Scale output water fluxes to be in mm/s
 
!       if (present(water_out)) water_out=water_out/ &
!            (params%tstep_mbal*hours2seconds)
 
       ! ---------------------------------------------------------
       ! Reset averaging fields, flags and counters
       ! ---------------------------------------------------------
 
       params%av_steps      = 0
       params%new_av        = .true.
       params%next_av_start = time + params%time_step
 
       tsfc_av(:,:,:) = c0
       qice_av(:,:,:) = c0
       topo_av(:,:,:) = c0

    endif    ! tstep_mbal
 
   end subroutine glc_glint_driver

!================================================================================

  subroutine glc_glint_ice_tstep(time,      instance,     &
                                 tsfc_g,    qice_g,       &
                                 topo_g,                  &
                                 gfrac,     gthck,        &
                                 gtopo,     ghflx,        &
                                 groff,     l_ice_tstep)
 
    ! Performs time-step of an ice model instance, given the surface mass balance
    !  as computed externally in multiple elevation classes.
    ! Based on GLIMMER subroutine glint_i_tstep.
    ! 
    ! Input quantities here are accumulated/average totals since the last call.

!lipscomb - are all of these needed?
    use glide
    use glide_setup
    use glide_io
    use glimmer_paramets
    use glint_io
    use glint_mbal_io
    use glimmer_routing
    use glimmer_log
    use glimmer_physcon, only: rhow, rhoi

    use glint_timestep, only: glint_remove_bath

    implicit none
 
    ! ------------------------------------------------------------------------  
    ! Arguments
    ! ------------------------------------------------------------------------  
 
    integer,                intent(in)   :: time          ! Current time in hours
    type(glint_instance), intent(inout)  :: instance      ! Model instance
    real(r8),dimension(:,:,:),intent(in) :: tsfc_g        ! Surface temperature (C)
    real(r8),dimension(:,:,:),intent(in) :: qice_g        ! Depth of new ice (kg/m^2)
    real(r8),dimension(:,:,:),intent(in) :: topo_g        ! Surface elevation (m)

    real(r8),dimension(:,:,:),intent(out) :: gfrac        ! ice fractional area [0,1]
    real(r8),dimension(:,:,:),intent(out) :: gthck        ! ice thickness (m)
    real(r8),dimension(:,:,:),intent(out) :: gtopo        ! surface elevation (m)
    real(r8),dimension(:,:,:),intent(out) :: ghflx        ! heat flux (W/m^2, positive down)
    real(r8),dimension(:,:),  intent(out) :: groff        ! runoff (kg/m^2/s = mm H2O/s)

    logical, optional,       intent(out)  :: l_ice_tstep  ! true if we have done an ice time step
 
    ! ------------------------------------------------------------------------  
    ! Internal variables
    ! ------------------------------------------------------------------------  

!lipscomb - keep all of these?
    real(r8),dimension(:,:),pointer :: upscale_temp => null() ! temporary array for upscaling
    real(r8),dimension(:,:),pointer :: routing_temp => null() ! temporary array for flow routing
    real(r8),dimension(:,:),pointer :: accum_temp   => null() ! temporary array for accumulation
    real(r8),dimension(:,:),pointer :: ablat_temp   => null() ! temporary array for ablation
    integer, dimension(:,:),pointer :: fudge_mask   => null() ! temporary array for fudging
!lipscomb - why sp?
    real(sp),dimension(:,:),pointer :: thck_temp    => null() ! temporary array for volume calcs
    real(sp),dimension(:,:),pointer :: calve_temp   => null() ! temporary array for calving flux

    real(r8),dimension(:,:),pointer :: ltopo_temp   => null() ! temp array for local topography

    real(r8) :: start_volume, end_volume, flux_fudge, ice_vol
    real(r8) :: t_win, t_wout
    real(r8),dimension(:,:),allocatable ::  &
       g_water_in    ,&! input water flux (mm)
       g_water_out     ! output water flux (mm)

    integer :: i
    integer :: nec   ! no. of elevation classes
  
!lipscomb - debug
    integer :: j, ii, jj, nx, ny, itest, jtest 
    real(r8) :: lat, lon 
 
    ! Check whether we're doing anything this time.
 
    if (time /= instance%next_time) then
       return
    else
       instance%next_time = instance%next_time + instance%mbal_tstep
    end if
 
    nec = size(qice_g, 3)
    
    ! Zero outputs
 
    gfrac(:,:,:) = c0
    gthck(:,:,:) = c0
    gtopo(:,:,:) = c0
    ghflx(:,:,:) = c0
    groff(:,:)   = c0

   ! Assume we always need this, as it's too complicated to work out when we do and don't
 
    call coordsystem_allocate(instance%lgrid,thck_temp)
    call coordsystem_allocate(instance%lgrid,calve_temp)

    l_ice_tstep = .false.
 
    ! Downscale input fields on global glc grid
!lipscomb - new subroutine; downscale mass balance to local grid 
!lipscomb - This subroutine computes instance%acab and instance%artm.

    call glc_glint_downscaling (instance,      nec,      &
                                tsfc_g,        qice_g,   &
                                topo_g)
 
    ! ------------------------------------------------------------------------  
    ! Sort out some local orography and remove bathymetry. This relies on the 
    ! point 1,1 being underwater. However, it's a better method than just 
    ! setting all points < 0.0 to zero
    ! ------------------------------------------------------------------------  
 
    call glide_get_usurf (instance%model, instance%local_orog)

!lipscomb - Is this needed?
    call glint_remove_bath(instance%local_orog,1,1)

!lipscomb - deleted lapse rate adjustment and precip processing
 
    ! Get ice thickness 
    ! Note: thck_temp is single-precision

    call glide_get_thk (instance%model, thck_temp)
 
    ! Do accumulation --------------------------------------------------------

!lipscomb - deleted call to glint_accumulate (we have acab already)
!           Just make sure mbal_accum (type glint_mbc) is set correctly
!           Includes prcp, ablt, acab, artm
 
    ! Initialise water budget quantities to zero. These will be over-ridden if
    ! there is an ice-model time-step
 
!lipscomb - compute these?
    t_win   = c0
    t_wout  = c0

!lipscomb - g_water_in and out are subroutine arguments in GLINT
    allocate(g_water_in(nx,ny))
    allocate(g_water_out(nx,ny))
    g_water_in (:,:) = c0
    g_water_out(:,:) = c0
 
    ! ------------------------------------------------------------------------  
    ! ICE TIMESTEP begins HERE ***********************************************
    ! ------------------------------------------------------------------------  
 
    if (time - instance%mbal_accum%start_time + instance%mbal_tstep   &
            == instance%mbal_accum_time) then
 
       if (instance%mbal_accum_time < instance%ice_tstep) then 
          instance%next_time = instance%next_time + instance%ice_tstep - instance%mbal_tstep
       end if
 
       l_ice_tstep = .true.
 
       ! Prepare arrays for water budgeting
 
!lipscomb - will not include ablat in water budget
       call coordsystem_allocate(instance%lgrid, accum_temp)
       call coordsystem_allocate(instance%lgrid, ablat_temp)
       accum_temp = c0
       ablat_temp = c0
 
       ! Calculate the initial ice volume (scaled and converted to water equiv)

       call glide_get_thk(instance%model, thck_temp)
       thck_temp = thck_temp * rhoi/rhow
       start_volume = sum(thck_temp)
 
       ! ---------------------------------------------------------------------
       ! do the different parts of the glint timestep
       ! ---------------------------------------------------------------------
 
       do i = 1, instance%n_icetstep
 
          ! Calculate the initial ice volume (scaled and converted to water equiv)
          call glide_get_thk (instance%model, thck_temp)
          thck_temp = thck_temp*real(rhoi/rhow)
 
          ! Get latest upper-surface elevation (needed for masking)
          call glide_get_usurf(instance%model,  &
                               instance%local_orog)
          call glint_remove_bath(instance%local_orog,1,1)
 
!lipscomb - New get_mbal subroutine
!lipscomb - This subroutine simply writes instance%mbal_accum%artm into instance%atrm, etc.
!lipscomb - Note: instance%acab has units of m/yr

          ! Get the mass-balance, as m water/year 
          call glc_glint_get_mbal(instance%mbal_accum,   &
                                  instance%artm,     instance%acab,         &
                                  instance%prcp,     instance%ablt,         &
                                  instance%snowd,    instance%siced,        &
                                  instance%mbal_accum_time)
 
          ! Mask out non-accumulation in ice-free areas
 
          where(thck_temp<=0.0 .and. instance%acab<0.0)
             instance%acab = 0.0
!             instance%ablt = instance%prcp
          end where
 
          ! Put climate inputs in the appropriate places, with conversion of units
          call glide_set_acab(instance%model,   &
                              instance%acab*real(rhow/rhoi))
          call glide_set_artm(instance%model,   &
                              instance%artm)
 
          ! Adjust glint acab and ablt for output 
          where (instance%acab < -thck_temp .and. thck_temp > 0.0)
             instance%acab = -thck_temp
!             instance%ablt =  thck_temp
          end where
 
          instance%glide_time = instance%glide_time + instance%model%numerics%tinc
          call glide_tstep_p1(instance%model, instance%glide_time)
          call glide_tstep_p2(instance%model)
          call glide_tstep_p3(instance%model)
 
          ! Add the calved ice to the ablation field

          call glide_get_calving(instance%model, calve_temp)
          calve_temp = calve_temp * real(rhoi/rhow)
 
!lipscomb - Calving needs to be added to runoff (groff)
!lipscomb - Also add basal melting, bmlt?
          instance%ablt = instance%ablt + calve_temp/instance%model%numerics%tinc
          instance%acab = instance%acab - calve_temp/instance%model%numerics%tinc
 
          ! Accumulate for water-budgeting
!lipscomb - what to do here?
!           for now, put all of acab (i.e., qice) in accum, with calving in ablat
!           Note: This is used to compute fudge factor below.

!          accum_temp = accum_temp + instance%prcp * instance%model%numerics%tinc
          accum_temp = accum_temp + instance%acab * instance%model%numerics%tinc
          ablat_temp = ablat_temp + instance%ablt * instance%model%numerics%tinc

          ! Finally, do some output
 
          call glint_io_writeall(instance,instance%model)
          call glint_mbal_io_writeall(instance%mbal_accum, instance%model)
 
       end do   ! n_icetstep
 
!lipscomb - not sure whether fudge factor is needed
       ! Calculate flux fudge factor --------------------------------------------
 
       call coordsystem_allocate(instance%lgrid,fudge_mask)
 
       call glide_get_thk(instance%model,thck_temp)
       end_volume = sum(thck_temp)
 
       where (thck_temp > c0)
          fudge_mask = 1
       elsewhere
          fudge_mask = 0
       endwhere
 
       flux_fudge = (start_volume + sum(accum_temp) - sum(ablat_temp) - end_volume)  &
                   / sum(fudge_mask)
 
       ! Apply fudge_factor
 
       where (thck_temp > c0)
          ablat_temp = ablat_temp + flux_fudge
       endwhere
          
       deallocate(fudge_mask)
       fudge_mask => null()
 
       ! Upscale water flux fields ----------------------------------------------
!lipscomb - Do we need to scale water input?
       ! First water input (i.e. mass balance + ablation)
 
       call coordsystem_allocate(instance%lgrid, upscale_temp)
 
       where (thck_temp > c0)
          upscale_temp = accum_temp
       elsewhere
          upscale_temp = c0
       endwhere
 
       call mean_to_global(instance%ups,   &
                           upscale_temp,   &
                           g_water_in,     &
                           instance%out_mask)

       deallocate(upscale_temp)
       upscale_temp => null()
 
       ! Now water output (i.e. ablation) - and do routing
 
       call coordsystem_allocate(instance%lgrid,upscale_temp)
       call coordsystem_allocate(instance%lgrid,routing_temp)
 
       where (thck_temp > c0)
          upscale_temp = ablat_temp
       elsewhere
          upscale_temp = c0
       endwhere
 
       call glide_get_usurf(instance%model,instance%local_orog)

       call flow_router(instance%local_orog, &
                        upscale_temp, &
                        routing_temp, &
                        instance%out_mask, &
                        instance%lgrid%delta%pt(1), &
                        instance%lgrid%delta%pt(2))
 
       call mean_to_global(instance%ups,   &
                           routing_temp,   &
                           g_water_out,    &
                           instance%out_mask)

       deallocate(upscale_temp,routing_temp)
       upscale_temp => null()
       routing_temp => null()
 
       ! Sum water fluxes and convert if necessary ------------------------------
 
       t_win  = sum(accum_temp)*instance%lgrid%delta%pt(1)* &
                                instance%lgrid%delta%pt(2)

 
       t_wout = sum(ablat_temp)*instance%lgrid%delta%pt(1)* &
                                instance%lgrid%delta%pt(2)
 
    end if   ! do timestep
 
    ! ------------------------------------------------------------------------ 
    ! Upscale output from local domain to global domain with elevation classes
    ! ------------------------------------------------------------------------ 
 

    ! Calculate ice volume ---------------------------------------------------
 
    call glide_get_thk(instance%model, thck_temp)
    ice_vol = sum(thck_temp)*instance%lgrid%delta%pt(1)* &
                             instance%lgrid%delta%pt(2)
 
    ! Tidy up ----------------------------------------------------------------
 
    if (associated(accum_temp)) then 
       deallocate(accum_temp)
       accum_temp => null()
    end if
 
    if (associated(ablat_temp)) then
       deallocate(ablat_temp)
       ablat_temp => null()
    end if
 
    if (associated(calve_temp)) then
       deallocate(calve_temp)
       calve_temp => null()
    end if
 
    deallocate(thck_temp)
    thck_temp => null()
 
  end subroutine glc_glint_ice_tstep

!================================================================================

  subroutine glc_glint_downscaling (instance,   nec,     &
                                    tsfc_g,     qice_g,  &
                                    topo_g)
 
    use glint_interp
 
    !*FD Downscale relevant fields
 
    type(glint_instance) :: instance
    integer(i4), intent(in) :: nec                       ! number of elevation classes
    real(r8),dimension(:,:,:),intent(in) :: tsfc_g       ! Surface temperature (C)
    real(r8),dimension(:,:,:),intent(in) :: qice_g       ! Depth of new ice (kg/m^2)
    real(r8),dimension(:,:,:),intent(in) :: topo_g       ! Surface elevation (m)

!lipscomb - is this needed?
!!!    logical, optional,       intent(in)  :: orogflag     ! Set if we have new global orog


    integer(i4) ::    &
       i, j, n,       &
       nxl, nyl
  
!lipscomb - Might want to find a less memory-intensive way to do this

    real(r8), dimension(:,:,:), allocatable ::   &
       tsfc_l,    &! interpolation of global sfc temperature to local grid
       qice_l,    &! interpolation of global mass balance to local grid
       topo_l    ! interpolation of global topography in each elev class to local grid

    real(r8) :: fact

    nxl = instance%lgrid%size%pt(1)
    nyl = instance%lgrid%size%pt(2)

    allocate(tsfc_l(nxl,nyl,nec))
    allocate(qice_l(nxl,nyl,nec))
    allocate(topo_l(nxl,nyl,nec))

!   Downscale global fields for each elevation class to local grid

    do n = 1, nec
       call interp_to_local(instance%lgrid, topo_g(:,:,n), instance%downs, localdp=topo_l(:,:,n))
       call interp_to_local(instance%lgrid, tsfc_g(:,:,n), instance%downs, localdp=tsfc_l(:,:,n))
       call interp_to_local(instance%lgrid, qice_g(:,:,n), instance%downs, localdp=qice_l(:,:,n))
    enddo

!   Interpolate tsfc and qice to local topography using values in the neighboring 
!   elevation classes.

    do n = 1, nec

   !lipscomb - lapse rate adjustment for tsfc?

       do j = 1, nxl
       do i = 1, nyl
          usrf = instance%model%geometry%usrf(i,j)
          if (n==1 .and. usrf < topo_l(i,j,n)) then   ! use values for n = 1
             instance%acab(i,j) = qice_l(i,j,n)
             instance%artm(i,j) = tsfc_l(i,j,n)
          elseif (n==nec .and. usrf >= topo_l(i,j,n)) then    ! use values for n = nec
             instance%acab(i,j) = qice_l(i,j,n)
             instance%artm(i,j) = tsfc_l(i,j,n)   !lipscomb - lapse rate adjustment?
          else   ! linear interpolation between neighboring classes
             fact = (topo_l(i,j,n) - usrf) / (topo_l(i,j,n) - topo_l(i,j,n-1))
             instance%acab(i,j) = fact*qice_l(i,j,n-1) + (c1-fact)*qice_l(i,j,n)
             instance%artm(i,j) = fact*tsfc_l(i,j,n-1) + (c1-fact)*tsfc_l(i,j,n)
          endif
       enddo
       enddo

    enddo    ! nec

!!!    if (orogflag) &
!!!       call interp_to_local(instance%lgrid, topo_g, instance%downs, localdp=instance%global_orog)
 
  end subroutine glc_glint_downscaling

!================================================================================

  subroutine glc_glint_get_mbal(params,           &
                                artm,    acab,    &
                                prcp,    ablt,    &
                                snowd,   siced,   &
                                dt)
 
!lipscomb - GLINT routine computes prcp, ablt, snowd and siced.
!           Here I just set these to 0.0.

    use glint_constants, only: hours2years
 
    type(glint_mbc)  :: params
    real(sp),dimension(:,:),intent(out)   :: artm   !*FD Mean air temperature (degC)
    real(sp),dimension(:,:),intent(out)   :: acab   !*FD Mass-balance
    real(sp),dimension(:,:),intent(out)   :: prcp   !*FD Precipitation (m)
    real(sp),dimension(:,:),intent(out)   :: ablt   !*FD Ablation
    real(sp),dimension(:,:),intent(inout) :: snowd  !*FD Snow depth (m)
    real(sp),dimension(:,:),intent(inout) :: siced  !*FD Superimposed ice depth (m)
    integer,                intent(in)    :: dt     !*FD accumulation time in hours
 
!lipscomb - Save artm_save?
    if (.not.params%new_accum) then
       params%artm_save = params%artm_save/real(params%av_count)
    end if
 
    params%new_accum=.true.
 
    artm=params%artm_save
    acab=params%acab_save / real(dt*hours2years,sp)

!lipscomb - just set the following to zero, since they are computed in CLM, not GLINT
!    prcp=params%prcp_save / real(dt*hours2years,sp)
!    ablt=params%ablt_save / real(dt*hours2years,sp)
!    snowd=params%snowd
!    siced=params%siced
 
!    where (snowd<0.0) snowd=0.0
!    where (siced<0.0) siced=0.0

     prcp  = 0.0
     ablt  = 0.0
     snowd = 0.0
     siced = 0.0 

  end subroutine glc_glint_get_mbal
  
!================================================================================

  subroutine get_glc_upscaled_fields(instance,              &
                                     gfrac,       gthck,    &
                                     gtopo,       ghflx)

    ! Upscales and returns fields required by glc for passing to coupler
    !
    ! This subroutine replaces the original get_i_upscaled_fields in glint_type.F90.
 
    use glimmer_paramets
 
    ! Arguments -------------------------------------------------------------------------------------
 
    type(glint_instance),     intent(in)  :: instance      ! the model instance
    real(r8),dimension(:,:,:),intent(out) :: gfrac         ! ice-covered fraction [0,1]
    real(r8),dimension(:,:,:),intent(out) :: gthck         ! ice thickness (m)
    real(r8),dimension(:,:,:),intent(out) :: gtopo         ! surface elevation (m)
    real(r8),dimension(:,:,:),intent(out) :: ghflx         ! heat flux (m)
 
    ! Internal variables ----------------------------------------------------------------------------
 
    real(r8),dimension(:,:),pointer :: temp => null()
    real(r8),dimension(:,:),pointer :: ltopo_temp => null()
 
    call coordsystem_allocate(instance%lgrid,temp)
    call coordsystem_allocate(instance%lgrid,ltopo_temp)

    ltopo_temp(:,:) = thk0 * instance%model%geometry%usrf(:,:)

    ! ice fraction

    where (instance%model%geometry%thck > c0)
       temp = c1
    elsewhere
       temp = c0
    endwhere

    call mean_to_global_mec(instance%ups,                       &
                            nec,                hec_max,        &
                            temp,               gfrac,          &
                            ltopo_temp,         instance%out_mask)

    ! ice thickness

    temp(:,:) = thk0 * instance%model%geometry%thck(:,:)

    call mean_to_global_mec(instance%ups,                   &
                            nec,                 hec_max,   &
                            temp,                gthck,     &
                            ltopo_temp,          instance%out_mask)

    ! surface elevation

    call mean_to_global_mec(instance%ups,                   &
                            nec,                 hec_max,   &
                            ltopo_temp,          gtopo,     &
                            ltopo_temp,          instance%out_mask)

    ! heat flux

!lipscomb - fill in heat flux here
    temp(:,:) = c0

    call mean_to_global_mec(instance%ups,                   &
                            nec,                 hec_max,   &
                            temp,                ghflx,     &
                            ltopo_temp,          instance%out_mask)
 
!lipscomb - Get runoff here?


    deallocate(temp)
    temp => null()

    deallocate(ltopo_temp)
    ltopo_temp => null()
 
  end subroutine get_glc_upscaled_fields
 
!================================================================================

  subroutine mean_to_global_mec(ups,                &
                                nec,      hec_max,  &
                                local,    global,   &
                                ltopo,    mask)
 
    ! Upscale from the local domain to a global domain with multiple elevation classes
    ! by areal averaging.
    !
    ! This subroutine is adapted from subroutine mean_to_global in GLIMMER.
    ! The only difference is that local topography is upscaled to multiple
    !  elevation classes in each global grid cell.
    !
    ! Note: This method is not the inverse of the interp_to_local routine.
 
    ! Arguments
 
    type(upscale),            intent(in)    :: ups     ! upscaling indexing data
    integer(i4),              intent(in)    :: nec     ! number of elevation classes 
    real(r8),dimension(0:nec),intent(in)    :: hec_max ! max elevation in each class 
    real(r8),dimension(:,:),  intent(in)    :: local   ! data on local grid
    real(r8),dimension(:,:,:),intent(out)   :: global  ! data on global grid
    real(r8),dimension(:,:),  intent(in)    :: ltopo   ! surface elevation on local grid (m)
    integer(i4),dimension(:,:),intent(in),optional :: mask ! mask for upscaling
 
    ! Internal variables
 
    integer(i4) ::  &
       i, j, n,    &! indices
       nxl, nyl     ! local dimensions

    real(r8), dimension(:,:), allocatable ::  &
        tempmask      ! temporary mask

    integer(i4), dimension(:,:), allocatable ::  &
        gboxec        ! elevation class associated with local topography

!lipscomb - debug
    real(r8) :: lsum, gsum
 
    ! Beginning of code
 
    nxl = size(local,1)
    nyl = size(local,2)

!lipscomb - Rewrite to avoid allocating and deallocating each time?
    allocate(tempmask(nxl,nyl))
    allocate(gboxec(nxl,nyl))

    if (present(mask)) then
       tempmask(:,:) = mask(:,:)
    else
       tempmask(:,:) = 1
    endif
 
    ! Compute global elevation class for each local grid cell

    gboxec(:,:) = 0
    do n = 1, nec
       do j = 1, nyl
       do i = 1, nxl
          if (ltopo(i,j) >= hec_max(n-1) .and. ltopo(i,j) < hec_max(n)) then
             gboxec(i,j) = n
          endif
       enddo
       enddo
    enddo

    global(:,:,:) = c0

    do j = 1, nyl
    do i = 1, nxl
       nxg = ups%gboxx(i,j)
       nyg = ups%gboxy(i,j)
       n = gboxec(i,j)       
!lipscomb - bug check
       if (n==0) then
          print*, 'Bug, local topography out of bounds'
          print*, 'i, j, topo:', i, j, ltopo(i,j)
          call exit_glc(sigAbort, 'Local topography out of bounds')
       endif
       global(nxg,nyg,n) = global(nxg,nyg,n) + local(i,j)*tempmask(i,j)
    enddo
    enddo
 
    do n = 1, nec
       do j = 1, nyl
       do i = 1, nxl
          if (ups%gboxn(i,j) /= 0) then
             global(i,j,n) = global(i,j,n) / ups%gboxn(i,j)
          else
             global(i,j,n) = c0
          endif
       enddo
       enddo
    enddo

!lipscomb - debug - conservation check

    lsum = sum(local)
    gsum = sum(global)

    if (gsum > c1 .and. abs(gsum-lsum)/gsum > eps) then
       print*, 'mean_to_global, local and global sums disagree'
       print*, 'lsum, gsum =', lsum, gsum
       call exit_glc(sigAbort, 'Upscaling conservation error')
    endif
 
    deallocate(tempmask)
    deallocate(gboxec)

  end subroutine mean_to_global_mec
  
!================================================================================

  end module glc_glint

!================================================================================


