!=======================================================================
!BOP
!
! !MODULE: glc_glint - drivers for upscaling, downscaling, calling GLIMMER routines
!
   module glc_glint

! !DESCRIPTION:
!
! This module contains driver routines that call various GLIMMER subroutines.
! Several GLINT subroutines from the GLIMMER release have been modified so that the
! surface mass balance can be computed externally in CLM and passed to GLIMMER via GLC.
!
! Input fields from CLM (on the global grid, with multiple elevation classes per gridcell)
! are downscaled to the local ice sheet grid.  In turn, output fields are upscaled from
! the local grid to the global grid.
!
! GLINT code can be somewhat opaque.  At some point it would be nice to have a 
! cleaner interface with the ice sheet dynamics.
!
! !REVISION HISTORY:
!  Author: William Lipscomb, LANL
!
! !USES:
!
  use glc_constants
  use glc_global_fields
  use glc_exit_mod

!lipscomb - to do - Might be better to pass glint_params explicitly?
  use glint_main, only: glint_params, splice_field, check_mbts

  use glimmer_global
  use glint_type
  use glint_global_grid
  use glint_constants
  use glide_diagnostics
 
!EOP
!=======================================================================

!lipscomb - to do - Should these be pointers?
  real(r8), dimension(:,:,:), allocatable ::  &
     tsfc_av     ,&! averaging array for tsfc
     topo_av     ,&! averaging array for topo
     qice_av       ! averaging array for qice

  real(r8), dimension(:,:,:), allocatable ::   &
     gfrac_temp    ,&! gfrac for a single instance
     gthck_temp    ,&! gthck for a single instance
     gtopo_temp    ,&! gtopo for a single instance
     ghflx_temp    ,&! ghflx for a single instance
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
    ! This subroutine is based on subroutine initialise_glint.
    ! It has been adapted for use with the GLC code, assuming that the
    !  surface mass balance is computed externally for multiple elevation
    !  classes and passed to GLIMMER.
 
    use glimmer_config
    use glint_initialise
    use glimmer_log

!lipscomb - added these
    use glint_main, only: check_init_args, glint_readconfig, glint_allocate_arrays

    implicit none

    ! Subroutine argument declarations --------------------------------------------------------
 
    type(glint_params),        intent(inout) :: params      !*FD parameters to be set
    type(global_grid),         intent(in)    :: glint_grid  !*FD global grid
    integer,                   intent(in)    :: time_step   !*FD Timestep of calling model (hours)
    character(*),dimension(:), intent(in)    :: paramfile   !*FD array of configuration file names.
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

    integer(i4) :: nxg, nyg  ! global grid dimensions
    integer(i4) :: nxl, nyl  ! local grid dimensions

    real(r8) :: timeyr   ! time in years

    if (verbose) then
       write(stdout,*) 'Starting glc_glint_initialize'
       call flushm(stdout)
    endif

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
 
    if (verbose) then
       write(stdout,*) 'time_step (hr)  =', params%time_step
       write(stdout,*) 'start_time (hr) =', params%start_time
       write(stdout,*) 'Initialize global grid'
       call flushm(stdout)
    endif

    ! Initialise main global grid -----------------------------------------------------
 
    nxg = glint_grid%nx
    nyg = glint_grid%ny

    params%g_grid%nx = nxg
    params%g_grid%ny = nyg

    allocate(params%g_grid%lats(nyg))
    allocate(params%g_grid%lons(nxg))
    allocate(params%g_grid%lat_bound(nyg+1))
    allocate(params%g_grid%lon_bound(nxg+1))
    allocate(params%g_grid%box_areas(nxg,nyg))

    params%g_grid%lons       (:) = glint_grid%lons(:)
    params%g_grid%lats       (:) = glint_grid%lats(:)
    params%g_grid%lon_bound  (:) = glint_grid%lon_bound(:)
    params%g_grid%lat_bound  (:) = glint_grid%lat_bound(:)
    params%g_grid%box_areas(:,:) = glint_grid%box_areas(:,:)

    ! Initialise orography grid ------------------------------------
 
!lipscomb - Fine orography grid is not used; use elevation classes instead
!lipscomb - to do - Do we need to define params%g_grid_orog?

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

    ! Allocate and initialize arrays
 
    allocate(tsfc_av(nxg,nyg,glc_nec))
    allocate(qice_av(nxg,nyg,glc_nec))
    allocate(topo_av(nxg,nyg,glc_nec))

    tsfc_av(:,:,:) = c0
    qice_av(:,:,:) = c0
    topo_av(:,:,:) = c0

    call glint_allocate_arrays(params)

    ! ---------------------------------------------------------------
    ! Zero coverage maps and normalisation fields for main grid and
    ! orography grid
    ! ---------------------------------------------------------------
 
    params%total_coverage = c0
    params%total_cov_orog = c0 
    params%cov_normalise  = c0
    params%cov_norm_orog  = c0
 
    ! ---------------------------------------------------------------
    ! Determine how many instances there are, according to what
    ! configuration files we've been provided with
    ! ---------------------------------------------------------------
 
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
 
    if (verbose) then
       write(stdout,*) 'Number of instances =', params%ninstances
       write(stdout,*) 'Read config files and initialize each instance'
       call flushm(stdout)
    endif

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

       if (verbose) then
          write(stdout,*) 'Initialize instance =', i
          call flushm(stdout)
       endif 

! note - upscale and downscale parameters are computed here
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
 
       ! initial ice sheet diagnostics 

       timeyr = c0
       if (verbose) then
          write(stdout,*) 'Write diagnostics, time (yr)=', timeyr
          call glide_write_diag(params%instances(i)%model, timeyr, itest, jtest)
       endif

    end do  ! ninstances
 
    ! Check that all mass-balance time-steps are the same length and 
    ! assign that value to the top-level variable
 
    params%tstep_mbal = check_mbts(mbts)
    if (present(ice_dt)) then
       ice_dt = check_mbts(idts)
    end if
 
!lipscomb - debug
    if (verbose) then
       write(stdout,*) 'tstep_mbal =', params%tstep_mbal
       write(stdout,*) 'start_time =', params%start_time
       write(stdout,*) 'time_step =',  params%time_step
       write(stdout,*) 'ice_dt =', ice_dt
       call flushm(stdout)
    endif

    ! Check that time steps divide into one another appropriately.
 
    if (.not. (mod(params%tstep_mbal,params%time_step)==0)) then
       call write_log  &
          ('The mass-balance timestep must be an integer multiple of the forcing time-step', &
            GM_FATAL,__FILE__,__LINE__)
       call flushm(stdout)
    end if
 
    ! Check we don't have coverage greater than one at any point.
 
    where (params%total_coverage > c1) params%total_coverage = c1
    where (params%total_cov_orog > c1) params%total_cov_orog = c1
    params%coverage_calculated = .true.
 
    ! Zero optional outputs, if present
 
!lipscomb - to do - Not sure if orog field is needed
    if (present(orog)) orog = c0

!lipscomb - to do - Set any other fields to zero?
 
    ! Allocate arrays
 
    allocate(orog_temp(nxg,nyg))

!lipscomb - added these fields
    allocate(gfrac_temp(nxg,nyg,glc_nec))
    allocate(gthck_temp(nxg,nyg,glc_nec))
    allocate(gtopo_temp(nxg,nyg,glc_nec))
    allocate(ghflx_temp(nxg,nyg,glc_nec))
    allocate(groff_temp(nxg,nyg,glc_nec))

    if (verbose) then
       write(stdout,*) 'Upscale initial ice sheet fields'
       call flushm(stdout)
    endif

    ! Get initial fields from instances, splice together and return
 
    do i = 1, params%ninstances

       nxl = params%instances(i)%lgrid%size%pt(1)
       nyl = params%instances(i)%lgrid%size%pt(2)

       call get_glc_upscaled_fields(params%instances(i), glc_nec,      &
                                    nxl,                 nyl,          &
                                    nxg,                 nyg,          &
                                    gfrac_temp,          gthck_temp,   &
                                    gtopo_temp,          ghflx_temp,   &
                                    groff_temp)

       do n = 1, glc_nec

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
 

          groff(:,:,n) = splice_field(groff(:,:,n),                      &
                                      groff_temp(:,:,n),                 &
                                      params%instances(i)%frac_coverage, &
                                      params%cov_normalise)

       enddo   ! glc_nec

    end do   ! ninstances
 
    ! Deallocate
 
    deallocate(gfrac_temp)
    deallocate(gthck_temp)
    deallocate(gtopo_temp)
    deallocate(ghflx_temp)
    deallocate(groff_temp)
 
    if (verbose) then
       write(stdout,*) 'Done in glc_glint_initialize'
       call flushm(stdout)
    endif

  end subroutine glc_glint_initialize
 
!================================================================================
 
  subroutine glc_glint_driver(params,         time,            &
                              tsfc,           qice,            &
                              topo,                            &
                              gfrac,          gthck,           &
                              gtopo,          ghflx,           &
                              groff,          l_ice_tstep)
 
    ! This subroutine is based on subroutine glint.
    ! The code has been modified to allow for computing the surface mass balance externally
    ! (e.g., in the land component of CCSM).
    !           
    ! This subroutine does spatial and temporal averaging of input fields
    !  and calls the ice sheet dynamics model when required.
    ! Input fields are means over the period since the last call.
    !
 
    use glimmer_utils
    use glint_interp
    use glint_timestep
    use glimmer_log

    implicit none
 
    ! Subroutine argument declarations ----------------------------------------
 
    type(glint_params),       intent(inout) :: params   ! parameters for this run
    integer,                  intent(in)    :: time     ! Current model time (hours)
    real(r8),dimension(:,:,:),intent(in)    :: tsfc     ! surface ground temperature (C)
    real(r8),dimension(:,:,:),intent(in)    :: qice     ! flux of glacier ice (kg/m^2/s)
    real(r8),dimension(:,:,:),intent(in)    :: topo     ! surface elevation (m)
 
    real(r8),dimension(:,:,:),intent(inout) :: gfrac    ! ice fractional area [0,1]
    real(r8),dimension(:,:,:),intent(inout) :: gthck    ! ice thickness (m)
    real(r8),dimension(:,:,:),intent(inout) :: gtopo    ! surface elevation (m)
    real(r8),dimension(:,:,:),intent(inout) :: ghflx    ! heat flux (W/m^2, positive down)
    real(r8),dimension(:,:,:),intent(inout) :: groff    ! runoff (kg/m^2/s = mm H2O/s)

    logical, optional, intent(out) :: l_ice_tstep    ! Set when an ice timestep has been done, and
                                                     ! output fields have been updated
 
    ! Internal variables -----------------------------------------------------
 
    integer :: i, n
 
    character(250) :: message
 
    integer ::    &
        nxl, nyl,     &! local grid dimensions
        nxg, nyg,     &! global grid dimensions
        nec            ! no. of elevation classes

    real(rk) :: timeyr   ! time in years 
 
    integer :: j, ii, jj, ig, jg
    real(rk) :: lat, lon 

    nxg = size(qice,1)
    nyg = size(qice,2)
    nec = size(qice,3)
  
    if (verbose) then
       write (stdout,*) 'In glc_glint_driver, current time (hr) =', time
       write (stdout,*) 'av_start_time =', params%av_start_time
       write (stdout,*) 'next_av_start =', params%next_av_start
       write (stdout,*) 'new_av =', params%new_av
       write (stdout,*) 'tstep_mbal =', params%tstep_mbal
       call flushm(stdout)
    endif

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
       if (mod(time-params%av_start_time, params%time_step) /= 0) then
          write(message,*) 'Unexpected calling of GLINT at time ',time
          call write_log(message,GM_FATAL,__FILE__,__LINE__)
       end if
    end if
 
    ! Reset ice_tstep flag
 
    l_ice_tstep = .false.

    ! accumulate sums
!lipscomb - to do - probably not necessary to average topo 

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
 
        if (verbose) then
           write(stdout,*) 'Take an ice timestep, time (hr) =', time
           i = itest
           j = jjtest
           do n = 1, glc_nec
              write (stdout,*) ' '
              write (stdout,*) 'n =', n
              write (stdout,*) 'tsfc_av (Celsius) =', tsfc_av(i,j,n)
              write (stdout,*) 'topo_av (m) =', topo_av(i,j,n)
              write (stdout,*) 'qice_av (kg m-2 s-1) =', qice_av(i,j,n)
           enddo
        endif

       ! Calculate total ice accumulated or ablated - multiply by time since last model timestep
       ! Divide by 1000 to convert from mm to m
       ! Note that tstep_mbal is measured in hours

        qice_av(:,:,:) = qice_av(:,:,:) * params%tstep_mbal * hours2seconds / 1000._r8
  
       ! Take a timestep for each instance

       allocate(gfrac_temp(nxg,nyg,nec))
       allocate(gthck_temp(nxg,nyg,nec))
       allocate(gtopo_temp(nxg,nyg,nec))
       allocate(ghflx_temp(nxg,nyg,nec))
       allocate(groff_temp(nxg,nyg,nec))

       do i=1,params%ninstances

          call glc_glint_ice_tstep(time,         params%instances(i),  &
                                   tsfc_av,      qice_av,              &
                                   topo_av,                            &
                                   gfrac_temp,   gthck_temp,           &
                                   gtopo_temp,   ghflx_temp,           &
                                   groff_temp,   l_ice_tstep)
                                  
          if (verbose) then
             write(stdout,*) 'Finished ice timestep'
             write(stdout,*) 'Upscale fields to global grid'
          endif

          ! Upscale the output to elevation classes on the global grid
 
          nxl = params%instances(i)%lgrid%size%pt(1)
          nyl = params%instances(i)%lgrid%size%pt(2)

          call get_glc_upscaled_fields(params%instances(i), glc_nec,      &
                                       nxl,                 nyl,          &
                                       nxg,                 nyg,          &
                                       gfrac_temp,          gthck_temp,   &
                                       gtopo_temp,          ghflx_temp,   &
                                       groff_temp)

          if (verbose) then
             ig = itest
             jg = jjtest
             write(stdout,*) ' '
             write(stdout,*) 'After upscaling:'
             do n = 1, nec
               write(stdout,*) ' '
               write(stdout,*) 'n =', n
               write(stdout,*) 'gfrac(n) =', gfrac(ig,jg,n)
               write(stdout,*) 'gthck(n) =', gthck(ig,jg,n)
               write(stdout,*) 'gtopo(n) =', gtopo(ig,jg,n)
!!               write(stdout,*) 'ghflx(n) =', ghflx(ig,jg,n)
!!               write(stdout,*) 'groff(n) =', groff(ig,jg,n)
             enddo
          endif

          ! Add this contribution to the global output

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

             groff(:,:,n) = splice_field(groff(:,:,n),                      &
                                         groff_temp(:,:,n),                 &
                                         params%instances(i)%frac_coverage, &
                                         params%cov_normalise)

          enddo   ! nec

!lipscomb - to do - Not yet computing runoff
 
!          if (present(water_out)) water_out = splice_field(water_out, &
!               wout_temp, params%instances(i)%frac_coverage, params%cov_normalise)
 
          ! Add total water variables to running totals
 
!          if (present(total_water_in))  total_water_in  = total_water_in  + twin_temp
!          if (present(total_water_out)) total_water_out = total_water_out + twout_temp
!          if (present(ice_volume))      ice_volume      = ice_volume      + icevol_temp
 
          if (mod(params%instances(i)%model%numerics%timecounter,  &
                  params%instances(i)%model%numerics%ndiag) == 0)  then
             if (verbose) then
                timeyr = real(time,r8)/8760.0_r8 
                write(stdout,*) 'Write diagnostics, time (yr)=', timeyr 
                call glide_write_diag(params%instances(i)%model, timeyr, itest, jtest)
             endif
          endif 
 
       enddo   ! ninstances
 
       ! Deallocate
 
       deallocate(gfrac_temp)
       deallocate(gthck_temp)
       deallocate(gtopo_temp)
       deallocate(ghflx_temp)
       deallocate(groff_temp)

!lipscomb - to do - Not yet computing runoff

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

    if (verbose) then
       write (stdout,*) 'Done in glc_glint_driver'
       call flushm(stdout)
    endif

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
    ! 
    ! This subroutine is based on GLIMMER subroutine glint_i_tstep.  Code has been
    !  modified to do appropriate downscaling of input fields from coupler.
    ! 
    ! Input quantities here are accumulated/average totals since the last call.

!lipscomb - to do - Not sure all of these needed
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
    real(r8),dimension(:,:,:),intent(in) :: qice_g        ! Depth of new ice (m)
    real(r8),dimension(:,:,:),intent(in) :: topo_g        ! Surface elevation (m)

    real(r8),dimension(:,:,:),intent(out) :: gfrac        ! ice fractional area [0,1]
    real(r8),dimension(:,:,:),intent(out) :: gthck        ! ice thickness (m)
    real(r8),dimension(:,:,:),intent(out) :: gtopo        ! surface elevation (m)
    real(r8),dimension(:,:,:),intent(out) :: ghflx        ! heat flux (W/m^2, positive down)
    real(r8),dimension(:,:,:),intent(out) :: groff        ! runoff (kg/m^2/s = mm H2O/s)

    logical, optional,       intent(out)  :: l_ice_tstep  ! true if we have done an ice time step
 
    ! ------------------------------------------------------------------------  
    ! Internal variables
    ! ------------------------------------------------------------------------  

!lipscomb - to do - not sure all of these are needed
    real(r8),dimension(:,:),pointer :: upscale_temp => null() ! temporary array for upscaling
    real(r8),dimension(:,:),pointer :: routing_temp => null() ! temporary array for flow routing
    real(r8),dimension(:,:),pointer :: accum_temp   => null() ! temporary array for accumulation
    real(r8),dimension(:,:),pointer :: ablat_temp   => null() ! temporary array for ablation
    integer, dimension(:,:),pointer :: fudge_mask   => null() ! temporary array for fudging
!lipscomb - to do - why single precision?
    real(sp),dimension(:,:),pointer :: thck_temp    => null() ! temporary array for volume calcs
    real(sp),dimension(:,:),pointer :: calve_temp   => null() ! temporary array for calving flux

    real(r8),dimension(:,:),pointer :: ltopo_temp   => null() ! temp array for local topography

    real(r8) :: start_volume, end_volume, flux_fudge, ice_vol
    real(r8) :: t_win, t_wout

!lipscomb - Comment out for now
!!    real(r8),dimension(:,:),allocatable ::  &
!!       g_water_in    ,&! input water flux (mm)
!!       g_water_out     ! output water flux (mm)

    integer :: i
    integer :: nec   ! no. of elevation classes
  
    integer :: j, ii, jj, nx, ny, il, jl
    real(r8) :: lat, lon 
 
    ! Check whether we're doing anything this time.

    if (verbose) then
       write(stdout,*) ' '
       write(stdout,*) 'In glc_glint_ice_tstep, time =', time
       write(stdout,*) 'next_time =', instance%next_time
       call flushm(stdout)
    endif

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
    groff(:,:,:) = c0

   ! Assume we always need this, as it's too complicated to work out when we do and do not
 
    call coordsystem_allocate(instance%lgrid,thck_temp)
    call coordsystem_allocate(instance%lgrid,calve_temp)

    l_ice_tstep = .false.
 
    ! Downscale input fields on global glc grid
    ! This subroutine computes instance%acab and instance%artm, the key inputs to GLIDE.

!lipscomb - This is a new subroutine adapted to CLM input.

    if (verbose) then
       write(stdout,*) 'Downscale input fields to local grid'
       call flushm(stdout)
    endif

    call glc_glint_downscaling (instance,      nec,      &
                                tsfc_g,        qice_g,   &
                                topo_g)
 
    ! ------------------------------------------------------------------------  
    ! Sort out some local orography and remove bathymetry. This relies on the 
    ! point 1,1 being underwater. However, it's a better method than just 
    ! setting all points < 0.0 to zero
    ! ------------------------------------------------------------------------  
 
    call glide_get_usurf (instance%model, instance%local_orog)

!lipscomb - to do - Is this needed?
    call glint_remove_bath(instance%local_orog,1,1)

!lipscomb - Deleted lapse rate adjustment and precip processing from GLINT code
 
    ! Get ice thickness 
    ! Note: thck_temp is single-precision

    call glide_get_thk (instance%model, thck_temp)

    ! Do accumulation --------------------------------------------------------

!    call glint_accumulate(instance%mbal_accum,  time,               &
!                          instance%artm,        instance%arng,                &
!                          instance%prcp,                            &
!                          instance%snowd,       instance%siced,    &
!                          instance%xwind,       instance%ywind,    &
!                          instance%local_orog,real(thck_temp,rk),  &
!                          instance%humid,       instance%swdown,   &
!                          instance%lwdown,      instance%airpress)

!lipscomb - Commented out call to glint_accumulate, since we have acab already.
!           Still have to initialize mbal_accum (type glint_mbc); this is done
!            in glc_glint_get_mbal and in the following code.
!lipscomb - to do - Move this code elsewhere?

    if (instance%mbal_accum%new_accum) then

       instance%mbal_accum%new_accum=.false.
       instance%mbal_accum%av_count =0

!lipscomb - important to initialize start_time correctly
       instance%mbal_accum%start_time = time

       ! Initialise some quantities that will not be used

!!       instance%mbal_accum%snowd=snowd
!!       instance%mbal_accum%siced=siced
       instance%mbal_accum%snowd=c0
       instance%mbal_accum%siced=c0

       instance%mbal_accum%prcp_save=c0
       instance%mbal_accum%ablt_save=c0
       instance%mbal_accum%acab_save=c0
       instance%mbal_accum%artm_save=c0

    end if

    instance%mbal_accum%av_count = instance%mbal_accum%av_count+1

    ! Initialise water budget quantities to zero. These will be over-ridden if
    ! there is an ice-model time-step
 
!lipscomb - to do - compute these?
    t_win   = c0
    t_wout  = c0

!lipscomb - g_water_in and out are subroutine arguments in GLINT
!lipscomb - Comment out for now
!!    allocate(g_water_in(nx,ny))
!!    allocate(g_water_out(nx,ny))
!!    g_water_in (:,:) = c0
!!    g_water_out(:,:) = c0
 
!lipscomb - debug
    if (verbose) then
       write(stdout,*) ' '
       write(stdout,*) 'Check for ice dynamics timestep'
       write(stdout,*) 'time =', time
       write(stdout,*) 'start_time =', instance%mbal_accum%start_time
       write(stdout,*) 'mbal_step =', instance%mbal_tstep
       write(stdout,*) 'mbal_accum_time =', instance%mbal_accum_time
       call flushm(stdout)
    endif

    ! ------------------------------------------------------------------------  
    ! ICE TIMESTEP begins HERE ***********************************************
    ! ------------------------------------------------------------------------  
 
    if (time - instance%mbal_accum%start_time + instance%mbal_tstep   &
            == instance%mbal_accum_time) then

       if (verbose) then
          write(stdout,*) 'Taking an ice dynamics step'
          call flushm(stdout)
       endif
  
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

!lipscomb - to do - Is this needed?  See call to glide_get_thck above.
       call glide_get_thk(instance%model, thck_temp)
       thck_temp = thck_temp * rhoi/rhow
       start_volume = sum(thck_temp)
 
       ! ---------------------------------------------------------------------
       ! do the different parts of the glide timestep
       ! ---------------------------------------------------------------------
 
       do i = 1, instance%n_icetstep

!lipscomb - debug
          if (verbose) then
             write (stdout,*) 'GLIDE timestep, iteration i =', i
             call flushm(stdout)
          endif

          ! Calculate the initial ice volume (scaled and converted to water equiv)
          call glide_get_thk (instance%model, thck_temp)
          thck_temp = thck_temp*real(rhoi/rhow)
 
          ! Get latest upper-surface elevation (needed for masking)
          call glide_get_usurf(instance%model,  &
                               instance%local_orog)
          call glint_remove_bath(instance%local_orog,1,1)
 
!lipscomb - The following is a new subroutine for coupled runs.
!         - The new version simply writes instance%mbal_accum%artm into instance%atrm, etc.
!         - Note: instance%acab has units of m/yr

!lipscomb - Removed artm and acab from argument list, since these are received from coupler

!lipscomb - to do - Get rid of this subroutine?

          ! Get the mass-balance, as m water/year 
          call glc_glint_get_mbal(instance%mbal_accum,   &
!                                  instance%artm,     instance%acab,         &
                                  instance%prcp,     instance%ablt,         &
                                  instance%snowd,    instance%siced,        &
                                  instance%mbal_accum_time)
  
          ! Mask out non-accumulation in ice-free areas
 
          where(thck_temp<=0.0 .and. instance%acab<0.0)
             instance%acab = 0.0
!             instance%ablt = instance%prcp
          end where

!lipscomb - debug
          if (verbose) then 
             il = itest_local
             jl = jtest_local
             write (stdout,*) ' '
             write (stdout,*) 'GLIDE input for test point: i, j =', il, jl
             write (stdout,*) 'acab, artm =', instance%acab(il,jl), instance%artm(il,jl)
             call flushm(stdout)
          endif

          ! Put climate inputs in the appropriate places, with conversion of units

          ! Input acab is in m/yr
          ! This value is mulitplied by tim0/(scyr*thk0) and written to data%climate%acab
          call glide_set_acab(instance%model,   &
                              instance%acab*real(rhow/rhoi))

          ! Input artm is in deg C
          ! This value is written to data%climate%artm (no unit conversion necessary)
          call glide_set_artm(instance%model,   &
                              instance%artm)
 
          ! Adjust glint acab and ablt for output 
!lipscomb - Note that acab can have either sign, and ablt = 0 at this point
          where (instance%acab < -thck_temp .and. thck_temp > 0.0)
             instance%acab = -thck_temp
!             instance%ablt =  thck_temp
          end where
 
          if (verbose) then
             il = itest_local
             jl = jtest_local
             write (stdout,*) 'After conversion of units: acab, artm =', &
                   instance%model%climate%acab(il,jl), instance%model%climate%artm(il,jl)
             write (stdout,*) 'Initial thickness =', instance%model%geometry%thck(il,jl)
             write (stdout,*) 'call glide_tstep_p1'
             call flushm(stdout)
          endif

          instance%glide_time = instance%glide_time + instance%model%numerics%tinc
          call glide_tstep_p1(instance%model, instance%glide_time)

          if (verbose) then
              write (stdout,*) 'call glide_tstep_p2'
              call flushm(stdout)
          endif

          call glide_tstep_p2(instance%model)

          if (verbose) then
              write (stdout,*) 'New thickness =', instance%model%geometry%thck(il,jl)
              write (stdout,*) 'call glide_tstep_p3'
              call flushm(stdout)
          endif

          call glide_tstep_p3(instance%model)
 
          ! Add the calved ice to the ablation field

          call glide_get_calving(instance%model, calve_temp)
          calve_temp = calve_temp * real(rhoi/rhow)
 
!lipscomb - to do - Calving needs to be added to the runoff field (groff)
!                   Also add basal melting, bmlt?

          instance%ablt = instance%ablt + calve_temp/instance%model%numerics%tinc
          instance%acab = instance%acab - calve_temp/instance%model%numerics%tinc
 
          ! Accumulate for water-budgeting
!lipscomb - to do - Not sure what to do here
!           For now, put all of acab (i.e., qice) in accum, with calving in ablat
!           Note: This is used to compute a fudge factor below.

!          accum_temp = accum_temp + instance%prcp * instance%model%numerics%tinc
          accum_temp = accum_temp + instance%acab * instance%model%numerics%tinc
          ablat_temp = ablat_temp + instance%ablt * instance%model%numerics%tinc

          ! Finally, do some output
 
          call glint_io_writeall(instance,instance%model)
          call glint_mbal_io_writeall(instance%mbal_accum, instance%model)
 
       end do   ! n_icetstep
 
!lipscomb - to do - not sure whether the fudge factor is needed
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

!lipscomb - to do - Do we need to upscale the water input?

       ! First water input (i.e. mass balance + ablation)
 
       call coordsystem_allocate(instance%lgrid, upscale_temp)
 
       where (thck_temp > c0)
          upscale_temp = accum_temp
       elsewhere
          upscale_temp = c0
       endwhere
 
!lipscomb - Comment out for now
!       call mean_to_global(instance%ups,   &
!                           upscale_temp,   &
!                           g_water_in,     &
!                           instance%out_mask)

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

!lipscomb - Flow routing is not needed for coupled runs.
!           Initialize routing_temp and comment out this call.
       routing_temp(:,:) = c0

!lipscomb - Comment out for now
!       call flow_router(instance%local_orog, &
!                        upscale_temp, &
!                        routing_temp, &
!                        instance%out_mask, &
!                        instance%lgrid%delta%pt(1), &
!                        instance%lgrid%delta%pt(2))
 
!       call mean_to_global(instance%ups,   &
!                           routing_temp,   &
!                           g_water_out,    &
!                           instance%out_mask)

       deallocate(upscale_temp,routing_temp)
       upscale_temp => null()
       routing_temp => null()
 
       ! Sum water fluxes and convert if necessary ------------------------------

!lipscomb - Comment out for now 
!       t_win  = sum(accum_temp)*instance%lgrid%delta%pt(1)* &
!                                instance%lgrid%delta%pt(2)

 
!       t_wout = sum(ablat_temp)*instance%lgrid%delta%pt(1)* &
!                                instance%lgrid%delta%pt(2)
 
    end if   ! do timestep
 
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
    use glimmer_paramets, only: thk0
 
    ! Downscale fields from the global grid (with multiple elevation classes)
    !  to the local grid.

    type(glint_instance) :: instance
    integer(i4), intent(in) :: nec                       ! number of elevation classes
    real(r8),dimension(:,:,:),intent(in) :: tsfc_g       ! Surface temperature (C)
    real(r8),dimension(:,:,:),intent(in) :: qice_g       ! Depth of new ice (m)
    real(r8),dimension(:,:,:),intent(in) :: topo_g       ! Surface elevation (m)

!lipscomb - to do - Is this needed?
!!!    logical, optional,       intent(in)  :: orogflag     ! Set if we have new global orog

    integer(i4) ::    &
       i, j, n,       &
       nxl, nyl
  
!lipscomb - to do - Might want to find a less memory-intensive way to do downscaling

    real(r8), dimension(:,:,:), allocatable ::   &
       tsfc_l,    &! interpolation of global sfc temperature to local grid
       qice_l,    &! interpolation of global mass balance to local grid
       topo_l      ! interpolation of global topography in each elev class to local grid

    real(r8) :: fact

    if (verbose) then
       i = itest
       j = jjtest
       write (stdout,*) ' ' 
       write (stdout,*) 'In glc_glint_downscaling: i, j =', i, j
       do n = 1, glc_nec
          write (stdout,*) ' '
          write (stdout,*) 'n =', n
          write (stdout,*) 'tsfc_g =', tsfc_g(i,j,n)
          write (stdout,*) 'topo_g =', topo_g(i,j,n)
          write (stdout,*) 'qice_g =', qice_g(i,j,n)
       enddo
       call flushm(stdout)
    endif

    nxl = instance%lgrid%size%pt(1)
    nyl = instance%lgrid%size%pt(2)

    allocate(tsfc_l(nxl,nyl,nec))
    allocate(topo_l(nxl,nyl,nec))
    allocate(qice_l(nxl,nyl,nec))

    if (verbose) then
       write (stdout,*) 'Interp to local grid'
    endif

!   Downscale global fields for each elevation class to local grid

    do n = 1, nec
       call interp_to_local(instance%lgrid, tsfc_g(:,:,n), instance%downs, localdp=tsfc_l(:,:,n))
       call interp_to_local(instance%lgrid, topo_g(:,:,n), instance%downs, localdp=topo_l(:,:,n))
       call interp_to_local(instance%lgrid, qice_g(:,:,n), instance%downs, localdp=qice_l(:,:,n))
    enddo

!lipscomb - debug
    if (verbose) then
       i = itest_local
       j = jtest_local
       write (stdout,*) ' ' 
       write (stdout,*) 'Interpolated to local cells: i, j =', i, j
       do n = 1, glc_nec
          write (stdout,*) ' '
          write (stdout,*) 'n =', n
          write (stdout,*) 'tsfc_l =', tsfc_l(i,j,n)
          write (stdout,*) 'topo_l =', topo_l(i,j,n)
          write (stdout,*) 'qice_l =', qice_l(i,j,n)
       enddo
       call flushm(stdout)
    endif

!   Interpolate tsfc and qice to local topography using values in the neighboring 
!    elevation classes.
!   If the local topography is outside the bounds of the global elevations classes,
!    extrapolate the temperature using the prescribed lapse rate.

    do j = 1, nxl
    do i = 1, nyl

       usrf = instance%model%geometry%usrf(i,j) * thk0   ! actual sfc elevation (m)

       if (usrf < topo_l(i,j,1)) then
          instance%acab(i,j) = qice_l(i,j,1)
          instance%artm(i,j) = tsfc_l(i,j,1) + lapse*(topo_l(i,j,1)-usrf)
       elseif (usrf > topo_l(i,j,nec)) then
          instance%acab(i,j) = qice_l(i,j,nec)
          instance%artm(i,j) = tsfc_l(i,j,nec) - lapse*(usrf-topo_l(i,j,nec))
       else
          do n = 2, nec
             if (usrf > topo_l(i,j,n-1) .and. usrf < topo_l(i,j,n)) then
                fact = (topo_l(i,j,n) - usrf) / (topo_l(i,j,n) - topo_l(i,j,n-1)) 
                instance%acab(i,j) = fact*qice_l(i,j,n-1) + (c1-fact)*qice_l(i,j,n)
                instance%artm(i,j) = fact*tsfc_l(i,j,n-1) + (c1-fact)*tsfc_l(i,j,n)
                exit
             endif
          enddo
       endif   ! usrf

       if (verbose) then
          if (i==itest_local .and. j==jtest_local) then
             write (stdout,*) ' '
             write (stdout,*) 'Interpolated values, i, j =', i, j
             write (stdout,*) 'usrf =', usrf
             write (stdout,*) 'acab =', instance%acab(i,j)
             write (stdout,*) 'artm =', instance%artm(i,j)
          endif
       endif

    enddo  ! i
    enddo  ! j

!lipscomb - commented out
!!!    if (orogflag) &
!!!       call interp_to_local(instance%lgrid, topo_g, instance%downs, localdp=instance%global_orog)
 
  end subroutine glc_glint_downscaling

!================================================================================
!lipscomb - Removed artm and acab from argument list.  Use values from coupler instead.

  subroutine glc_glint_get_mbal(params,           &
!                                artm,    acab,    &
                                prcp,    ablt,    &
                                snowd,   siced,   &
                                dt)
 
!lipscomb - The corresponding GLINT routine computes prcp, ablt, snowd and siced.
!           Here I just set these to 0.0.

    use glint_constants, only: hours2years
 
    type(glint_mbc)  :: params
!    real(sp),dimension(:,:),intent(out)   :: artm   !*FD Mean air temperature (degC)
!    real(sp),dimension(:,:),intent(out)   :: acab   !*FD Mass-balance
    real(sp),dimension(:,:),intent(out)   :: prcp   !*FD Precipitation (m)
    real(sp),dimension(:,:),intent(out)   :: ablt   !*FD Ablation
    real(sp),dimension(:,:),intent(inout) :: snowd  !*FD Snow depth (m)
    real(sp),dimension(:,:),intent(inout) :: siced  !*FD Superimposed ice depth (m)
    integer,                intent(in)    :: dt     !*FD accumulation time in hours
 
!lipscomb - to do - Is this needed?
    if (.not.params%new_accum) then
       params%artm_save = params%artm_save/real(params%av_count)
    end if
 
    params%new_accum=.true.
 
!lipscomb - Commented out because we use values from CLM
!    artm=params%artm_save
!    acab=params%acab_save / real(dt*hours2years,sp)

!lipscomb - Just set the following to zero, since they are computed in CLM
!           and not needed by GLIMMER.
!    prcp=params%prcp_save / real(dt*hours2years,sp)
!    ablt=params%ablt_save / real(dt*hours2years,sp)
!    snowd=params%snowd
!    siced=params%siced
 
!    where (snowd<0.0) snowd=0.0
!    where (siced<0.0) siced=0.0

     ! single precision
     prcp  = 0.0
     ablt  = 0.0
     snowd = 0.0
     siced = 0.0 

  end subroutine glc_glint_get_mbal
  
!================================================================================

  subroutine get_glc_upscaled_fields(instance,    nec,      &
                                     nxl,         nyl,        &
                                     nxg,         nyg,        &
                                     gfrac,       gthck,    &
                                     gtopo,       ghflx,    &
                                     groff)

    ! Upscales and returns fields required by glc for passing to coupler
    !
    ! This subroutine replaces get_i_upscaled_fields in glint_type.F90.
 
    use glimmer_paramets

    ! Arguments ----------------------------------------------------------------------------
 
    type(glint_instance),     intent(in)  :: instance      ! the model instance
    integer,                  intent(in)  :: nec           ! number of elevation classes
    integer(i4),              intent(in)  :: nxl,nyl       ! local grid dimensions 
    integer(i4),              intent(in)  :: nxg,nyg       ! global grid dimensions 

    real(r8),dimension(nxg,nyg,nec),intent(out) :: gfrac   ! ice-covered fraction [0,1]
    real(r8),dimension(nxg,nyg,nec),intent(out) :: gthck   ! ice thickness (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: gtopo   ! surface elevation (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: ghflx   ! heat flux (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: groff   ! runoff/calving flux (kg/m^2/s)
 
    ! Internal variables ----------------------------------------------------------------------
 
    real(r8),dimension(nxl,nyl) :: temp
    real(r8),dimension(nxl,nyl) :: ltopo_temp

    integer :: i, j            ! indices
 
    integer :: il, jl, ig, jg

    ltopo_temp(:,:) = thk0 * instance%model%geometry%usrf(:,:)
    
    if (verbose) then
       ig = itest
       jg = jjtest
       il = itest_local
       jl = jtest_local
       write(stdout,*) 'In get_gcl_upscaled_fields'
       write(stdout,*) 'il, jl =', il, jl
       write(stdout,*) 'ig, jg =', ig, jg
       write(stdout,*) 'nxl, nyl =', nxl,nyl
       write(stdout,*) 'nxg, nyg =', nxg,nyg
       call flushm(stdout)
    endif

    ! ice fraction

    do j = 1, nyl
    do i = 1, nxl
       if (ltopo_temp(i,j) > c0) then
          temp(i,j) = c1
       else
          temp(i,j) = c0
       endif
    enddo
    enddo

    if (verbose) then
       write(stdout,*) 'local ifrac =', temp(il, jl)
       write(stdout,*) 'local topo =', ltopo_temp(il,jl)
       write(stdout,*) 'local mask =', instance%out_mask(il,jl)
    endif

    call mean_to_global_mec(instance%ups,                       &
                            nxl,                nyl,            &
                            nxg,                nyg,            &
                            nec,                hec_max,        &
                            temp,               gfrac,          &
                            ltopo_temp,         instance%out_mask)

    ! ice thickness

    temp(:,:) = thk0 * instance%model%geometry%thck(:,:)

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                nyl,        &
                            nxg,                nyg,        &
                            nec,                hec_max,    &
                            temp,               gthck,      &
                            ltopo_temp,         instance%out_mask)


!lipscomb - debug
    write(stdout,*) ' '
    write(stdout,*) 'local topo =', ltopo_temp(il,jl)

    ! surface elevation

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            ltopo_temp,          gtopo,     &
                            ltopo_temp,          instance%out_mask)


    ! heat flux

!lipscomb - to do - Copy runoff into temp array
    temp(:,:) = c0

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            temp,                ghflx,     &
                            ltopo_temp,          instance%out_mask)
 
!lipscomb - to do - Copy runoff into temp array
    temp(:,:) = c0

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            temp,                groff,     &
                            ltopo_temp,          instance%out_mask)

    if (verbose) then

       write(stdout,*) ' '
       write(stdout,*) 'global ifrac:'
       do n = 1, nec
          write(stdout,*) n, gfrac(ig, jg, n)
       enddo

       write(stdout,*) ' '
       write(stdout,*) 'global gtopo:'
       do n = 1, nec
          write(stdout,*) n, gtopo(ig, jg, n)
       enddo

       write(stdout,*) ' '
       write(stdout,*) 'global gthck:'
       do n = 1, nec
          write(stdout,*) n, gthck(ig, jg, n)
       enddo

!       write(stdout,*) ' '
!       write(stdout,*) 'global ghflx:'
!       do n = 1, nec
!          write(stdout,*) n, ghflx(ig, jg, n)
!       enddo

!       write(stdout,*) ' '
!       write(stdout,*) 'global groff:'
!       do n = 1, nec
!          write(stdout,*) n, groff(ig, jg, n)
!       enddo

    endif    ! verbose

  end subroutine get_glc_upscaled_fields
 
!================================================================================

  subroutine mean_to_global_mec(ups,                &
                                nxl,      nyl,      &
                                nxg,      nyg,      &
                                nec,      hec_max,  &
                                local,    global,   &
                                ltopo,    mask)
 
    ! Upscale from the local domain to a global domain with multiple elevation classes
    ! by areal averaging.
    !
    ! This subroutine is adapted from subroutine mean_to_global in GLIMMER.
    ! The difference is that local topography is upscaled to multiple elevation classes
    !  in each global grid cell.
    !
    ! Note: This method is not the inverse of the interp_to_local routine.
    ! Also note that each local grid cell is assumed to have the same area.
    ! It would be nice to have a more sophisticated routine.
 
    ! Arguments
 
    type(upscale),            intent(in)    :: ups     ! upscaling indexing data
    integer(i4),              intent(in)    :: nxl,nyl ! local grid dimensions 
    integer(i4),              intent(in)    :: nxg,nyg ! global grid dimensions 
    integer(i4),              intent(in)    :: nec     ! number of elevation classes 
    real(r8),dimension(0:nec),intent(in)    :: hec_max ! max elevation in each class 
    real(r8),dimension(nxl,nyl),  intent(in)      :: local   ! data on local grid
    real(r8),dimension(nxg,nyg,nec),intent(out)   :: global  ! data on global grid
    real(r8),dimension(nxl,nyl),  intent(in)      :: ltopo   ! surface elevation on local grid (m)
    integer(i4),dimension(nxl,nyl),intent(in),optional :: mask ! mask for upscaling
 
    ! Internal variables
 
    integer(i4) ::  &
       i, j, n,    &! indices
       ig, jg       ! indices

    integer(i4), dimension(nxl,nyl) ::  &
        tempmask,    &! temporary mask
        gboxec        ! elevation class associated with local topography

    integer(i4), dimension(nxg,nyg,nec) ::  &
        gnumloc       ! no. of local cells within each global cell in each elevation class

    integer :: il, jl
    real(r8) :: lsum, gsum
 
    if (present(mask)) then
       tempmask(:,:) = mask(:,:)
    else
       tempmask(:,:) = 1
    endif
 
    ! Compute global elevation class for each local grid cell
    ! Also compute number of local cells within each global cell in each elevation class

    gboxec(:,:) = 0
    gnumloc(:,:,:) = 0

    do n = 1, nec
       do j = 1, nyl
       do i = 1, nxl
          if (ltopo(i,j) >= hec_max(n-1) .and. ltopo(i,j) < hec_max(n)) then
             gboxec(i,j) = n
             if (tempmask(i,j)==1) then
                ig = ups%gboxx(i,j)
                jg = ups%gboxy(i,j)
                gnumloc(ig,jg,n) = gnumloc(ig,jg,n) + 1
             endif
          endif
       enddo
       enddo
    enddo

    global(:,:,:) = c0

    do j = 1, nyl
    do i = 1, nxl
       ig = ups%gboxx(i,j)
       jg = ups%gboxy(i,j)
       n = gboxec(i,j)
!lipscomb - bug check
       if (n==0) then
          write(stdout,*) 'Bug, local topography out of bounds'
          write(stdout,*) 'i, j, topo:', i, j, ltopo(i,j)
          write(stdout,*) 'hec_max(0) =', hec_max(0)
          call exit_glc(sigAbort, 'Local topography out of bounds')
       endif

!lipscomb - debug
       if (verbose .and. i==itest_local .and. j==jtest_local) then
          write(stdout,*) ' '
          write(stdout,*) 'il, jl =', i, j
          write(stdout,*) 'ig, jg, n =', ig, jg, n
          write(stdout,*) 'Old global val =', global(ig,jg,n)
          write(stdout,*) 'local, mask =', local(i,j), tempmask(i,j)
       endif

       global(ig,jg,n) = global(ig,jg,n) + local(i,j)*tempmask(i,j)

!lipscomb - debug
       if (verbose .and. i==itest_local .and. j==jtest_local) then
          write(stdout,*) 'New global val =', global(ig,jg,n)
       endif

    enddo
    enddo
 
    do n = 1, nec
       do j = 1, nyg
       do i = 1, nxg
          if (gnumloc(i,j,n) /= 0) then
             global(i,j,n) = global(i,j,n) / gnumloc(i,j,n)
          else
             global(i,j,n) = c0
          endif
       enddo
       enddo
    enddo

    ! conservation check

    lsum = c0
    do j = 1, nyl
    do i = 1, nxl
       lsum = lsum + local(i,j)*tempmask(i,j)
    enddo
    enddo

    gsum = c0
    do n = 1, nec
    do j = 1, nyg
    do i = 1, nxg
       gsum = gsum + global(i,j,n)*gnumloc(i,j,n)
    enddo
    enddo
    enddo

!lipscomb - to do - Use a less arbitrary error threshold
    if (abs(gsum-lsum) > 1.0) then 
       write(stdout,*) 'local and global sums disagree'
       write (stdout,*) 'lsum, gsum =', lsum, gsum 
       call exit_glc(sigAbort, 'Upscaling conservation error')
    endif

  end subroutine mean_to_global_mec
  
!================================================================================

  end module glc_glint

!================================================================================


