! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                           +
! +  glint_example_clim.f90 - part of the GLIMMER ice model   + 
! +                                                           +
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 
! Copyright (C) 2004 GLIMMER contributors - see COPYRIGHT file 
! for list of contributors.
!
! This program is free software; you can redistribute it and/or 
! modify it under the terms of the GNU General Public License as 
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful, 
! but WITHOUT ANY WARRANTY; without even the implied warranty of 
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License 
! along with this program; if not, write to the Free Software 
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 
! 02111-1307 USA
!
! GLIMMER is maintained by:
!
! Ian Rutt
! School of Geographical Sciences
! University of Bristol
! University Road
! Bristol
! BS8 1SS
! UK
!
! email: <i.c.rutt@bristol.ac.uk> or <ian.rutt@physics.org>
!
! GLIMMER is hosted on NeSCForge:
!
! http://forge.nesc.ac.uk/projects/glimmer/
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ifdef HAVE_CONFIG_H
#include <config.inc>
#endif

module glint_example_clim

  !*FD Module containing the glint example climate driver.

  use glimmer_global
  use glint_global_grid

  implicit none

  type glex_climate
     ! Mass-balance coupling timing parameters --------------------------
     integer                 :: total_years=10    ! Length of run in years
     integer                 :: climate_tstep=6   ! Climate time-step in hours
     real(rk)                :: diurnal_cycle=0.0 ! Imposed Diurnal cycle (degC)
     ! Filenames --------------------------------------------------------
     character(fname_length) :: precip_file = 'monthly_precip_mean_1974-2003.nc' !*FD Name of precip file
     character(fname_length) :: stemp_file  = 'surf_temp_6h_1974-2003.nc'        !*FD Name of surface temp file
     character(fname_length) :: orog_file   = 'global_orog.nc'                   !*FD Name of orography file
     ! Variable names ---------------------------------------------------
     character(fname_length) :: precip_varname = 'prate'
     character(fname_length) :: stemp_varname  = 'air_temperature'
     character(fname_length) :: orog_varname   = 'hgt'
     ! Arrays for loading climatology -----------------------------------
     real(rk),dimension(:,:,:),pointer :: pclim_load    => null()  !*FD Precip
     real(rk),dimension(:,:,:),pointer :: stclim_load   => null()  !*FD Surf temps
     real(rk),dimension(:,:),  pointer :: orog_load     => null()  !*FD Orog
     ! Arrays for holding climatology -----------------------------------
     real(rk),dimension(:,:),  pointer :: orog_clim     => null()  !*FD Orography
     real(rk),dimension(:,:,:),pointer :: precip_clim   => null()  !*FD Precip
     real(rk),dimension(:,:,:),pointer :: surftemp_clim => null()  !*FD Surface temperature
     ! Grid variables ---------------------------------------------------
     type(global_grid),pointer :: orog_grid   => null()
     type(global_grid),pointer :: precip_grid => null()
     type(global_grid),pointer :: temp_grid   => null()
     type(global_grid),pointer :: all_grid    => null()
     ! Other parameters -------------------------------------------------
     integer :: days_in_year=365
     integer :: hours_in_year=365*24
     real(rk) :: precip_scale=1.0 ! Factor to scale precip by
  end type glex_climate

  interface read_ncdf
     module procedure read_ncdf_2d,read_ncdf_3d
  end interface

contains

  subroutine glex_clim_init(params,filename)

    use glimmer_config
    use glint_global_interp

    type(glex_climate) :: params   !*FD Climate parameters
    character(*)       :: filename !*FD config filename

    type(ConfigSection),pointer :: config !*FD structure holding sections of configuration file   
    integer :: ierr,i

    call ConfigRead(filename,config)
    call glex_clim_readconfig(params,config)
    call CheckSections(config)

    ! Read in climate data

    call read_ncdf(params%precip_file,     &
                   params%precip_varname,  &
                   params%pclim_load,      &
                   params%precip_grid)
    print*, 'Read precip, nx, ny =', &
            params%precip_grid%nx, params%precip_grid%ny
    print*, 'size(pclim_load) =', size(params%pclim_load,1), &
                                  size(params%pclim_load,2), &
                                  size(params%pclim_load,3)

    call read_ncdf(params%stemp_file,    &
                   params%stemp_varname, &
                   params%stclim_load,   &
                   params%temp_grid)
    print*, 'Read temperatures, nx, ny =', &
            params%temp_grid%nx, params%temp_grid%ny
    print*, 'size(stclim_load) =', size(params%stclim_load,1), &
                                   size(params%stclim_load,2), &
                                   size(params%stclim_load,3)

    call read_ncdf(params%orog_file,     &
                   params%orog_varname,  &
                   params%orog_load,     &
                   params%orog_grid)
    print*, 'Read orography, nx, ny =', &
            params%orog_grid%nx, params%orog_grid%ny

    ! Find a suitable grid

    if (associated(params%all_grid)) deallocate(params%all_grid)
    allocate(params%all_grid)
    params%all_grid=min(params%orog_grid,min(params%precip_grid,params%temp_grid))

    ! Allocate climate arrays

    call grid_alloc(params%orog_clim,    params%all_grid)
    call grid_alloc(params%precip_clim,  params%all_grid,size(params%pclim_load,3))
    call grid_alloc(params%surftemp_clim,params%all_grid,size(params%stclim_load,3))

    print*, ' '
    print*, 'size(orog_clim) =', size(params%orog_clim,1),  &
                                 size(params%orog_clim,2)
    print*, 'size(surftemp_clim) =', size(params%surftemp_clim,1), &
                                     size(params%surftemp_clim,2), &
                                     size(params%surftemp_clim,3)
    print*, 'size(precip_clim) =', size(params%precip_clim,1), &
                                   size(params%precip_clim,2), &
                                   size(params%precip_clim,3)

    ! Interpolate or not as the case may be
    ! Orography

    if (params%orog_grid==params%all_grid) then
       params%orog_clim=params%orog_load
    else
       call global_interp(params%orog_grid,params%orog_load,params%all_grid,params%orog_clim,error=ierr)
       if (ierr>0) call interp_error(ierr,__LINE__)
    end if

    ! Precip - first scale it
!lipscomb - Precip is assumed to be in mm/s

    params%pclim_load=params%pclim_load*params%precip_scale

    if (params%precip_grid==params%all_grid) then
       params%precip_clim=params%pclim_load
    else
       do i=1,size(params%pclim_load,3)
          call global_interp(params%precip_grid,        &
                             params%pclim_load(:,:,i),  &
                             params%all_grid,           &
                             params%precip_clim(:,:,i), &
                             error=ierr)
          if (ierr>0) call interp_error(ierr,__LINE__)
       end do
    end if

    ! Temperature

    if (params%temp_grid==params%all_grid) then
       params%surftemp_clim=params%stclim_load
    else
       do i=1,size(params%stclim_load,3)
          call global_interp(params%temp_grid,params%stclim_load(:,:,i),params%all_grid,params%surftemp_clim(:,:,i),error=ierr)
          if (ierr>0) call interp_error(ierr,__LINE__)
       end do
    end if

    ! Deallocate unneeded input data

    deallocate(params%orog_load,params%pclim_load,params%stclim_load)

    ! Fix up a few things

    params%surftemp_clim=params%surftemp_clim-273.15       ! Convert temps to degreesC

  end subroutine glex_clim_init

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine glex_clim_readconfig(params,config)

    use glimmer_config

    type(glex_climate)           :: params !*FD Climate parameters
    type(ConfigSection), pointer :: config !*FD structure holding sections of configuration file   
    type(ConfigSection), pointer :: section

    call GetSection(config,section,'GLEX precip')
    if (associated(section)) then
       call GetValue(section,'filename',params%precip_file)
       call GetValue(section,'variable',params%precip_varname)
       call GetValue(section,'scaling',params%precip_scale)
    end if
    call GetSection(config,section,'GLEX temps')
    if (associated(section)) then
       call GetValue(section,'filename',params%stemp_file)
       call GetValue(section,'variable',params%stemp_varname)
    end if
    call GetSection(config,section,'GLEX orog')
    if (associated(section)) then
       call GetValue(section,'filename',params%orog_file)
       call GetValue(section,'variable',params%orog_varname)
    end if
    call GetSection(config,section,'GLEX climate')
    if (associated(section)) then
       call GetValue(section,'days_in_year',params%days_in_year)
       call GetValue(section,'total_years',params%total_years)
       call GetValue(section,'climate_tstep',params%climate_tstep)
       call GetValue(section,'diurnal_cycle',params%diurnal_cycle)
       params%hours_in_year=params%days_in_year*24
    end if

  end subroutine glex_clim_readconfig

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_2d(filename,varname,array,grid)

    use netcdf

    character(*)                    :: filename,varname
    real(rk),dimension(:,:),pointer :: array
    type(global_grid),      pointer :: grid

    real(rk),dimension(:),allocatable :: dim1,dim2
    real(rk),dimension(:),pointer :: lonbound => NULL()
    real(rk),dimension(:),pointer :: latbound => NULL()

    integer  :: ncerr     ! NetCDF error 
    integer  :: ncid      ! NetCDF file id
    integer  :: varid     ! NetCDF variable id
    integer  :: ndims     ! Number of dimensions
    real(rk) :: offset=0.0,scale=1.0
    integer,      dimension(2) :: dimids,dimlens
    character(20),dimension(2) :: dimnames
    logical :: lonb_present,latb_present

    if (associated(array)) deallocate(array)
    if (associated(grid))  deallocate(grid)

    call read_ncdf_common1(filename,ncid,varid,ndims,varname)

    ! If not a 3d variable, flag and error and exit ----

    if (ndims/=2) then
       print*,'NetCDF: Requested variable has ',ndims,' dimensions'
       print*,'Need 2 dimensions'
       stop
    end if

    call read_ncdf_common2(ncid,varid,ndims,dimids,dimlens,dimnames)

    ! Allocate output and dimension arrays -------------

    allocate(array(dimlens(1),dimlens(2)))
    allocate(dim1(dimlens(1)))
    allocate(dim2(dimlens(2)))

    ! Retrieve variable contents -----------------------

    ncerr=nf90_get_var(ncid, varid, array)
    call handle_err(ncerr,__LINE__)

    call read_ncdf_common3(ncid,varid,offset,scale)

    array=offset+(array*scale)

    ! Get dimension variables --------------------------

    call read_ncdf_getdim(ncid,varid,dimnames(1),dim1)
    call read_ncdf_getdim(ncid,varid,dimnames(2),dim2)

    ! Get boundary arrays, if present ------------------

    call read_ncdf_getbound(ncid,varid,'bounds_lon',lonb_present,lonbound)
    call read_ncdf_getbound(ncid,varid,'bounds_lat',latb_present,latbound)

    ! Construct global grid ----------------------------

    call read_ncdf_common4(grid,lonb_present,latb_present,dim1,dim2,lonbound,latbound)

    ! Tidy up ------------------------------------------

    deallocate(dim1,dim2)
    if (associated(latbound)) deallocate(latbound)
    if (associated(lonbound)) deallocate(lonbound)

  end subroutine read_ncdf_2d

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_3d(filename,varname,array,grid)

    use netcdf

    character(*)                      :: filename,varname
    real(rk),dimension(:,:,:),pointer :: array
    type(global_grid),        pointer :: grid

    real(rk),dimension(:),allocatable :: dim1,dim2,dim3
    real(rk),dimension(:),pointer :: lonbound => NULL()
    real(rk),dimension(:),pointer :: latbound => NULL()

    integer  :: ncerr     ! NetCDF error 
    integer  :: ncid      ! NetCDF file id
    integer  :: varid     ! NetCDF variable id
    integer  :: ndims     ! Number of dimensions
    real(rk) :: offset=0.0,scale=1.0
    integer,      dimension(3) :: dimids,dimlens
    character(20),dimension(3) :: dimnames
    logical :: lonb_present,latb_present

    if (associated(array)) deallocate(array)
    if (associated(grid))  deallocate(grid)

    call read_ncdf_common1(filename,ncid,varid,ndims,varname)

    ! If not a 3d variable, flag and error and exit ----

    if (ndims/=3) then
       print*,'NetCDF: Requested variable has ',ndims,' dimensions'
       print*,'Need 3 dimensions'
       stop
    end if

    call read_ncdf_common2(ncid,varid,ndims,dimids,dimlens,dimnames)

    ! Allocate output and dimension arrays -------------

    allocate(array(dimlens(1),dimlens(2),dimlens(3)))
    allocate(dim1(dimlens(1)))
    allocate(dim2(dimlens(2)))
    allocate(dim3(dimlens(3)))

    ! Retrieve variable contents -----------------------

    ncerr=nf90_get_var(ncid, varid, array)
    call handle_err(ncerr,__LINE__)

    call read_ncdf_common3(ncid,varid,offset,scale)

    array=offset+(array*scale)

    ! Get dimension variables --------------------------

    call read_ncdf_getdim(ncid,varid,dimnames(1),dim1)
    call read_ncdf_getdim(ncid,varid,dimnames(2),dim2)
    call read_ncdf_getdim(ncid,varid,dimnames(3),dim3)

    ! Get boundary arrays, if present ------------------

    call read_ncdf_getbound(ncid,varid,'bounds_lon',lonb_present,lonbound)
    call read_ncdf_getbound(ncid,varid,'bounds_lat',latb_present,latbound)

    ! Construct global grid ----------------------------

    call read_ncdf_common4(grid,lonb_present,latb_present,dim1,dim2,lonbound,latbound)

    ! Tidy up ------------------------------------------

    deallocate(dim1,dim2,dim3)
    if (associated(latbound)) deallocate(latbound)
    if (associated(lonbound)) deallocate(lonbound)

  end subroutine read_ncdf_3d

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_common1(filename,ncid,varid,ndims,varname)

    use netcdf

    character(*) :: filename
    integer :: ncid,varid,ndims
    character(*) :: varname
    integer :: ncerr

    ! Open file

    ncerr=nf90_open(filename,0,ncid)
    call handle_err(ncerr,__LINE__)

    ! Find out the id of variable and its dimensions

    ncerr=nf90_inq_varid(ncid,varname,varid)
    call handle_err(ncerr,__LINE__)
    ncerr=nf90_inquire_variable(ncid, varid, ndims=ndims)
    call handle_err(ncerr,__LINE__)

  end subroutine read_ncdf_common1

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_common2(ncid,varid,ndims,dimids,dimlens,dimnames)

    use netcdf

    integer :: ncid,varid,ndims
    integer,dimension(:) :: dimids,dimlens
    character(*),dimension(:) :: dimnames

    integer :: ncerr,i

    ! Get dimensions ids 

    ncerr=nf90_inquire_variable(ncid, varid, dimids=dimids)
    call handle_err(ncerr,__LINE__)

    ! Retrieve dimension names

    do i=1,ndims
       ncerr=nf90_inquire_dimension(ncid, dimids(i),name=dimnames(i),len=dimlens(i))
       call handle_err(ncerr,__LINE__)
    end do

  end subroutine read_ncdf_common2

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_common3(ncid,varid,offset,scale)

    use netcdf

    integer :: ncid,varid
    real(rk) :: offset,scale
    integer :: ncerr

    ! Get scaling and offset, if present, and apply ----

    ncerr=nf90_get_att(ncid, varid, 'add_offset', offset)
    if (ncerr/=NF90_NOERR) then
       offset=0.0
       ncerr=NF90_NOERR
    end if

    ncerr=nf90_get_att(ncid, varid, 'scale_factor', scale)
    if (ncerr/=NF90_NOERR) then
       scale=1.0
       ncerr=NF90_NOERR
    end if

  end subroutine read_ncdf_common3

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_getdim(ncid,varid,dimname,dim)

    use netcdf

    integer :: ncid,varid
    character(*) :: dimname
    real(rk),dimension(:) :: dim
    integer :: ncerr

    ! Get dimension variables 

    ncerr=nf90_inq_varid(ncid,dimname,varid)
    call handle_err(ncerr,__LINE__)
    ncerr=nf90_get_var(ncid, varid, dim)
    call handle_err(ncerr,__LINE__)

  end subroutine read_ncdf_getdim

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_getbound(ncid,varid,boundname,present,bound)

    use netcdf

    integer :: ncid,varid
    character(*) :: boundname
    logical :: present
    real(rk),dimension(:),pointer :: bound

    integer :: ncerr
    integer,dimension(2) :: dimids,dimlens
    character(20),dimension(2) :: dimnames
    real(rk),dimension(:,:),allocatable :: b
    integer :: i

    ncerr=nf90_inq_varid(ncid,boundname,varid)
    if (ncerr/=NF90_NOERR) then
       present=.false.
       ncerr=NF90_NOERR
    else
       ncerr=nf90_inquire_variable(ncid, varid, dimids=dimids)
       call handle_err(ncerr,__LINE__)
       do i=1,2
          ncerr=nf90_inquire_dimension(ncid, dimids(i), name=dimnames(i),len=dimlens(i))
          call handle_err(ncerr,__LINE__)
       end do
       if (associated(bound)) deallocate(bound)
       allocate(b(dimlens(1),dimlens(2)),bound(dimlens(2)+1))
       ncerr=nf90_get_var(ncid, varid,b)
       call handle_err(ncerr,__LINE__)
       do i=1,dimlens(2)
          bound(i)=b(1,i)
          bound(i+1)=b(2,i)
       end do
       deallocate(b)
       present=.true.
    end if

  end subroutine read_ncdf_getbound

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine read_ncdf_common4(grid,lonb_present,latb_present,dim1,dim2,lonbound,latbound)

    type(global_grid),        pointer :: grid
    logical :: lonb_present,latb_present
    real(rk),dimension(:) :: dim1,dim2
    real(rk), dimension(:), pointer :: lonbound,latbound
    integer :: args

    ! Construct grid type

    allocate(grid)
    args=0
    if (lonb_present) args=args+1
    if (latb_present) args=args+2

    select case(args)
    case(0)
       call new_global_grid(grid,dim1,dim2,correct=.false.)
    case(1)
       call new_global_grid(grid,dim1,dim2,lonb=lonbound)
    case(2)
       call new_global_grid(grid,dim1,dim2,latb=latbound,correct=.false.)
    case(3)
       call new_global_grid(grid,dim1,dim2,lonb=lonbound,latb=latbound)
    end select

  end subroutine read_ncdf_common4

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine handle_err(status,line)

    use netcdf

    integer, intent (in) :: status
    integer, intent (in) :: line

    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       print *, 'Line:',line
       stop "Stopped"
    end if
  end subroutine handle_err

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine example_climate(params,precip,temp,time)

    use glimmer_log

    type(glex_climate) :: params
    real(rk),dimension(:,:),intent(out)  :: precip,temp
    real(rk),intent(in) :: time

    integer :: ntemp,nprecip
    real(rk) :: tsp,tst
    real(rk) :: pos
    integer :: lower,upper

    ntemp   = size(params%surftemp_clim,3)
    nprecip = size(params%precip_clim,3)

    tst=params%hours_in_year/ntemp
    tsp=params%hours_in_year/nprecip

    ! Temperature first

    lower=int(time/tst)
    upper=lower+1
    pos=mod(time,tst)/tst
    call fixbounds(lower,1,ntemp)
    call fixbounds(upper,1,ntemp)
    temp=linear_interp(params%surftemp_clim(:,:,lower),params%surftemp_clim(:,:,upper),pos)

    ! precip

    lower=int(time/tsp)
    upper=lower+1
    pos=mod(time,tsp)/tsp
    call fixbounds(lower,1,nprecip)
    call fixbounds(upper,1,nprecip)
    precip=linear_interp(params%precip_clim(:,:,lower),params%precip_clim(:,:,upper),pos)

    ! Add diurnal cycle to temperature. We assume that
    ! the lowest temperature is at midnight, and the highest at midday.
    ! Obviously, this probably isn't true...

    if (mod(int(time),24)==0)  temp=temp-params%diurnal_cycle
    if (mod(int(time),24)==12) temp=temp+params%diurnal_cycle

  end subroutine example_climate

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  function linear_interp(a,b,pos)

    real(rk),dimension(:,:),intent(in) :: a,b
    real(rk),dimension(size(a,1),size(a,2)) :: linear_interp
    real(rk),               intent(in) :: pos

    linear_interp=a*(1.0-pos)+b*pos

  end function linear_interp

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine fixbounds(in,bottom,top)

    integer :: in,top,bottom

    do
       if (in<=top) exit
       in=in-(top-bottom+1)
    end do

    do
       if (in>=bottom) exit
       in=in+(top-bottom+1)
    end do

  end subroutine fixbounds

end module glint_example_clim
