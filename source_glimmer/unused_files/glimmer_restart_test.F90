
module glimmer_test_restart

  ! This is a trial piece of code to do restart-on-demand...
  ! The idea is that this provides the model for 
  ! auto-generated code

  use netcdf
  use glimmer_restart_common
  use glimmer_restart_statscal

  implicit none

contains

  subroutine rsw_glint_instance(file,dat)

    use glint_type

    type(restart_file),intent(inout) :: file
    type(glint_instance)   :: dat

    call write_pointarr(file,'glint_instance','artm',dat%artm)
    call write_pointarr(file,'glint_instance','arng',dat%arng)
    call write_pointarr(file,'glint_instance','prcp',dat%prcp)
    call write_pointarr(file,'glint_instance','snowd',dat%snowd)
    call write_pointarr(file,'glint_instance','siced',dat%siced)
    call write_pointarr(file,'glint_instance','xwind',dat%xwind)
    call write_pointarr(file,'glint_instance','ywind',dat%ywind)
    call write_pointarr(file,'glint_instance','humid',dat%humid)
    call write_pointarr(file,'glint_instance','lwdown',dat%lwdown)
    call write_pointarr(file,'glint_instance','swdown',dat%swdown)
    call write_pointarr(file,'glint_instance','airpress',dat%airpress)
    call write_pointarr(file,'glint_instance','global_orog',dat%global_orog)
    call write_pointarr(file,'glint_instance','local_orog',dat%local_orog)

  end subroutine rsw_glint_instance

  subroutine rsr_glint_instance(file,dat)

    use glint_type

    type(restart_file),intent(inout) :: file
    type(glint_instance)   :: dat

    call read_pointarr(file,'glint_instance','artm',dat%artm)
    call read_pointarr(file,'glint_instance','arng',dat%arng)
    call read_pointarr(file,'glint_instance','prcp',dat%prcp)
    call read_pointarr(file,'glint_instance','snowd',dat%snowd)
    call read_pointarr(file,'glint_instance','siced',dat%siced)
    call read_pointarr(file,'glint_instance','xwind',dat%xwind)
    call read_pointarr(file,'glint_instance','ywind',dat%ywind)
    call read_pointarr(file,'glint_instance','humid',dat%humid)
    call read_pointarr(file,'glint_instance','lwdown',dat%lwdown)
    call read_pointarr(file,'glint_instance','swdown',dat%swdown)
    call read_pointarr(file,'glint_instance','airpress',dat%airpress)
    call read_pointarr(file,'glint_instance','global_orog',dat%global_orog)
    call read_pointarr(file,'glint_instance','local_orog',dat%local_orog)

  end subroutine rsr_glint_instance

  recursive subroutine rsw_glint_instance_pointarr_1d(file,prefix,name,values)

    use glint_type

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    type(glint_instance),dimension(:),pointer    :: values
    
    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(1) :: sh
    integer :: i

    if (associated(values)) then
       sh=shape(values)
       call write_array_pointer(file,prefix,name,sh)
       do i=1,sh(1)
          call rsw_glint_instance(file,values(i))
       end do
    else
       call write_null_array_pointer(file,prefix,name)
    end if

  end subroutine rsw_glint_instance_pointarr_1d

  recursive subroutine rsr_glint_instance_pointarr_1d(file,prefix,name,values)

    use glint_type

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    type(glint_instance),dimension(:),pointer    :: values
    
    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(1) :: sh
    logical :: assoc
    integer :: i
    
    if (associated(values)) then
       deallocate(values)
       nullify(values)
    end if

    call read_array_pointer(file,prefix,name,sh,assoc)

    if (assoc) then
       allocate(values(sh(1)))
       do i=1,sh(1)
          call rsr_glint_instance(file,values(i))
       end do
    end if

  end subroutine rsr_glint_instance_pointarr_1d

  subroutine write_restart_glint_params(file,dat)

    use glint_main

    type(restart_file),      intent(inout) :: file
    type(glint_params) :: dat

    call rsw_glint_instance_pointarr_1d(file,'glint_params','instances',dat%instances)

  end subroutine write_restart_glint_params

  subroutine read_restart_glint_params(file,dat)

    use glint_main

    type(restart_file),      intent(inout) :: file
    type(glint_params) :: dat

    call rsr_glint_instance_pointarr_1d(file,'glint_params','instances',dat%instances)

  end subroutine read_restart_glint_params

  subroutine write_restart_glimmer_pdd(file,dat)

    use glimmer_pdd

    type(restart_file),      intent(inout) :: file
    type(glimmer_pdd_params),intent(in) :: dat

    call write_statscalvar(file,'pdd','dx',dat%dx)
    call write_statscalvar(file,'pdd','dy',dat%dy)
    call write_statscalvar(file,'pdd','ix',dat%ix)
    call write_statscalvar(file,'pdd','iy',dat%iy)
    call write_statscalvar(file,'pdd','nx',dat%nx)
    call write_statscalvar(file,'pdd','ny',dat%ny)
    call write_statscalvar(file,'pdd','dailytemp',dat%dailytemp)
    call write_statscalvar(file,'pdd','tma',dat%tma)
    call write_statscalvar(file,'pdd','tmj',dat%tmj)
    call write_statscalvar(file,'pdd','dtmj',dat%dtmj)
    call write_statscalvar(file,'pdd','dd_sigma',dat%dd_sigma)
    call write_pointarr(file,'pdd','pddtab',dat%pddtab)
    call write_statscalvar(file,'pdd','wmax',dat%wmax)
    call write_statscalvar(file,'pdd','pddfac_ice',dat%pddfac_ice)
    call write_statscalvar(file,'pdd','pddfac_snow',dat%pddfac_snow)

  end subroutine write_restart_glimmer_pdd

  !---------------------------------------------------------

  subroutine read_restart_glimmer_pdd(file,dat)

    use glimmer_pdd

    type(restart_file),      intent(inout) :: file
    type(glimmer_pdd_params),intent(out)   :: dat

    call read_statscalvar(file,'pdd','dx',dat%dx)
    call read_statscalvar(file,'pdd','dy',dat%dy)
    call read_statscalvar(file,'pdd','ix',dat%ix)
    call read_statscalvar(file,'pdd','iy',dat%iy)
    call read_statscalvar(file,'pdd','nx',dat%nx)
    call read_statscalvar(file,'pdd','ny',dat%ny)
    call read_statscalvar(file,'pdd','dailytemp',dat%dailytemp)
    call read_statscalvar(file,'pdd','tma',dat%tma)
    call read_statscalvar(file,'pdd','tmj',dat%tmj)
    call read_statscalvar(file,'pdd','dtmj',dat%dtmj)
    call read_statscalvar(file,'pdd','dd_sigma',dat%dd_sigma)
    call read_pointarr(file,'pdd','pddtab',dat%pddtab)
    call read_statscalvar(file,'pdd','wmax',dat%wmax)
    call read_statscalvar(file,'pdd','pddfac_ice',dat%pddfac_ice)
    call read_statscalvar(file,'pdd','pddfac_snow',dat%pddfac_snow)

  end subroutine read_restart_glimmer_pdd

  !---------------------------------------------------------

  recursive subroutine write_restart_configvalue(file,dat)

    use glimmer_config

    type(restart_file), intent(inout) :: file
    type(configvalue),intent(in)    :: dat

    call write_statscalvar(file,'configvalue','name',dat%name)
    call write_statscalvar(file,'configvalue','value',dat%value)

    call write_pointer(file,'configvalue','next',associated(dat%next))
    if (associated(dat%next)) call write_restart_configvalue(file,dat%next)
 
  end subroutine write_restart_configvalue

  !---------------------------------------------------------

  recursive subroutine read_restart_configvalue(file,dat)

    use glimmer_config

    type(restart_file), intent(inout) :: file
    type(configvalue),pointer    :: dat

    logical :: assoc

    allocate(dat)
    call read_statscalvar(file,'configvalue','name',dat%name)
    call read_statscalvar(file,'configvalue','value',dat%value)

    call read_pointer(file,'configvalue','next',assoc)
    if (assoc) call read_restart_configvalue(file,dat%next)
 
  end subroutine read_restart_configvalue

  !---------------------------------------------------------

  recursive subroutine write_restart_configsection(file,dat)

    use glimmer_config

    type(restart_file), intent(inout) :: file
    type(configsection),intent(in)    :: dat

    call write_statscalvar(file,'configsection','name',dat%name)
    call write_statscalvar(file,'configsection','used',dat%used)

    call write_pointer(file,'configsection','values',associated(dat%values))
    if (associated(dat%values)) call write_restart_configvalue(file,dat%values)

    call write_pointer(file,'configsection','next',associated(dat%next))
    if (associated(dat%next)) call write_restart_configsection(file,dat%next)
    
  end subroutine write_restart_configsection

  !---------------------------------------------------------

  recursive subroutine read_restart_configsection(file,dat)
    
    use glimmer_config

    type(restart_file), intent(inout) :: file
    type(configsection),pointer :: dat

    logical :: assoc

    allocate(dat)
    call read_statscalvar(file,'configsection','name',dat%name)
    call read_statscalvar(file,'configsection','used',dat%used)

    call read_pointer(file,'configsection','values',assoc)
    if (assoc) call read_restart_configvalue(file,dat%values)

    call read_pointer(file,'configsection','next',assoc)
    if (assoc) call read_restart_configsection(file,dat%next)
 
  end subroutine read_restart_configsection

end module glimmer_test_restart

!------------------------------------------------------------------------------------------------
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!------------------------------------------------------------------------------------------------

program restart_test

  use glimmer_pdd
  use glint_main
  use glimmer_config
  use glimmer_restart_common
  use glimmer_test_restart

  implicit none

  type(restart_file) :: rfile,infile
  type(glimmer_pdd_params) :: pdd1,pdd2
  type(glint_params) :: ice,ice2
  character(30) :: fname,cname
  type(configsection),pointer :: config=> null(),config2=>null()

  fname = 'test.nc'
  cname='gland20.config'

  call ConfigRead(cname,config)
  call glimmer_pdd_init(pdd1,config)

  allocate(ice%instances(2))

  rfile = new_restart_file(fname)
  call write_restart_glint_params(rfile,ice)
  call rsw_glimmer_pdd_params(rfile,pdd1)
  !call rsw_configsection_point(rfile,config)
  call close_restart_file(rfile)

  ! Now try and read it back

  infile=open_restart_file(fname)
  call read_restart_glint_params(infile,ice2)
  call rsr_glimmer_pdd_params(infile,pdd2)
  !call rsr_configsection_point(infile,config2)
  call close_restart_file(infile)

  ! Result of tests

!!$  print*,(pdd1%dx==pdd2%dx)
!!$  print*,(pdd1%dy==pdd2%dy)
!!$  print*,(pdd1%ix==pdd2%ix)
!!$  print*,(pdd1%iy==pdd2%iy)
!!$  print*,(pdd1%nx==pdd2%nx)
!!$  print*,(pdd1%ny==pdd2%ny)
!!$  print*,(pdd1%dailytemp==pdd2%dailytemp)
!!$  print*,(pdd1%tma==pdd2%tma)
!!$  print*,(pdd1%tmj==pdd2%tmj)
!!$  print*,(pdd1%dtmj==pdd2%dtmj)
!!$  print*,(pdd1%dd_sigma==pdd2%dd_sigma)
!!$  if (associated(pdd1%pddtab)) then
!!$     print*,(pdd1%pddtab==pdd2%pddtab)
!!$  else
!!$     print*,(associated(pdd1%pddtab)==associated(pdd2%pddtab))
!!$  end if
!!$  print*,(pdd1%wmax==pdd2%wmax)
!!$  print*,(pdd1%pddfac_ice==pdd2%pddfac_ice)
!!$  print*,(pdd1%pddfac_snow==pdd2%pddfac_snow)


 ! call PrintConfig(config2)

end program restart_test
