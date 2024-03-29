module glc_comp_mct

  ! !uses:

  use shr_sys_mod
  use shr_kind_mod,        only: IN=>SHR_KIND_IN, R8=>SHR_KIND_R8, &
                                 CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_file_mod,        only: shr_file_getunit, shr_file_getlogunit, shr_file_getloglevel, &
                                 shr_file_setlogunit, shr_file_setloglevel, shr_file_setio, &
                                 shr_file_freeunit
  use mct_mod
  use esmf

  use seq_cdata_mod    ,   only: seq_cdata_getdata=>seq_cdata_setptrs, seq_cdata
  use seq_infodata_mod
  use seq_timemgr_mod

  use glc_import_export
  use glc_cpl_indices
  use glc_constants,       only: verbose, stdout, stderr, radius
  use glc_constants,       only: zero_gcm_fluxes_for_all_icesheets, model_doi_url, icesheet_names
  use glc_InitMod,         only: glc_initialize
  use glc_RunMod,          only: glc_run
  use glc_FinalMod,        only: glc_final
  use glc_io,              only: glc_io_write_restart
  use glc_communicate,     only: init_communicate, my_task, master_task
  use glc_time_management, only: iyear,imonth,iday,ihour,iminute,isecond,runtype
  use glc_fields,          only: ice_sheet

  ! Public types:
  implicit none
  save
  private ! except

  ! Public interfaces
  public :: glc_init_mct
  public :: glc_run_mct
  public :: glc_final_mct

  ! Private data interfaces

  !--- stdin input stuff ---
  character(CS) :: str                  ! cpp  defined model name
  
  ! my_task_local and master_task_local are needed for some checks that are done before
  ! init_communicate is called (although, it's possible that init_communicate could be
  ! moved to earlier to prevent the need for these copies)
  integer(IN)           :: my_task_local        ! my task in mpi communicator mpicom 
  integer(IN),parameter :: master_task_local=0  ! task number of master task

  type(seq_infodata_type), pointer :: infodata

!===============================================================================
CONTAINS
!===============================================================================

  subroutine glc_init_mct( EClock, cdata, x2g, g2x, NLFilename )

    ! description:
    ! initialize glc model

    ! uses:

    use seq_comm_mct       , only : seq_comm_suffix, seq_comm_inst, seq_comm_name
    use glc_ensemble       , only : set_inst_vars, write_inst_vars, get_inst_name
    use glc_files          , only : set_filenames, ionml_filename
    use glc_coupling_flags , only : has_ocn_coupling, has_ice_coupling
    use glc_indexing       , only : get_nx_tot, get_ny_tot, get_npts
    
    ! input/output parameters:

    type(ESMF_Clock)         , intent(inout) :: EClock
    type(seq_cdata)          , intent(inout) :: cdata
    type(mct_aVect)          , intent(inout) :: x2g, g2x
    character(len=*), optional  , intent(in) :: NLFilename ! Namelist filename

    !--- local variables ---
    integer(IN)              :: ierr        ! error code
    integer(IN)              :: i,j,n
    integer(IN)              :: COMPID
    integer(IN)              :: mpicom
    type(mct_gsMap), pointer :: gsMap
    type(mct_gGrid), pointer :: dom
    integer(IN)              :: shrlogunit, shrloglev  
    character(CL)            :: starttype
    character(CS)            :: myModelName
    logical                  :: lnd_present
    logical                  :: glc_coupled_fluxes ! are we sending fluxes to other components?
    integer                  :: inst_index    ! number of current instance (e.g., 1)
    character(len=16)        :: inst_name     ! full name of current instance (e.g., GLC_0001)
    character(len=16)        :: inst_suffix   ! character string associated with instance number
                                              ! (e.g., "_0001", or "" for the single-instance case)
    !--- formats ---
    character(*), parameter :: F00   = "('(glc_init_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_init_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_init_mct) ',a,4es13.6)"
    character(*), parameter :: F91   = "('(glc_init_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_init_mct) "
    !-------------------------------------------------------------------------------

    !----------------------------------------------------------------------------
    ! Determine attribute vector indices
    !----------------------------------------------------------------------------
    
    call glc_cpl_indices_set()

    !----------------------------------------------------------------------------
    ! Set cdata pointers
    !----------------------------------------------------------------------------

    call seq_cdata_getdata(cdata, &
         id=compid, mpicom=mpicom, gsMap=gsMap, dom=dom, infodata=infodata)

    call mpi_comm_rank(mpicom, my_task_local, ierr)

    !---------------------------------------------------------------------------
    ! set variables that depend on ensemble index
    !---------------------------------------------------------------------------

    inst_name   = seq_comm_name(COMPID)
    inst_index  = seq_comm_inst(COMPID)
    inst_suffix = seq_comm_suffix(COMPID)

    call set_inst_vars(inst_index, inst_name, inst_suffix)
    call get_inst_name(myModelName)
    call set_filenames()

    !---------------------------------------------------------------------------
    ! use infodata to determine type of run
    !---------------------------------------------------------------------------

    call seq_infodata_GetData( infodata, &
         start_type=starttype)

    if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
       runtype = "initial"
    else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
       runtype = "continue"
    else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
       runtype = "branch"
    else
       write(*,*) 'glc_comp_mct ERROR: unknown starttype'
       call shr_sys_abort()
    end if

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------
    !--- open log file ---
    if (my_task_local == master_task_local) then
       stdout = shr_file_getUnit()
       call shr_file_setIO(ionml_filename,stdout)
    else
       stdout = 6
    endif
    stderr = stdout

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)

    if (verbose .and. my_task_local == master_task_local) then
       write(stdout,F00) ' Starting'
       write(stdout,*) subname, 'COMPID: ', COMPID
       call write_inst_vars
       call shr_sys_flush(stdout)
    endif

    ! ------------------------------------------------------------------------
    ! Perform initial sanity checks, making sure we're okay to run
    ! ------------------------------------------------------------------------

    ! The coupler allows running glc without lnd. MPAS-LI can handle that configuration,
    ! but CISM cannot. So abort if there is no land model (data or active) present.
    call seq_infodata_GetData(infodata, &
         lnd_present=lnd_present)
    if (.not. lnd_present) then
       call shr_sys_abort('ERROR: CISM requires a land component (either active land or dlnd)&
            & - it cannot be run with a stub land')
    end if

    ! ------------------------------------------------------------------------
    ! Get other info from the coupler
    ! ------------------------------------------------------------------------

    call seq_infodata_GetData(infodata, &
         model_doi_url=model_doi_url)

    ! ------------------------------------------------------------------------
    ! Do main initialization
    ! ------------------------------------------------------------------------

    call init_communicate(mpicom)

    call glc_initialize(EClock)
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' GLC Initial Date ',iyear,imonth,iday,ihour,iminute,isecond
       write(stdout,F00) ' Initialize Done'
       call shr_sys_flush(stdout)
    endif

    ! Initialize MCT gsmap

    call glc_SetgsMap_mct(mpicom, COMPID, gsMap)

    ! Initialize MCT domain

    call glc_domain_mct(gsMap,dom)

    ! Set flags in infodata

    glc_coupled_fluxes = (.not. zero_gcm_fluxes_for_all_icesheets)
    call seq_infodata_PutData(infodata, &
         glc_present= .true., &
         glclnd_present = .true., &
         glcocn_present = has_ocn_coupling(), &
         glcice_present = has_ice_coupling(), &
         glc_prognostic = .true., &
         glc_coupled_fluxes = glc_coupled_fluxes, &
         glc_nx=get_nx_tot(instance_index=1), &
         glc_ny=get_ny_tot(instance_index=1))

    ! Initialize MCT attribute vectors

    call mct_aVect_init(g2x, rList=seq_flds_g2x_fields, lsize=get_npts(instance_index=1))
    call mct_aVect_zero(g2x)

    call mct_aVect_init(x2g, rList=seq_flds_x2g_fields, lsize=get_npts(instance_index=1))
    call mct_aVect_zero(x2g)

    ! Create initial glc export state

    call glc_export(g2x%rattr)
	 
   if (my_task == master_task) then
      write(stdout,F91) 
      write(stdout,F00) trim(myModelName),': start of main integration loop'
      write(stdout,F91) 
   end if

    !----------------------------------------------------------------------------
    ! Reset shr logging to original values
    !----------------------------------------------------------------------------
    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)
    call shr_sys_flush(stdout)

end subroutine glc_init_mct

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: glc_run_mct
!
! !DESCRIPTION:
!     run method for glc model
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------

subroutine glc_run_mct( EClock, cdata, x2g, g2x)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   type(ESMF_Clock)            ,intent(inout) :: EClock
   type(seq_cdata)             ,intent(inout) :: cdata
   type(mct_aVect)             ,intent(inout) :: x2g        ! driver -> glc
   type(mct_aVect)             ,intent(inout) :: g2x        ! glc    -> driver
   
!EOP
   !--- local ---
   integer(IN)   :: cesmYMD           ! cesm model date
   integer(IN)   :: cesmTOD           ! cesm model sec
   integer(IN)   :: glcYMD            ! glc model date
   integer(IN)   :: glcTOD            ! glc model sec 
   integer(IN)   :: n                 ! index
   integer(IN)   :: nf                ! fields loop index
   integer(IN)   :: ki                ! index of ifrac
   real(R8)      :: lat               ! latitude
   real(R8)      :: lon               ! longitude
   integer(IN)   :: shrlogunit, shrloglev  
   logical       :: stop_alarm        ! is it time to stop
   logical       :: rest_alarm        ! is it time to write a restart
   logical       :: done              ! time loop logical
   integer           :: num 
   character(len= 2) :: cnum
   character(len=64) :: name
   logical           :: valid_inputs

   character(*), parameter :: F00   = "('(glc_run_mct) ',8a)"
   character(*), parameter :: F01   = "('(glc_run_mct) ',a,8i8)"
   character(*), parameter :: F04   = "('(glc_run_mct) ',2a,2i8,'s')"
   character(*), parameter :: subName = "(glc_run_mct) "
!-------------------------------------------------------------------------------

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)

    ! Set internal time info
 
    call seq_timemgr_EClockGetData(EClock,curr_ymd=cesmYMD, curr_tod=cesmTOD)

    stop_alarm = seq_timemgr_StopAlarmIsOn( EClock )

    glcYMD = iyear*10000 + imonth*100 + iday
    glcTOD = ihour*3600 + iminute*60 + isecond
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' Run Starting ',glcYMD,glcTOD
       call shr_sys_flush(stdout)
    endif

    ! Unpack

    call glc_import(x2g%rattr)

    ! Run 

    call seq_infodata_GetData( infodata, glc_valid_input=valid_inputs)

    done = .false.
    if (glcYMD == cesmYMD .and. glcTOD == cesmTOD) done = .true.
    do while (.not. done) 
       if (glcYMD > cesmYMD .or. (glcYMD == cesmYMD .and. glcTOD > cesmTOD)) then
          write(stdout,*) subname,' ERROR overshot coupling time ',glcYMD,glcTOD,cesmYMD,cesmTOD
          call shr_sys_abort('glc error overshot time')
       endif

       call glc_run(EClock, valid_inputs)

       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       if (glcYMD == cesmYMD .and. glcTOD == cesmTOD) done = .true.
       if (verbose .and. my_task == master_task) then
          write(stdout,F01) ' GLC  Date ',glcYMD,glcTOD
       endif
    enddo

    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' Run Done',glcYMD,glcTOD
       call shr_sys_flush(stdout)
    endif
    
    ! Pack

    call glc_export(g2x%rattr)
    
    ! Log output for model date

    if (my_task == master_task) then
       call seq_timemgr_EClockGetData(EClock,curr_ymd=cesmYMD, curr_tod=cesmTOD)
       write(stdout,F01) ' CESM Date ', cesmYMD,cesmTOD
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       write(stdout,F01) ' GLC  Date ',glcYMD,glcTOD
       write(stdout,*) '(glc_run_mct) valid Inputs =  ', valid_inputs
       call shr_sys_flush(stdout)
    end if

    ! If time to write restart, do so

    rest_alarm = seq_timemgr_RestartAlarmIsOn( EClock )
    if (rest_alarm) then
       call glc_io_write_restart(ice_sheet%instances(1), icesheet_names(1), EClock)
    endif

    ! Reset shr logging to original values

    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)
    call shr_sys_flush(stdout)
    
end subroutine glc_run_mct

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: glc_final_mct
!
! !DESCRIPTION:
!     finalize method for glc model
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------
!
subroutine glc_final_mct( EClock, cdata, x2d, d2x)

! !USES:

    use glc_ensemble, only : get_inst_name

! !INPUT/OUTPUT PARAMETERS:

   type(ESMF_Clock)            ,intent(inout) :: EClock
   type(seq_cdata)             ,intent(inout) :: cdata
   type(mct_aVect)             ,intent(inout) :: x2d        
   type(mct_aVect)             ,intent(inout) :: d2x        

!EOP

   integer(IN)             :: shrlogunit, shrloglev  
   character(CS)           :: myModelName


   !--- formats ---
   character(*), parameter :: F00   = "('(glc_final_mct) ',8a)"
   character(*), parameter :: F01   = "('(glc_final_mct) ',a,8i8)"
   character(*), parameter :: F91   = "('(glc_final_mct) ',73('-'))"
   character(*), parameter :: subName = "(glc_final_mct) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   ! Reset shr logging to my log file
   call shr_file_getLogUnit (shrlogunit)
   call shr_file_getLogLevel(shrloglev)
   call shr_file_setLogUnit (stdout)

   call get_inst_name(myModelName)

   if (my_task == master_task) then
      write(stdout,F91) 
      write(stdout,F00) trim(myModelName),': end of main integration loop'
      write(stdout,F91) 
   end if
      
   call glc_final()

   if (verbose .and. my_task == master_task) then
      write(stdout,F00) ' Done'
      call shr_sys_flush(stdout)
   endif

   ! Reset shr logging to original values

   call shr_file_setLogUnit (shrlogunit)
   call shr_file_setLogLevel(shrloglev)
   call shr_sys_flush(stdout)

end subroutine glc_final_mct

!=================================================================================

subroutine glc_setgsmap_mct( mpicom_g, GLCID, gsMap_g )
  
  ! Initialize MCT global seg map
  
  use glc_indexing, only : local_to_global_indices, get_npts_tot

  integer        , intent(in)  :: mpicom_g
  integer        , intent(in)  :: GLCID
  type(mct_gsMap), intent(out) :: gsMap_g

  ! Local Variables

  integer :: ier

  !--- formats ---
  character(*), parameter :: F02   = "('(glc_SetgsMap_mct) ',a,4es13.6)"
  character(*), parameter :: subName = "(glc_SetgsMap_mct) "
  !-------------------------------------------------------------------

  ! npts_tot is the number of grid cells on CISM's global grid. It is passed to
  ! mct_gsMapinit in case there are ice-free grid cells on the global grid that are not
  ! assigned to any processor.
  call mct_gsMap_init( gsMap_g, local_to_global_indices(instance_index=1), mpicom_g, GLCID, &
       gsize = get_npts_tot(instance_index=1))

end subroutine glc_SetgsMap_mct

!===============================================================================

  subroutine glc_domain_mct( gsMap_g, dom_g )

    use glc_indexing, only : get_npts, get_nx, get_ny, spatial_to_vector
    use glad_main, only : glad_get_lat_lon, glad_get_areas
    
    !-------------------------------------------------------------------
    type(mct_gsMap), intent(inout) :: gsMap_g
    type(mct_ggrid), intent(out)   :: dom_g      

    ! Local Variables

    integer :: npts, nx, ny
    integer :: i,n                ! index
    real(r8), pointer :: data(:)  ! temporary
    integer , pointer :: idata(:) ! temporary
    real(r8), allocatable :: lats(:,:)  ! latitude of each point (degrees)
    real(r8), allocatable :: lons(:,:)  ! longitude of each point (degrees)
    real(r8), allocatable :: areas(:,:) ! area of each point (square meters)
    character(*), parameter :: subName = "(glc_domain_mct) "
    !-------------------------------------------------------------------

    npts = get_npts(instance_index=1)
    nx = get_nx(instance_index=1)
    ny = get_ny(instance_index=1)

    ! Initialize mct domain type

    call mct_gGrid_init( GGrid=dom_g, CoordChars=trim(seq_flds_dom_coord), &
       OtherChars=trim(seq_flds_dom_other), lsize=npts )

    ! Initialize attribute vector with special value

    allocate(data(npts))
    dom_g%data%rAttr(:,:) = -9999.0_R8
    dom_g%data%iAttr(:,:) = -9999
    data(:) = 0.0_R8     
    call mct_gGrid_importRAttr(dom_g,"mask" ,data,npts) 
    call mct_gGrid_importRAttr(dom_g,"frac" ,data,npts) 

    ! Determine global gridpoint number attribute, GlobGridNum, which is set automatically by MCT

    call mct_gsMap_orderedPoints(gsMap_g, my_task, idata)
    call mct_gGrid_importIAttr(dom_g,'GlobGridNum',idata,npts)

    ! Fill in correct values for domain components
    ! lat/lon in degrees, area in radians^2, real-valued mask and frac

    allocate(lats(nx, ny))
    allocate(lons(nx, ny))
    allocate(areas(nx, ny))
    
    call glad_get_lat_lon(ice_sheet, instance_index = 1, &
         lats = lats, lons = lons)
    call glad_get_areas(ice_sheet, instance_index = 1, areas = areas)

    call spatial_to_vector(instance_index=1, &
         arr_spatial = lons, &
         arr_vector = data)
    call mct_gGrid_importRattr(dom_g,"lon",data,npts) 

    call spatial_to_vector(instance_index=1, &
         arr_spatial = lats, &
         arr_vector = data)
    call mct_gGrid_importRattr(dom_g,"lat",data,npts) 

    call spatial_to_vector(instance_index=1, &
         arr_spatial = areas, &
         arr_vector = data)
    ! convert from m^2 to radians^2
    data = data/(radius*radius)
    call mct_gGrid_importRattr(dom_g,"area",data,npts) 

    ! For now, assume mask and frac are 1 everywhere. This may need to be changed in the
    ! future.
    data(:) = 1._r8
    call mct_gGrid_importRattr(dom_g,"mask",data,npts) 
    call mct_gGrid_importRattr(dom_g,"frac",data,npts) 

    deallocate(data)
    deallocate(idata)
    deallocate(lats)
    deallocate(lons)
    deallocate(areas)

    if (verbose .and. my_task==master_task) then
       i = mct_aVect_nIattr(dom_g%data)
       do n = 1,i
          write(stdout,*) subname,' dom_g ',n,minval(dom_g%data%iAttr(n,:)),maxval(dom_g%data%iAttr(n,:))
       enddo
       i = mct_aVect_nRattr(dom_g%data)
       do n = 1,i
          write(stdout,*) subname,' dom_g ',n,minval(dom_g%data%rAttr(n,:)),maxval(dom_g%data%rAttr(n,:))
       enddo
       call shr_sys_flush(stdout)
    endif

  end subroutine glc_domain_mct
    
!===============================================================================

end module glc_comp_mct
