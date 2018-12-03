module glc_comp_nuopc

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for CISM
  !----------------------------------------------------------------------------

  use ESMF
  use NUOPC                 , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC                 , only : NUOPC_CompFilterPhaseMap, NUOPC_CompAttributeGet, NUOPC_CompAttributeSet
  use NUOPC_Model           , only : model_routine_SS           => SetServices
  use NUOPC_Model           , only : model_label_Advance        => label_Advance
  use NUOPC_Model           , only : model_label_DataInitialize => label_DataInitialize
  use NUOPC_Model           , only : model_label_SetRunClock    => label_SetRunClock
  use NUOPC_Model           , only : model_label_Finalize       => label_Finalize
  use NUOPC_Model           , only : NUOPC_ModelGet
  use shr_sys_mod           , only : shr_sys_abort
  use shr_file_mod          , only : shr_file_getlogunit, shr_file_setlogunit
  use shr_file_mod          , only : shr_file_getloglevel, shr_file_setloglevel, shr_file_getUnit
  use shr_file_mod          , only : shr_file_setIO 
  use shr_cal_mod           , only : shr_cal_ymd2date
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_State_diagnose
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_State_SetScalar
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_State_GetScalar
  use shr_nuopc_time_mod    , only : shr_nuopc_time_alarmInit
  use shr_nuopc_scalars_mod , only : flds_scalar_index_nx
  use shr_nuopc_scalars_mod , only : flds_scalar_index_ny
  use shr_nuopc_scalars_mod , only : flds_scalar_name
  use shr_nuopc_scalars_mod , only : flds_scalar_num
  use shr_nuopc_scalars_mod , only : flds_scalar_index_valid_glc_input
  use med_constants_mod     , only : R8, CS, CL

  use glc_import_export     , only : advertise_fields, realize_fields
  use glc_import_export     , only : export_fields, import_fields 
  use glc_constants         , only : verbose, stdout, stderr, nml_in, radius
  use glc_constants         , only : zero_gcm_fluxes, model_doi_url
  use glc_InitMod           , only : glc_initialize
  use glc_RunMod            , only : glc_run
  use glc_FinalMod          , only : glc_final
  use glc_io                , only : glc_io_write_restart
  use glc_communicate       , only : init_communicate, my_task, master_task
  use glc_time_management   , only : iyear,imonth,iday,ihour,iminute,isecond,runtype
  use glc_fields            , only : ice_sheet
  use perf_mod              , only : t_startf, t_stopf, t_barrierf

  implicit none
  private ! except

  ! Module routines
  public  :: SetServices
  private :: InitializeP0
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelSetRunClock
  private :: ModelAdvance
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  integer                    :: lmpicom
  character(CS)              :: str       ! cpp  defined model name
  character(len=16)          :: inst_name ! full name of current instance (e.g., GLC_0001)
  integer, parameter         :: dbug = 1
  character(len=*),parameter :: modName =  "(glc_comp_nuopc)"
  character(len=*),parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    integer :: dbrc
    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=InitializeP0, phase=0, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MethodRemove(gcomp, label=model_label_SetRunClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, &
         specRoutine=ModelSetRunClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
         specRoutine=ModelFinalize, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug > 5) then
       call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

  end subroutine SetServices

  !===============================================================================

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries

    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv01p"/), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeP0

  !===============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    ! uses
    use shr_nuopc_utils_mod   , only : shr_nuopc_set_component_logging
    use shr_nuopc_utils_mod   , only : shr_nuopc_get_component_instance
    use glc_ensemble          , only : set_inst_vars
    use glc_files             , only : set_filenames, ionml_filename

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM)          :: vm
    character(ESMF_MAXSTR) :: cvalue
    logical                :: exists
    logical                :: isPresent
    character(len=512)     :: diro
    character(len=512)     :: logfile
    integer                :: localpet
    integer                :: compid      ! component id
    integer                :: dbrc
    integer                :: shrlogunit ! original log unit
    integer                :: shrloglev  ! original log level
    integer                :: ierr       ! error code
    integer                :: i,j,n
    character(CL)          :: starttype
    character(CS)          :: myModelName
    integer                :: inst_index    ! number of current instance (e.g., 1)
    character(len=16)      :: inst_suffix   ! character string associated with instance number
    logical                :: lnd_present
    logical                :: glc_coupled_fluxes ! are we sending fluxes to other components?
    character(len=*), parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    character(len=*), parameter :: format = "('("//trim(subname)//") :',A)"
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    !----------------------------------------------------------------------------
    ! generate local mpi comm
    !----------------------------------------------------------------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=lmpicom, localpet=localpet, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !----------------------------------------------------------------------------
    ! initialize CISM MPI stuff
    !----------------------------------------------------------------------------

    call init_communicate(lmpicom)

    !----------------------------------------------------------------------------
    ! Open glc log file
    !----------------------------------------------------------------------------

    if (my_task == master_task) then
       stdout = shr_file_getUnit()
       call shr_file_setIO(ionml_filename,stdout)
    else
       stdout = 6
    endif
    stderr = stdout
    nml_in = shr_file_getUnit()

    !----------------------------------------------------------------------------
    ! determine instance information
    !----------------------------------------------------------------------------

    ! the following sets the module instance variables in glc_ensemble
    call shr_nuopc_get_component_instance(gcomp, inst_suffix, inst_index)
    inst_name = "GLC"//trim(inst_suffix)

    call set_inst_vars(inst_index, inst_name, inst_suffix )

    !----------------------------------------------------------------------------
    ! reset shr logging to my log file
    !----------------------------------------------------------------------------

    call shr_nuopc_set_component_logging(gcomp, localPet==0, stdout,  shrlogunit, shrloglev)

    !----------------------------------------------------------------------------
    ! Set filenames which depend on instance information
    !----------------------------------------------------------------------------

    call set_filenames()

    !----------------------------------------------------------------------------
    ! advertise fields
    !----------------------------------------------------------------------------

    call advertise_fields(gcomp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !----------------------------------------------------------------------------
    ! reset shr logging to original values
    !----------------------------------------------------------------------------

    call shr_file_setLogLevel(shrloglev)
    call shr_file_setLogUnit (shrlogunit)
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

  end subroutine InitializeAdvertise

  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! uses
    use glc_indexing          , only : nx_tot, ny_tot, local_to_global_indices
    use glc_indexing          , only : npts, nx, ny, spatial_to_vector
    use glad_main             , only : glad_get_lat_lon, glad_get_areas

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Mesh)         :: Emesh                 ! esmf
    type(ESMF_DistGrid)     :: DistGrid              ! esmf global index space descriptor
    type(ESMF_Time)         :: currTime              ! Current time
    type(ESMF_Time)         :: startTime             ! Start time
    type(ESMF_Time)         :: stopTime              ! Stop time
    type(ESMF_Time)         :: refTime               ! Ref time
    type(ESMF_TimeInterval) :: timeStep              ! Model timestep
    type(ESMF_Calendar)     :: esmf_calendar         ! esmf calendar
    type(ESMF_CalKind_Flag) :: esmf_caltype          ! esmf calendar type
    integer                 :: ref_ymd               ! reference date (YYYYMMDD)
    integer                 :: ref_tod               ! reference time of day (sec)
    integer                 :: yy,mm,dd              ! Temporaries for time query
    integer                 :: start_ymd             ! start date (YYYYMMDD)
    integer                 :: start_tod             ! start time of day (sec)
    integer                 :: stop_ymd              ! stop date (YYYYMMDD)
    integer                 :: stop_tod              ! stop time of day (sec)
    integer                 :: curr_ymd              ! Start date (YYYYMMDD)
    integer                 :: curr_tod              ! Start time of day (sec)
    character(ESMF_MAXSTR)  :: cvalue                ! config data
    integer                 :: g,n                   ! indices
    character(len=CL)       :: caseid                ! case identifier name
    character(len=CL)       :: ctitle                ! case description title
    character(len=CL)       :: starttype             ! start-type (startup, continue, branch, hybrid)
    character(len=CL)       :: calendar              ! calendar type name
    character(len=CL)       :: hostname              ! hostname of machine running on
    character(len=CL)       :: model_version         ! Model version
    character(len=CL)       :: username              ! user running the model
    integer                 :: nsrest                ! ctsm restart type
    logical                 :: brnch_retain_casename ! flag if should retain the case name on a branch start type
    integer                 :: lbnum                 ! input to memory diagnostic
    integer                 :: shrlogunit            ! original log unit
    integer                 :: shrloglev             ! original log level
    character(ESMF_MAXSTR)  :: convCIM, purpComp
    integer                 :: spatialDim
    integer                 :: numOwnedElements
    real(r8), pointer       :: ownedElemCoords(:)
    real(r8), pointer       :: mesh_lons(:), lons(:,:), lons_vec(:)
    real(r8), pointer       :: mesh_lats(:), lats(:,:), lats_vec(:)
    real(r8), pointer       :: mesh_areas(:), areas(:,:), areas_vec(:)
    type(ESMF_Array)        :: elemAreaArray
    type(ESMF_Array)        :: elemAreaArrayTemp
    integer, pointer        :: global_index(:)
    real(r8)                :: tolerance = 1.e-5_r8
    integer                 :: dbrc
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize) '
    character(*), parameter :: F00   = "('(InitializeRealize) ',8a)"
    character(*), parameter :: F01   = "('(InitializeRealize) ',a,8i8)"
    character(*), parameter :: F91   = "('(InitializeRealize) ',73('-'))"
    integer :: elementCount
    integer, allocatable, target :: seqIndexList(:)
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)

#if (defined _MEMTRACE)
    if (my_task == master_task) then
       lbnum=1
       call memmon_dump_fort('memmon.out','lnd_comp_nuopc_InitializeRealize:start::',lbnum)
    endif
#endif

    !----------------------
    ! Obtain attribute values
    !----------------------

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) caseid

    !TODO: case_desc does not appear in the esm_AddAttributes in esm.F90
    ! just hard-wire from now - is this even needed?
    ! call NUOPC_CompAttributeGet(gcomp, name='case_desc', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    ! read(cvalue,*) ctitle
    ctitle='UNSET'

    call NUOPC_CompAttributeGet(gcomp, name='brnch_retain_casename', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) brnch_retain_casename

    call NUOPC_CompAttributeGet(gcomp, name='start_type', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) starttype

    call NUOPC_CompAttributeGet(gcomp, name='model_version', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) model_version

    call NUOPC_CompAttributeGet(gcomp, name='hostname', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) hostname

    call NUOPC_CompAttributeGet(gcomp, name='username', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) username

    !TODO: the following strings must not be hard-wired - must have module variables
    if (     trim(starttype) == trim('startup')) then
       runtype = 'initial'
    else if (trim(starttype) == trim('continue') ) then
       runtype='continue'
    else if (trim(starttype) == trim('branch')) then
       runtype='branch'
    else
       call shr_sys_abort(subname//' ERROR: unknown starttype' )
    end if

    !----------------------
    ! Get properties from clock
    !----------------------

    call ESMF_ClockGet( clock, &
         currTime=currTime, startTime=startTime, stopTime=stopTime, refTime=RefTime, &
         timeStep=timeStep, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet( currTime, yy=yy, mm=mm, dd=dd, s=curr_tod, rc=rc )
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yy,mm,dd,curr_ymd)

    call ESMF_TimeGet( startTime, yy=yy, mm=mm, dd=dd, s=start_tod, rc=rc )
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yy,mm,dd,start_ymd)

    call ESMF_TimeGet( stopTime, yy=yy, mm=mm, dd=dd, s=stop_tod, rc=rc )
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yy,mm,dd,stop_ymd)

    call ESMF_TimeGet( refTime, yy=yy, mm=mm, dd=dd, s=ref_tod, rc=rc )
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yy,mm,dd,ref_ymd)

    call ESMF_TimeGet( currTime, calkindflag=esmf_caltype, rc=rc )
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (esmf_caltype == ESMF_CALKIND_NOLEAP) then
       ! do nothing
    else if (esmf_caltype == ESMF_CALKIND_GREGORIAN) then
       ! do nothing
    else
       call shr_sys_abort( subname//'ERROR:: bad calendar for ESMF' )
    end if

    !----------------------
    ! Initialize GLC
    !----------------------

    ! now initialize GLC, will use nml_in
    call glc_initialize(clock)
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' GLC Initial Date ',iyear,imonth,iday,ihour,iminute,isecond
       write(stdout,F00) ' Initialize Done'
    endif

    ! TODO (mvertens, 2018-11-28): Determine if land is present as a sanity check - do we need this? 
    ! TODO (mvertens, 2018-11-28): read in model_doi_url

    !--------------------------------
    ! Generate the mesh 
    !--------------------------------

    ! read in the mesh
    call NUOPC_CompAttributeGet(gcomp, name='mesh_glc', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (my_task == master_task) then
       write(stdout,*)'mesh file for domain is ',trim(cvalue)
    end if

    ! create distGrid from global index array
    allocate(global_index(npts))
    global_index(:) = local_to_global_indices()
    DistGrid = ESMF_DistGridCreate(arbSeqIndexList=global_index, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    !deallocate(global_index)

    ! recreate the mesh using the above distGrid
   !EMesh = ESMF_MeshCreate(filename=trim(cvalue), fileformat=ESMF_FILEFORMAT_ESMFMESH, addUserArea=.true., rc=rc)
    EMesh = ESMF_MeshCreate(filename=trim(cvalue), fileformat=ESMF_FILEFORMAT_ESMFMESH, elementDistGrid=distGrid, &
         addUserArea=.true., rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_DistGridGet(DistGrid, localDE=0, elementCount=elementCount, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(seqIndexList(elementCount))
    call ESMF_DistGridGet(DistGrid, localDE=0, seqIndexList=seqIndexList, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! obtain mesh lats, lons and areas
    call ESMF_MeshGet(Emesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (numOwnedElements /= npts) then
       call shr_sys_abort('ERROR: numOwnedElements is not equal to npts')
    end if

    allocate(ownedElemCoords(spatialDim*numOwnedElements))
    allocate(mesh_lons(numOwnedElements))
    allocate(mesh_lats(numOwnedElements))
    allocate(mesh_areas(numOwnedElements))
    do n = 1,npts
       mesh_lons(n) = ownedElemCoords(2*n-1)
       mesh_lats(n) = ownedElemCoords(2*n)
!        write(6,103)n,mesh_lons(n),mesh_lats(n)
! 103    format('DEBUG: n,mesh_lon, mesh_lat= ',i6,2(f20.10,2x)) 
       ! TODO(mvertens, 2018-11-30) for now hard-wire this for testing
       mesh_areas(n) = 9.85405060010902e-06 
    end do

    ! elemAreaArray = ESMF_ArrayCreate(DistGrid, mesh_areas, rc=rc)
    ! if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    ! call ESMF_MeshGet(Emesh, elemAreaArray=elemAreaArray, rc=rc)
    ! if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Check consistency of mesh with internal CISM lats, lons, areas
    !--------------------------------

    ! obtain CISM internal mesh lats, lons and areas
    allocate(lats(nx,ny))
    allocate(lons(nx,ny))
    allocate(areas(nx,ny))

    allocate(lats_vec(npts))
    allocate(lons_vec(npts))
    allocate(areas_vec(npts))

    ! TODO(wjs, 2015-04-02) The following may need a loop over instances
    call glad_get_lat_lon(ice_sheet, instance_index = 1, lats = lats, lons = lons)
    call glad_get_areas(ice_sheet, instance_index = 1, areas = areas)
    call spatial_to_vector(lons, lons_vec)
    call spatial_to_vector(lats, lats_vec)
    call spatial_to_vector(areas, areas_vec)

    ! check that areas, lats and lons from the mesh are not different to a tolerance factor
    ! from areas, lats and lons calculated internally

    areas_vec(:) = areas_vec(:)/(radius*radius) ! convert from m^2 to radians^2
    do n = 1, npts
       write(6,100),n, lons_vec(n), mesh_lons(n)
100    format('DEBUG: CISM n, lon, mesh_lon = ',i6,2(f20.10,2x))
       if (abs(mesh_lons(n) - lons_vec(n)) > tolerance) then
          write(6,*)'ERROR: CISM n, lon, mesh_lon = ',n, lons_vec(n), mesh_lons(n)
          write(6,*)'ERROR: CISM lon diff = ',abs(mesh_lons(n)-lons_vec(n)),' too large'
          !call shr_sys_abort()
       end if
       write(6,101),n, lats_vec(n), mesh_lats(n)
101    format('DEBUG: CISM n, lat, mesh_lat = ',i6,2(f20.10,2x))
       if (abs(mesh_lats(n) - lats_vec(n)) > tolerance) then
          write(6,*)'ERROR: CISM n, lat, mesh_lat = ',n, lats_vec(n), mesh_lats(n)
          write(6,*)'ERROR: CISM lat diff = ',abs(mesh_lats(n)-lats_vec(n)),' too large'
          !call shr_sys_abort()
       end if
       if (abs(mesh_areas(n) - areas_vec(n)) > 1.e-5) then
          write(6,*)'ERROR: CISM n, area, mesh_area = ',n, areas_vec(n), mesh_areas(n)
          write(6,*)'ERROR: CISM area diff = ',abs(mesh_areas(n)-areas_vec(n)),' too large'
          !call shr_sys_abort()
       end if
       !TODO (mvertens, 2018-11-20) Need to add a check for areas - but need to determine the units
       ! of areas coming from the mesh inquire first
    end do

    !--------------------------------
    ! Realize the actively coupled fields
    !--------------------------------

    call realize_fields(gcomp, Emesh, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Create glc export state
    !--------------------------------

    call export_fields(exportState, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set scalars in export state
    call shr_nuopc_methods_State_SetScalar(dble(nx_tot), flds_scalar_index_nx, exportState, lmpicom, &
         flds_scalar_name, flds_scalar_num, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_State_SetScalar(dble(ny_tot), flds_scalar_index_ny, exportState, lmpicom, &
         flds_scalar_name, flds_scalar_num, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Write diagnostics if appropriate
    !--------------------------------

   if (my_task == master_task) then
      write(stdout,F91) 
      write(stdout,F00) trim(inst_name),': start of main integration loop'
      write(stdout,F91) 
   end if

    if (dbug > 1) then
       call shr_nuopc_methods_State_diagnose(exportState,subname//':ES',rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    !--------------------------------
    ! Reset shr logging to original values
    !--------------------------------

    call shr_file_setLogLevel(shrloglev)
    call shr_file_setLogUnit (shrlogunit)

#ifdef USE_ESMF_METADATA
    convCIM  = "CIM"
    purpComp = "Model Component Simulation Description"
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ShortName", "CTSM", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "LongName", "Community Land Model", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "Description", "Community Land Model", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ReleaseDate", "2017", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ModelType", "Terrestrial", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "Name", "TBD", convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "EmailAddress", TBD, convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ResponsiblePartyRole", "contact", convention=convCIM, purpose=purpComp, rc=rc)
#endif

#if (defined _MEMTRACE)
    if(my_task == master_task) then
       write(stdout,*) TRIM(Sub) // ':end::'
       lbnum=1
       call memmon_dump_fort('memmon.out','lnd_comp_nuopc_InitializeRealize:end::',lbnum)
       call memmon_reset_addr()
    endif
#endif

    if (dbug > 5) then
       call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

  end subroutine InitializeRealize

  !===============================================================================

  subroutine ModelAdvance(gcomp, rc)

    !------------------------
    ! Run CISM
    !------------------------

    ! arguments:
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables:
    type(ESMF_clock)  :: clock
    type(ESMF_STATE)  :: importState
    type(ESMF_STATE)  :: exportState
    type(ESMF_Time)   :: NextTime
    integer           :: glcYMD     ! glc model date
    integer           :: glcTOD     ! glc model sec
    integer           :: cesmYMD    ! cesm model date
    integer           :: cesmTOD    ! cesm model sec
    integer           :: cesmYR     ! cesm model year
    integer           :: cesmMON    ! cesm model month
    integer           :: cesmDAY    ! cesm model day
    integer           :: n          ! index
    integer           :: nf         ! fields loop index
    integer           :: ki         ! index of ifrac
    real(R8)          :: lat        ! latitude
    real(R8)          :: lon        ! longitude
    integer           :: shrlogunit
    integer           :: shrloglev
    logical           :: stop_alarm ! is it time to stop
    logical           :: rest_alarm ! is it time to write a restart
    logical           :: done       ! time loop logical
    integer           :: num
    character(len= 2) :: cnum
    character(len=64) :: name
    logical           :: valid_inputs
    real(r8)          :: valid_inputs_real
    integer           :: dbrc
    character(*), parameter :: F01   = "('(glc_comp_nuopc: ModelAdvance) ',a,8i8)"
    character(*), parameter :: subName = "(glc_comp_nuopc: ModelAdvance) "
    !----------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogLevel(max(shrloglev,1))
    call shr_file_setLogUnit (stdout)

    !--------------------------------
    ! Query the Component for its clock at the next time step
    !--------------------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Need to get the next time here since the clock in the nuopc driver does not get advanced until the end
    ! of the time loop and in the mct case it was advanced in the beginning
    call ESMF_ClockGetNextTime(clock, nextTime=nextTime, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet( NextTime, yy=cesmYR, mm=cesmMON, dd=cesmDAY, s=cesmTOD, rc=rc )
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: glc_io_write_restart")

    call shr_cal_ymd2date(cesmYR, cesmMON, cesmDAY, cesmYMD)
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' CESM Run Starting ',cesmYMD, cesmTOD
    endif

    !--------------------------------
    ! Obtain the CISM internal time
    !--------------------------------

    glcYMD = iyear*10000 + imonth*100 + iday
    glcTOD = ihour*3600 + iminute*60 + isecond
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' CISM Run Starting ',glcYMD,glcTOD
    endif

    !--------------------------------
    ! Unpack import state
    !--------------------------------

    call import_fields(importState, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_State_diagnose(importState, subname//':ES',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Determine if have valid inputs to CISM
    !--------------------------------

    call shr_nuopc_methods_State_GetScalar(importState, flds_scalar_index_valid_glc_input, valid_inputs_real, &
         lmpicom, flds_scalar_name, flds_scalar_num, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (valid_inputs_real > 0.) then
       valid_inputs = .true.
    else
       valid_inputs = .false.
    end if

    !--------------------------------
    ! Run CISM
    !--------------------------------

    ! NOTE: in mct the cesmYMD is advanced at the beginning of the time loop 

    write(stdout,*)'DEBUG: glcYMD, cesmYMD= ',glcYMD,cesmYMD
    write(stdout,*)'DEBUG: glcTOD, cesmTOD= ',glcTOD,cesmTOD

    done = .false.
    if (glcYMD == cesmYMD .and. glcTOD == cesmTOD) done = .true.
    do while (.not. done)
       if (glcYMD > cesmYMD .or. (glcYMD == cesmYMD .and. glcTOD > cesmTOD)) then
          write(stdout,*) subname,' ERROR overshot coupling time ',glcYMD,glcTOD,cesmYMD,cesmTOD
          call shr_sys_abort('glc error overshot time')
       endif

       call glc_run(clock, valid_inputs)

       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       if (glcYMD == cesmYMD .and. glcTOD == cesmTOD) done = .true.
       if (verbose .and. my_task == master_task) then
          write(stdout,F01) ' GLC  Date ',glcYMD,glcTOD
       endif

    enddo

    !--------------------------------
    ! Pack export state
    !--------------------------------

    call export_fields(exportState, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_State_diagnose(exportState, subname//':ES',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine ModelAdvance

  !===============================================================================

  subroutine ModelSetRunClock(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)         :: mclock, dclock
    type(ESMF_Time)          :: mcurrtime, dcurrtime
    type(ESMF_Time)          :: mstoptime
    type(ESMF_TimeInterval)  :: mtimestep, dtimestep
    character(len=256)       :: cvalue
    character(len=256)       :: restart_option ! Restart option units
    integer                  :: restart_n      ! Number until restart interval
    integer                  :: restart_ymd    ! Restart date (YYYYMMDD)
    type(ESMF_ALARM)         :: restart_alarm
    character(len=256)       :: stop_option    ! Stop option units
    integer                  :: stop_n         ! Number until stop interval
    integer                  :: stop_ymd       ! Stop date (YYYYMMDD)
    type(ESMF_ALARM)         :: stop_alarm
    character(len=128)       :: name
    integer                  :: alarmcount
    integer                  :: dbrc
    character(len=*),parameter :: subname=trim(modName)//':(ModelSetRunClock) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    ! query the Component for its clocks
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(dclock, currTime=dcurrtime, timeStep=dtimestep, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(mclock, currTime=mcurrtime, timeStep=mtimestep, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! force model clock currtime and timestep to match driver and set stoptime
    !--------------------------------

    mstoptime = mcurrtime + dtimestep
    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! set restart and stop alarms
    !--------------------------------

    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (alarmCount == 0) then

       call ESMF_GridCompGet(gcomp, name=name, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite(subname//'setting alarms for' // trim(name), ESMF_LOGMSG_INFO, rc=dbrc)

       !----------------
       ! Restart alarm
       !----------------
       call NUOPC_CompAttributeGet(gcomp, name="restart_option", value=restart_option, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="restart_n", value=cvalue, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_n

       call NUOPC_CompAttributeGet(gcomp, name="restart_ymd", value=cvalue, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_ymd

       call shr_nuopc_time_alarmInit(mclock, restart_alarm, restart_option, &
            opt_n   = restart_n,           &
            opt_ymd = restart_ymd,         &
            RefTime = mcurrTime,           &
            alarmname = 'alarm_restart', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_AlarmSet(restart_alarm, clock=mclock, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       !----------------
       ! Stop alarm
       !----------------
       call NUOPC_CompAttributeGet(gcomp, name="stop_option", value=stop_option, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="stop_n", value=cvalue, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) stop_n

       call NUOPC_CompAttributeGet(gcomp, name="stop_ymd", value=cvalue, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) stop_ymd

       call shr_nuopc_time_alarmInit(mclock, stop_alarm, stop_option, &
            opt_n   = stop_n,           &
            opt_ymd = stop_ymd,         &
            RefTime = mcurrTime,           &
            alarmname = 'alarm_stop', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_AlarmSet(stop_alarm, clock=mclock, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    end if

    !--------------------------------
    ! Advance model clock to trigger alarms then reset model clock back to currtime
    !--------------------------------

    call ESMF_ClockAdvance(mclock,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug > 5) then
       call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

  end subroutine ModelSetRunClock

  !===============================================================================

  subroutine ModelFinalize(gcomp, rc)

    ! input/output arguments
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                 :: dbrc
    character(*), parameter :: F00   = "('(glc_comp_nuopc) ',8a)"
    character(*), parameter :: F91   = "('(glc_comp_nuopc) ',73('-'))"
    character(len=*),parameter  :: subname=trim(modName)//':(ModelFinalize) '
    !-------------------------------------------------------------------------------

    !--------------------------------
    ! Finalize routine
    !--------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) then
       call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

    if (my_task==master_task) then
       write(stdout,F91)
       write(stdout,F00) 'MOSART: end of main integration loop'
       write(stdout,F91)
    end if

    if (dbug > 5) then
       call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)
    end if

  end subroutine ModelFinalize

  !===============================================================================

end module glc_comp_nuopc
