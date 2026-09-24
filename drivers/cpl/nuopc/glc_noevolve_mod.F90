module glc_noevolve_mod

  !----------------------------------------------------------------------------
  ! Handles the "noevolve" (data-glacier) path for individual ice sheets within
  ! the CISM NUOPC cap.  A noevolve ice sheet reads static topography/thickness
  ! from a file and computes ice runoff (Fgrg_rofi) from the incoming SMB each
  ! coupling step.  All CISM-specific export fields (heat flux, liquid runoff,
  ! flooding, volume) are zero-filled once at initialization.
  !
  ! Adapted from components/cdeps/dglc/dglc_datamode_noevolve_mod.F90.
  !----------------------------------------------------------------------------

  use ESMF                , only : ESMF_State, ESMF_Mesh, ESMF_DistGrid, ESMF_Field
  use ESMF                , only : ESMF_StateGet, ESMF_FieldGet, ESMF_VMGetCurrent
  use ESMF                , only : ESMF_FieldBundle, ESMF_FieldBundleCreate, ESMF_FieldCreate
  use ESMF                , only : ESMF_FieldBundleAdd, ESMF_MESHLOC_ELEMENT, ESMF_TYPEKIND_R8
  use ESMF                , only : ESMF_MeshGet, ESMF_DistGridGet
  use ESMF                , only : ESMF_GridComp, ESMF_GridCompGet
  use ESMF                , only : ESMF_VM, ESMF_VMBroadCast, ESMF_VMAllreduce, ESMF_VMGetCurrent
  use ESMF                , only : ESMF_REDUCE_SUM
  use ESMF                , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF                , only : ESMF_Time, ESMF_TimeGet, ESMF_Clock, ESMF_ClockGet 
  use NUOPC               , only : NUOPC_IsConnected
  use shr_kind_mod        , only : r8=>shr_kind_r8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod         , only : shr_sys_abort
  use shr_log_mod         , only : shr_log_error
  use shr_cal_mod         , only : shr_cal_ymd2date
  use shr_const_mod       , only : SHR_CONST_RHOICE, SHR_CONST_RHOSW, SHR_CONST_REARTH, SHR_CONST_SPVAL
  use dshr_methods_mod    , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use pio                 , only : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t
  use pio                 , only : pio_openfile, pio_inq_varid, pio_inq_varndims, pio_inq_vardimid
  use pio                 , only : pio_inq_dimlen, pio_initdecomp, pio_read_darray, pio_double
  use pio                 , only : pio_createfile, pio_def_dim, pio_def_var, pio_put_att, pio_fill
  use pio                 , only : pio_set_fill, pio_put_att, pio_enddef, pio_write_darray, PIO_GLOBAL
  use pio                 , only : pio_closefile, pio_freedecomp, PIO_BCAST_ERROR, PIO_NOWRITE, PIO_CLOBBER
  use pio                 , only : pio_seterrorhandling
  use shr_pio_mod         , only : shr_pio_getiosys, shr_pio_getiotype
  use glc_io              , only : glc_filename
  use glc_files           , only : get_rpointer_filename
  use glc_constants       , only : icesheet_modes, icesheet_names_total, stdout 
  use glc_constants       , only : noevolve_global_nx, noevolve_global_ny
  use glc_constants       , only : noevolve_internal_gridsize, noevolve_datafiles
  use glc_communicate     , only : my_task, master_task
  use glc_time_management , only : runtype
  use glc_import_export   , only : flds_scalar_index_nx, flds_scalar_index_ny
  use glc_import_export   , only : flds_scalar_name, flds_scalar_num
  use nuopc_shr_methods   , only : state_setscalar

  implicit none
  private

  public :: glc_noevolve_init
  public :: glc_noevolve_advance
  public :: glc_noevolve_restart_write
  public :: glc_noevolve_restart_read

  private :: glc_noevolve_zero_cism_fields

  !----------------------------------------------------------------------------
  ! Per-ice-sheet pointer type (field data lives in the ESMF field; we hold
  ! Fortran pointers into it for convenience).
  !----------------------------------------------------------------------------
  type icesheet_ptr_t
     real(r8), pointer :: ptr(:) => null()
  end type icesheet_ptr_t

  type icesheet_info_t
     integer, allocatable :: gindex(:)
     character(len=CS) :: name ! icesheet name (gris, ais,..) 
     character(len=CS) :: mode ! prognostic or noevolve
     integer           :: nx   ! global nx size
     integer           :: ny   ! global ny size
  end type icesheet_info_t
  type(icesheet_info_t), allocatable :: icesheet_info(:) 

  ! Field name constants match the names used in glc_import_export.F90 (hard-wired for now)
  character(len=*), parameter :: field_in_tsrf = 'Sl_tsrf'
  character(len=*), parameter :: field_in_qice = 'Flgl_qice'
  character(len=*), parameter :: field_in_so_t_depth = 'So_t_depth'
  character(len=*), parameter :: field_in_so_s_depth = 'So_s_depth'

  character(len=*), parameter :: field_out_area = 'Sg_area'
  character(len=*), parameter :: field_out_ice_covered = 'Sg_ice_covered'
  character(len=*), parameter :: field_out_topo = 'Sg_topo'
  character(len=*), parameter :: field_out_icemask = 'Sg_icemask'
  character(len=*), parameter :: field_out_icemask_coupled_fluxes = 'Sg_icemask_coupled_fluxes'
  character(len=*), parameter :: field_out_hflx_to_lnd = 'Flgg_hflx'
  character(len=*), parameter :: field_out_rofi_to_ice = 'Figg_rofi'
  character(len=*), parameter :: field_out_rofi_to_ocn = 'Fgrg_rofi'
  character(len=*), parameter :: field_out_rofl_to_ocn = 'Fgrg_rofl'

  ! Import field pointer (SMB, needed every coupling step)
  type(icesheet_ptr_t), allocatable :: Flgl_qice(:)

  ! Export field pointers (indexed 1..num_noevolve)
  type(icesheet_ptr_t), allocatable :: Sg_area(:)
  type(icesheet_ptr_t), allocatable :: Sg_ice_covered(:)
  type(icesheet_ptr_t), allocatable :: Sg_topo(:)
  type(icesheet_ptr_t), allocatable :: Sg_icemask(:)
  type(icesheet_ptr_t), allocatable :: Sg_icemask_coupled_fluxes(:)
  type(icesheet_ptr_t), allocatable :: Fgrg_rofi(:)

  real(r8), parameter :: thk0 = 1._r8  ! thickness scaling (= 1 in modern CISM)

  integer :: num_icesheets_total ! total number of ice sheets (prognostic + noevolve)

  type(iosystem_desc_t), pointer :: pio_subsystem
  integer :: pio_io_type

  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine glc_noevolve_init(NStateExp, NStateImp, meshes, clock, rc)

    !---------------------------------------------------------------------------
    ! Read static topography and thickness for each noevolve ice sheet, compute
    ! the time-invariant export fields (area, topo, ice_covered, masks), and
    ! grab a pointer into the import SMB field used on every coupling step.
    !
    ! Cell areas are computed from the user-specified internal grid spacing
    ! (matching dglc datamode_noevolve convention):
    !   Sg_area = (internal_gridsize / SHR_CONST_REARTH)**2   ! radians^2
    !---------------------------------------------------------------------------

    ! input/output variables
    type(ESMF_State)      , intent(inout) :: NStateExp(:)         ! all ice sheets (including prognostic)
    type(ESMF_State)      , intent(inout) :: NStateImp(:)         ! all ice sheets (including prognostic)
    type(ESMF_Mesh)       , intent(in)    :: meshes(:)            ! all ice sheets (including prognostic)
    type(ESMF_Clock)      , intent(in)    :: clock                ! model clock
    integer               , intent(out)   :: rc

    ! local variables
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_FieldBundle) :: fldbun_noevolve
    type(ESMF_Field)       :: field_noevolve
    type(file_desc_t)      :: pioid
    type(io_desc_t)        :: pio_iodesc
    type(var_desc_t)       :: varid
    real(r8), pointer      :: topog(:), thck(:)
    integer                :: ns, ng, lsize, ndims, rcode
    integer , allocatable  :: dimid(:)
    real(r8)               :: rhoi      ! density of ice ~ kg/m^3
    real(r8)               :: rhoo      ! density of sea water ~ kg/m^3
    real(r8)               :: eus       ! eustatic sea level
    real(r8)               :: lsrf      ! lower surface elevation (m) on ice grid
    real(r8)               :: usrf      ! upper surface elevation (m) on ice grid
    logical                :: exists
    character(len=*), parameter :: subname = '(glc_noevolve_mod:noevolve_init) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Set module variables
    num_icesheets_total = size(NStateExp)

    allocate(icesheet_info(num_icesheets_total))

    allocate(Sg_area(num_icesheets_total))
    allocate(Sg_topo(num_icesheets_total))
    allocate(Sg_ice_covered(num_icesheets_total))
    allocate(Sg_icemask(num_icesheets_total))
    allocate(Sg_icemask_coupled_fluxes(num_icesheets_total))
    allocate(Fgrg_rofi(num_icesheets_total))
    allocate(Flgl_qice(num_icesheets_total))

    ! Get the GLC PIO iosystem from the shared  PIO initialization
    pio_subsystem => shr_pio_getiosys('GLC')
    pio_io_type = shr_pio_getiotype('GLC')

    ! Loop over ice sheets and initialize only those that are noevolve
    icesheet_loop: do ns = 1, num_icesheets_total

       icesheet_info(ns)%nx = noevolve_global_nx(ns) 
       icesheet_info(ns)%ny = noevolve_global_ny(ns) 
       icesheet_info(ns)%mode = icesheet_modes(ns) 
       icesheet_info(ns)%name = icesheet_names_total(ns) 

       !--- Skip this ice sheet if it is prognostic ---
       if (trim(icesheet_info(ns)%mode) /= 'noevolve') cycle

       !--- Grab pointers into the ESMF export fields ---
       call dshr_state_getfldptr(NStateExp(ns), field_out_area, &
            fldptr1=Sg_area(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_state_getfldptr(NStateExp(ns), field_out_topo, &
            fldptr1=Sg_topo(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_state_getfldptr(NStateExp(ns), field_out_ice_covered, &
            fldptr1=Sg_ice_covered(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_state_getfldptr(NStateExp(ns), field_out_icemask, &
            fldptr1=Sg_icemask(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_state_getfldptr(NStateExp(ns), field_out_icemask_coupled_fluxes, &
            fldptr1=Sg_icemask_coupled_fluxes(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_state_getfldptr(NStateExp(ns), field_out_rofi_to_ocn, &
            fldptr1=Fgrg_rofi(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       !--- Grab pointer into the SMB import field ---
       if (.not. NUOPC_IsConnected(NStateImp(ns), fieldName=field_in_qice)) then
          call shr_log_error(subname//': '//field_in_qice// &
               ' must be connected for noevolve ice sheet', rc=rc)
          return
       end if
       call dshr_state_getfldptr(NStateImp(ns), field_in_qice, &
            fldptr1=Flgl_qice(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       Flgl_qice(ns)%ptr(:) = 0._r8

       !--- Determine local size and global index from mesh ---
       call ESMF_MeshGet(meshes(ns), elementDistGrid=distgrid, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_DistGridGet(distgrid, localDe=0, elementCount=lsize, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       allocate(icesheet_info(ns)%gindex(lsize))
       call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=icesheet_info(ns)%gindex, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       !--- Cell area (radians^2, constant) ---
       !    Computed from the user-specified internal grid spacing (matches the
       !    dglc datamode_noevolve convention).
       !    SHR_CONST_REARTH is the radius of earth in m
       !    noevolve_internal_gridsize is the internal model gridsize in m
       do ng = 1, lsize
          Sg_area(ns)%ptr(ng) = (noevolve_internal_gridsize(ns) / SHR_CONST_REARTH)**2
       end do

       !--- Build field bundle to hold topg and thk from file ---
       fldbun_noevolve = ESMF_FieldBundleCreate(rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! "ice thickness" ;
       field_noevolve = ESMF_FieldCreate(meshes(ns), ESMF_TYPEKIND_R8, &
            name='thk', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! "bed topography" ;
       field_noevolve = ESMF_FieldCreate(meshes(ns), ESMF_TYPEKIND_R8, &
            name='topg', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       !--- Open data file, set up PIO decomposition, read topg and thk ---
       inquire(file=trim(noevolve_datafiles(ns)), exist=exists)
       if (.not.exists) then
          call shr_sys_abort(' ERROR: model input file '//trim(noevolve_datafiles(ns))//' does not exist', rc=rc)
       else
          if (my_task == master_task) then
             write(stdout,'(a,a)')' opening file ',trim(noevolve_datafiles(ns))
          end if
       end if
       rcode = pio_openfile(pio_subsystem, pioid, pio_io_type, trim(noevolve_datafiles(ns)), PIO_NOWRITE)
       call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
       rcode = pio_inq_varid(pioid, 'thk', varid)
       rcode = pio_inq_varndims(pioid, varid, ndims)
       allocate(dimid(ndims))
       rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
       deallocate(dimid)
       call pio_initdecomp(pio_subsystem, pio_double, (/icesheet_info(ns)%nx, icesheet_info(ns)%ny/), &
            icesheet_info(ns)%gindex, pio_iodesc)

       ! Read in the data into the appropriate field bundle pointers
       ! Note that Sg_ice_covered(ns)%ptr points into the data for
       ! the Sg_ice_covered field in NStateExp(ns)
       ! Note that Sg_topo(ns)%ptr points into the data for
       ! the Sg_topon NStateExp(ns)
       ! Note that topog is bedrock topography

       call dshr_fldbun_getFldPtr(fldbun_noevolve, 'topg', topog, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       rcode = pio_inq_varid(pioid, 'topg', varid)
       call pio_read_darray(pioid, varid, pio_iodesc, topog, rcode)

       call dshr_fldbun_getFldPtr(fldbun_noevolve, 'thk', thck, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       rcode = pio_inq_varid(pioid, 'thk', varid)
       call pio_read_darray(pioid, varid, pio_iodesc, thck, rcode)

       call pio_closefile(pioid)
       call pio_freedecomp(pio_subsystem, pio_iodesc)

       !--- Compute static mask / topo fields from topg and thk ---
       rhoi = SHR_CONST_RHOICE   ! 0.917e3
       rhoo = SHR_CONST_RHOSW    ! 1.026e3
       eus = 0
       do ng = 1, lsize
          if (topog(ng) - eus < (-rhoi/rhoo) * thck(ng)) then
             lsrf = (-rhoi/rhoo) * thck(ng)
          else
             lsrf = topog(ng)
          end if
          usrf = max(0._r8, thck(ng) + lsrf)

          ! The export field 'ice_mask_coupled_fluxes' determines who is handling the
          ! runoff associated with the surface mass balance
          ! If its 0 -then ctsm needs to handle it.
          ! Since we want CISM noevolve to handle it noevolve mode - then
          ! ice_mask_coupled_fluxes to be identical to the mask
          
          if (is_in_active_grid(usrf)) then
             Sg_icemask(ns)%ptr(ng) = 1.d0
             Sg_icemask_coupled_fluxes(ns)%ptr(ng) = 1.d0
             if (is_ice_covered(thck(ng))) then
                Sg_ice_covered(ns)%ptr(ng) = 1.d0
             else
                Sg_ice_covered(ns)%ptr(ng) = 0.d0
             end if
             ! Note that we use the same method for computing topo whether this point is
             ! ice-covered or ice-free. This is in contrast to the method for computing
             ! ice-free topo in glint_upscaling_gcm.
             Sg_topo(ns)%ptr(ng) = thk0 * usrf
          else
             ! Note that this logic implies that if (in theory) we had an ice-covered
             ! point outside the "active grid", it will get classified as ice-free for
             ! these purposes. This mimics the logic currently in glint_upscaling_gcm.
             Sg_icemask(ns)%ptr(ng) = 0.d0
             Sg_icemask_coupled_fluxes(ns)%ptr(ng) = 0.d0
             Sg_ice_covered(ns)%ptr(ng) = 0.d0
             Sg_topo(ns)%ptr(ng) = 0.d0
          end if
       end do

       ! Set scalars in export state
       call State_SetScalar(dble(icesheet_info(ns)%nx), flds_scalar_index_nx, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call State_SetScalar(dble(icesheet_info(ns)%ny), flds_scalar_index_ny, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize Fgrg_rofi
       if (runtype == 'continue') then
          call glc_noevolve_restart_read(ns, clock, rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       else
          Fgrg_rofi(ns)%ptr(:) = 0._r8
       end if

       if (my_task == master_task) then
          write(stdout,'(a,i4)') subname//' finished initialization for noevolve ice sheet '// int_to_str(ns)
       end if

    end do icesheet_loop

    ! Zero-fill the CISM-specific export fields (heat flux, runoff, etc.)
    call glc_noevolve_zero_cism_fields(NStateExp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine glc_noevolve_init

  !===============================================================================

  subroutine glc_noevolve_advance(rc)

    !---------------------------------------------------------------------------
    ! Compute Fgrg_rofi for each noevolve ice sheet from the imported SMB
    ! (Flgl_qice), conserving total ice mass by redistributing negative fluxes
    ! across positive-SMB cells.  Called each coupling step.
    !---------------------------------------------------------------------------

    ! input/output variables
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM) :: vm
    integer       :: ns, ng, lsize
    real(r8)      :: loc_pos_smb(1), Tot_pos_smb(1) ! Sum of positive smb values on each ice sheet for hole-filling
    real(r8)      :: loc_neg_smb(1), Tot_neg_smb(1) ! Sum of negative smb values on each ice sheet for hole-filling
    real(r8)      :: rat     ! Ratio of hole-filling flux to apply
    character(len=*), parameter :: subname = '(glc_noevolve_mod:noevolve_advance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    icesheet_loop: do ns = 1, num_icesheets_total

       if (trim(icesheet_info(ns)%mode) /= 'noevolve') cycle

       ! Get number of grid cells per ice sheet
       lsize = size(Fgrg_rofi(ns)%ptr)

       ! reset output variables to zero
       Fgrg_rofi(ns)%ptr(:) = 0._r8
       loc_pos_smb(1) = 0.d0
       Tot_pos_smb(1) = 0.d0
       loc_neg_smb(1) = 0.d0
       Tot_neg_smb(1) = 0.d0
       rat = 0.d0

       ! For No Evolve to reduce negative ice fluxes, we will
       ! Calculate the total positive and total negative fluxes on each
       ! processor first (local totals).
       do ng = 1,lsize
          if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
             if(Flgl_qice(ns)%ptr(ng) > 0.d0) then
                loc_pos_smb(1) = loc_pos_smb(1)+Flgl_qice(ns)%ptr(ng)*Sg_area(ns)%ptr(ng)
             end if
             ! Ignore places that are exactly 0.d0
             if(Flgl_qice(ns)%ptr(ng) < 0.d0) then
                loc_neg_smb(1) = loc_neg_smb(1)+Flgl_qice(ns)%ptr(ng)*Sg_area(ns)%ptr(ng)
             end if
          end if
       end do

       ! Now do two global sums to get the ice sheet total positive
       ! and negative ice fluxes
       call ESMF_VMAllreduce(vm, senddata=loc_pos_smb, recvdata=Tot_pos_smb, count=1, &
            reduceflag=ESMF_REDUCE_SUM, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMAllreduce(vm, senddata=loc_neg_smb, recvdata=Tot_neg_smb, count=1, &
            reduceflag=ESMF_REDUCE_SUM, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! If there's more positive than negative, then set all
       ! negative to zero and destribute the negative flux amount
       ! across the positive values, scaled by the size of the
       ! positive value. This section also applies to any chunks
       ! where there is no negative smb. In that case the ice
       ! runoff is exactly equal to the input smb.
       if(abs(Tot_pos_smb(1)) >= abs(Tot_neg_smb(1))) then
          do ng = 1,lsize
             if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
                if(Flgl_qice(ns)%ptr(ng) > 0.d0) then
                   rat = Flgl_qice(ns)%ptr(ng)/Tot_pos_smb(1)
                   Fgrg_rofi(ns)%ptr(ng) = Flgl_qice(ns)%ptr(ng) + rat*Tot_neg_smb(1)
                else
                   Fgrg_rofi(ns)%ptr(ng) = 0.d0
                end if
             else
                Fgrg_rofi(ns)%ptr(ng) = 0.d0
             end if
          end do
       else
          ! If there's more negative than positive, set the positive to zero
          ! and distribute the positive amount to the negative spaces to
          ! reduce their negativity a bit. This shouldn't happen often.
          ! This section of code also applies if Tot_pos_smb is zero.
          do ng = 1,lsize
             if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
                if(Flgl_qice(ns)%ptr(ng) < 0.d0) then
                   rat = Flgl_qice(ns)%ptr(ng)/Tot_neg_smb(1)
                   Fgrg_rofi(ns)%ptr(ng) = Flgl_qice(ns)%ptr(ng) + rat*Tot_pos_smb(1)
                else
                   Fgrg_rofi(ns)%ptr(ng) = 0.d0
                end if
             else
                Fgrg_rofi(ns)%ptr(ng) = 0.d0
             end if
          end do

       end if ! More neg or pos smb

    end do icesheet_loop

  end subroutine glc_noevolve_advance

  !===============================================================================

  subroutine glc_noevolve_zero_cism_fields(NStateExp, rc)

    !---------------------------------------------------------------------------
    ! Zero-fill the CISM-specific export fields (heat flux, ice->seaice runoff,
    ! for all noevolve ice sheets.
    ! Called once during InitializeRealize.
    !---------------------------------------------------------------------------

    ! input/output variables
    type(ESMF_State), intent(inout) :: NStateExp(:)     ! total number if ice sheets
    integer         , intent(out)   :: rc

    ! local variables
    integer :: ns                ! ice sheet counter
    real(r8), pointer :: ptr(:)  ! pointer into NStateExp
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    icesheet_loop: do ns = 1, num_icesheets_total

       if (trim(icesheet_info(ns)%mode) /= 'noevolve') cycle

       call dshr_state_getfldptr(NStateExp(ns), field_out_hflx_to_lnd, fldptr1=ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       ptr(:) = 0._r8

       ! call dshr_state_getfldptr(NStateExp(ns), field_out_rofi_to_ice, fldptr1=ptr, rc=rc)
       ! if (chkerr(rc,__LINE__,u_FILE_u)) return
       ! ptr(:) = 0._r8

       call dshr_state_getfldptr(NStateExp(ns), field_out_rofl_to_ocn, fldptr1=ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       ptr(:) = 0._r8

    end do icesheet_loop

  end subroutine glc_noevolve_zero_cism_fields

   !===============================================================================
   logical function is_in_active_grid(usrf)
      ! Return true if the given point is inside the "active grid". The active grid includes
      ! any point that can receive a positive surface mass balance, which includes any
      ! point classified as land or ice sheet.

      real(r8), intent(in) :: usrf  ! surface elevation (m)

      if (thk0 * usrf > 0.d0) then
         ! points not at sea level are assumed to be land or ice sheet
         is_in_active_grid = .true.
      else
         is_in_active_grid = .false.
      end if
   end function is_in_active_grid

   !===============================================================================
   logical function is_ice_covered(thck)
      ! Return true if the given point is ice-covered

      real(r8), intent(in) :: thck     ! ice thickness (m)
      real(r8), parameter :: min_thck = 0.d0

      if (thk0 * thck > min_thck) then
         is_ice_covered = .true.
      else
         is_ice_covered = .false.
      end if
   end function is_ice_covered

  !===============================================================================
  subroutine glc_noevolve_restart_write(icesheet_index, clock, rc)

    ! input/output variables
    integer          , intent(in)    :: icesheet_index
    type(ESMF_Clock) , intent(in)    :: clock
    integer          , intent(out)   :: rc

    ! local variables
    integer             :: ymd       ! model date
    integer             :: tod       ! model sec
    integer             :: yr        ! model year
    integer             :: mon       ! model month
    integer             :: day       ! model day
    character(len=CL)   :: rest_file
    type(file_desc_t)   :: pioid
    integer             :: dimid2(2)
    integer             :: oldmode
    integer             :: rcode
    type(ESMF_Time)     :: CurrentTime
    type(var_desc_t)    :: varid
    type(io_desc_t)     :: pio_iodesc
    integer             :: ptr_unit
    integer             :: ns
    character(len=*), parameter :: subname = '(glc_noevolve_mod:noevolve_restart_write) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ns = icesheet_index

    ! determine restart filename
    call ESMF_ClockGet(clock, currTime=CurrentTime, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: "//subname)
    call ESMF_TimeGet( CurrentTime, yy=yr, mm=mon, dd=day, s=tod, rc=rc )
    if ( rc /= ESMF_SUCCESS ) call shr_sys_abort("ERROR: "//subname)
    call shr_cal_ymd2date(yr, mon, day, ymd)

    rest_file = glc_filename(icesheet_info(ns)%name, yr, mon, day, tod, 'restart')
    if (my_task == master_task) then
       write(stdout,'(a)') subname//' writing noevolve restart file '//trim(rest_file)
    endif

    ! write data model restart data
    rcode = pio_createfile(pio_subsystem, pioid, pio_io_type, trim(rest_file), pio_clobber)
    rcode = pio_def_dim(pioid, '_nx', icesheet_info(ns)%nx, dimid2(1))
    rcode = pio_def_dim(pioid, '_ny', icesheet_info(ns)%ny, dimid2(2))
    rcode = pio_def_var(pioid, 'flgl_rofi', PIO_DOUBLE, (/dimid2/), varid)
    rcode = pio_put_att(pioid, varid, "_FillValue", shr_const_spval)
    rcode = pio_set_fill(pioid, PIO_FILL, oldmode)
    rcode = pio_enddef(pioid)

    call pio_initdecomp(pio_subsystem, pio_double, &
         (/icesheet_info(ns)%nx,icesheet_info(ns)%ny/), icesheet_info(ns)%gindex, pio_iodesc)
    call pio_write_darray(pioid, varid, pio_iodesc, Fgrg_rofi(ns)%ptr, rcode, fillval=shr_const_spval)
    call pio_closefile(pioid)
    call pio_freedecomp(pio_subsystem, pio_iodesc)

    ! write pointer to restart file
    if (my_task == master_task) then
       open(newunit=ptr_unit, file=get_rpointer_filename(icesheet_info(ns)%name, yr, mon, day, tod, .false.))
       write(ptr_unit,'(a)') trim(rest_file)
       close(ptr_unit)
    endif

  end subroutine glc_noevolve_restart_write

  !===============================================================================
  subroutine glc_noevolve_restart_read(icesheet_index, clock, rc)

    ! input/output arguments
    integer          , intent(in)  :: icesheet_index
    type(ESMF_Clock) , intent(in)  :: clock
    integer          , intent(out) :: rc

    ! local variables
    integer           :: ns
    integer           :: yr
    integer           :: mon
    integer           :: day
    integer           :: tod
    type(ESMF_Time)   :: currtime
    type(ESMF_VM)     :: vm
    type(file_desc_t) :: pioid
    type(var_desc_t)  :: varid
    type(io_desc_t)   :: pio_iodesc
    integer           :: rcode
    logical           :: exists  ! file existance
    integer           :: ptr_unit
    character(len=CL) :: restfile
    character(len=CL) :: restfile0
    character(len=CL) :: pointerfile
    character(len=*), parameter :: subname = '(glc_noevolve_mod:noevolve_restart_read) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ns = icesheet_index

    ! Get restart filename from rpointer file
    call ESMF_ClockGet(clock, currtime=currtime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currtime, yy=yr, mm=mon, dd=day, s=tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (my_task == master_task) then
       pointerfile = get_rpointer_filename(icesheet_info(ns)%name, yr, mon, day, tod, .true.)
       open(newunit=ptr_unit, file=pointerfile)
       read(ptr_unit,'(a)') restfile
       close(ptr_unit)
       inquire(file=trim(restfile), exist=exists)
       if (.not. exists) then
          call shr_sys_abort(' ERROR: restart file '//trim(restfile)//' does not exist', rc=rc)
       end if
    end if
    call ESMF_VMBroadCast(vm, restfile, CL, master_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Read restart file
    if (my_task == master_task) then
       write(stdout,'(3a)') subname,' reading noevolve restart file ',trim(restfile)
    end if
    rcode = pio_openfile(pio_subsystem, pioid, pio_io_type, trim(restfile), pio_nowrite)
    call pio_initdecomp(pio_subsystem, pio_double, &
         (/icesheet_info(ns)%nx,icesheet_info(ns)%ny/), icesheet_info(ns)%gindex, pio_iodesc)
    rcode = pio_inq_varid(pioid, 'flgl_rofi', varid)
    call pio_read_darray(pioid, varid, pio_iodesc, Fgrg_rofi(ns)%ptr, rcode)
    call pio_freedecomp(pio_subsystem, pio_iodesc)
    call pio_closefile(pioid)

  end subroutine glc_noevolve_restart_read

  !===============================================================================

  function int_to_str(n) result(s)
    integer, intent(in) :: n
    character(len=10)   :: s
    write(s,'(i0)') n
  end function int_to_str

end module glc_noevolve_mod
