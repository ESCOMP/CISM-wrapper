module glc_import_export

#include "shr_assert.h"

  use ESMF                , only : ESMF_GridComp, ESMF_State, ESMF_Mesh, ESMF_StateGet
  use ESMF                , only : ESMF_KIND_R8, ESMF_SUCCESS, ESMF_MAXSTR, ESMF_LOGMSG_INFO
  use ESMF                , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LogFoundError
  use ESMF                , only : ESMF_STATEITEM_NOTFOUND, ESMF_StateItem_Flag
  use ESMF                , only : ESMF_LogFoundAllocError, ESMF_MeshIsCreated
  use ESMF                , only : operator(/=), operator(==)
  use NUOPC               , only : NUOPC_CompAttributeGet, NUOPC_Advertise, NUOPC_IsConnected
  use NUOPC               , only : NUOPC_AddNamespace, NUOPC_AddNestedState
  use NUOPC_Model         , only : NUOPC_ModelGet
  use shr_kind_mod        , only : r8 => shr_kind_r8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod         , only : shr_sys_abort
  use glc_constants       , only : verbose, stdout, stderr, tkfrz, radius, enable_frac_overrides
  use glc_communicate     , only : my_task, master_task
  use glc_time_management , only : iyear,imonth,iday,ihour,iminute,isecond
  use glc_indexing        , only : get_nx_tot, get_ny_tot, get_nx, get_ny, spatial_to_vector, vector_to_spatial
  use glc_fields          , only : ice_sheet
  use glad_main           , only : glad_get_areas
  use nuopc_shr_methods   , only : chkerr, state_setscalar, state_getscalar, state_diagnose

  implicit none
  private ! except

  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields
  public  :: get_num_icesheets

  private :: fldlist_add
  private :: fldlist_realize
  private :: state_getimport
  private :: state_setexport
  private :: state_getfldptr

  character(len=CL) :: flds_scalar_name = ''
  integer           :: flds_scalar_num = 0
  integer           :: flds_scalar_index_nx = 0
  integer           :: flds_scalar_index_ny = 0

  type fld_list_type
     character(len=128) :: stdname
     integer :: ungridded_lbound = 0
     integer :: ungridded_ubound = 0
  end type fld_list_type

  ! Field names
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

  integer, parameter :: nlev_import = 30
  real(r8) :: vertical_levels(nlev_import) = (/  &
       30., 90., 150., 210., 270., 330., 390., 450., 510., 570., &
       630., 690., 750., 810., 870., 930., 990., 1050., 1110., 1170., &
       1230., 1290., 1350., 1410., 1470., 1530., 1590., 1650., 1710., 1770. /)

  integer, parameter     :: fldsMax = 100
  integer                :: fldsToGlc_num = 0
  integer                :: fldsFrGlc_num = 0
  type (fld_list_type)   :: fldsToGlc(fldsMax)
  type (fld_list_type)   :: fldsFrGlc(fldsMax)

  type(ESMF_State), allocatable :: NStateImp(:)
  type(ESMF_State), allocatable :: NStateExp(:)
  integer            :: num_icesheets
  integer            :: dbug_flag = 0

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine advertise_fields(gcomp, cism_evolve, num_icesheets_in, rc)

    use glc_constants, only : glc_smb

    ! input/output variables
    type(ESMF_GridComp)            :: gcomp
    logical          , intent(in)  :: cism_evolve
    integer          , intent(in)  :: num_icesheets_in
    integer          , intent(out) :: rc

    ! local variables
    type(ESMF_State)    :: importState
    type(ESMF_State)    :: exportState
    integer             :: nf,ns
    integer             :: stat
    character(len=CS)   :: cnum
    character(len=CL)   :: cvalue
    character(len=CL)   :: logmsg
    logical             :: isPresent, isSet
    character(len=*), parameter :: subname='(glc_import_export:advertise_fields)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    num_icesheets = num_icesheets_in

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (chkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       flds_scalar_name = trim(cvalue)
       call ESMF_LogWrite(trim(subname)//' flds_scalar_name = '//trim(flds_scalar_name), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call shr_sys_abort(subname//'Need to set attribute ScalarFieldName')
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldCount", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue, *) flds_scalar_num
       write(logmsg,*) flds_scalar_num
       call ESMF_LogWrite(trim(subname)//' flds_scalar_num = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call shr_sys_abort(subname//'Need to set attribute ScalarFieldCount')
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNX", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) flds_scalar_index_nx
       write(logmsg,*) flds_scalar_index_nx
       call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_nx = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call shr_sys_abort(subname//'Need to set attribute ScalarFieldIdxGridNX')
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNY", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) flds_scalar_index_ny
       write(logmsg,*) flds_scalar_index_ny
       call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_ny = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call shr_sys_abort(subname//'Need to set attribute ScalarFieldIdxGridNY')
    endif

    !--------------------------------
    ! Create nested state for active ice sheets only
    !--------------------------------

    allocate(NStateImp(num_icesheets))
    allocate(NStateExp(num_icesheets))

    do ns = 1,num_icesheets
       write(cnum,'(i0)') ns
       call NUOPC_AddNestedState(importState, CplSet="GLC"//trim(cnum), nestedState=NStateImp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_AddNestedState(exportState, CplSet="GLC"//trim(cnum), nestedState=NStateExp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    !--------------------------------
    ! Advertise export fields
    !--------------------------------

    call fldlist_add(fldsFrGlc_num, fldsFrglc, trim(flds_scalar_name))

    ! area is constant in time, so if it were easy to just send this during
    ! initialization, we could do that - but currently, for ease of implementation, we
    ! resend it every coupling interval
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_area)

    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_ice_covered)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_topo)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_icemask)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_icemask_coupled_fluxes)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_hflx_to_lnd)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_rofi_to_ice)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_rofi_to_ocn)
    call fldlist_add(fldsFrGlc_num, fldsFrglc, field_out_rofl_to_ocn)

    ! Now advertise above export fields
    do ns = 1,num_icesheets
       do nf = 1,fldsFrGlc_num
          call NUOPC_Advertise(NStateExp(ns), standardName=fldsFrGlc(nf)%stdname, &
               TransferOfferGeomObject='will provide', rc=rc)
          if (chkErr(rc,__LINE__,u_FILE_u)) return
          if (my_task == master_task) then
             write(cnum,'(i0)') ns
             write(stdout,'(a)') 'Advertised export field: '//trim(fldsFrGlc(nf)%stdname)//' for ice sheet '//trim(cnum)
          end if
          call ESMF_LogWrite(subname//'Export field'//': '//trim(fldsFrGlc(nf)%stdname), ESMF_LOGMSG_INFO)
       end do
    enddo

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

    call fldlist_add(fldsToGlc_num, fldsToGlc, trim(flds_scalar_name))
    call fldlist_add(fldsToGlc_num, fldsToGlc, field_in_tsrf)
    call fldlist_add(fldsToGlc_num, fldsToGlc, field_in_qice)
    if (cism_evolve) then
       call fldlist_add(fldsToGlc_num, fldsToGlc, field_in_so_t_depth, ungridded_lbound=1, ungridded_ubound=nlev_import)
       call fldlist_add(fldsToGlc_num, fldsToGlc, field_in_so_s_depth, ungridded_lbound=1, ungridded_ubound=nlev_import)
     end if

     ! Now advertise import fields
     do ns = 1,num_icesheets
       do nf = 1,fldsToGlc_num
         call NUOPC_Advertise(NStateImp(ns), standardName=fldsToGlc(nf)%stdname, &
              TransferOfferGeomObject='will provide', rc=rc)
         if (chkErr(rc,__LINE__,u_FILE_u)) return
         if (my_task == master_task) then
           write(cnum,'(i0)') ns
           write(stdout,'(a)') 'Advertised import field: '//trim(fldsToGlc(nf)%stdname)//' for ice sheet '//trim(cnum)
         end if
         call ESMF_LogWrite(subname//'Import field'//': '//trim(fldsToGlc(nf)%stdname), ESMF_LOGMSG_INFO)
       end do
     enddo

    ! Set glc_smb
    ! true  => get surface mass balance from land model via coupler (in multiple elev classes)
    ! false => use PDD scheme in GLIMMER
    ! For now, we always use true
    glc_smb = .true.
  end subroutine advertise_fields

  !===============================================================================

  subroutine realize_fields(gcomp, mesh, rc)

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    type(ESMF_Mesh)     , intent(in)    :: mesh(:)
    integer             , intent(out)   :: rc

    ! local variables
    integer           :: ns
    character(len=CS) :: cns
    character(len=*), parameter :: subname='(glc_import_export:realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Realize import and export states for each ice sheet
    do ns = 1,num_icesheets
       write(cns,'(i0)') ns

       call fldlist_realize( &
            state=NStateExp(ns), &
            fldList=fldsFrGlc, &
            numflds=fldsFrGlc_num, &
            flds_scalar_name=flds_scalar_name, &
            flds_scalar_num=flds_scalar_num, &
            tag=subname//':cismExport'//trim(cns),&
            mesh=mesh(ns), rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       call fldlist_realize( &
            state=NStateImp(ns), &
            fldList=fldsToGlc, &
            numflds=fldsToGlc_num, &
            flds_scalar_name=flds_scalar_name, &
            flds_scalar_num=flds_scalar_num, &
            tag=subname//':cismImport'//trim(cns),&
            mesh=mesh(ns), rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call State_diagnose(NStateExp(ns), trim(subname)//':ES',rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    end do

  end subroutine realize_fields

  !===============================================================================

  subroutine import_fields(rc)

    !---------------------------------------------------------------------------
    ! Convert the input data from the mediator to cism
    !---------------------------------------------------------------------------

    use glc_fields, only : cpl_bundles

    ! input/output variabes
    integer, intent(out) :: rc

    ! local variables
    integer :: ns
    character(*), parameter :: subName = "(glc_import_export:import_fields) "
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get cism import fields
    do ns = 1,num_icesheets
       associate(&
            tsfc => cpl_bundles(ns)%tsfc, &
            qsmb => cpl_bundles(ns)%qsmb)

       call state_getimport(NStateImp(ns), field_in_tsrf, tsfc, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_getimport(NStateImp(ns), field_in_qice, qsmb, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       tsfc = tsfc - tkfrz

       !Jer hack fix:
       !For some land points where CLM sees ocean, and all ocean points, CLM doesn't provide a temperature,
       !and so the incoming temperature is 0.d0.  This gets dropped to -273.15, in the above code.  So,
       !manually reverse this, below, to set to 0C.

       where (tsfc < -250.d0) tsfc=0.d0

       if (dbug_flag > 0) then
          call State_diagnose(NStateImp(ns), subname//':ES',rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       end associate
    end do

  end subroutine import_fields

  !===============================================================================

  subroutine export_fields(exportState, rc)

    !---------------------------------------------------------------------------
    ! Convert the cism data to export data to the mediator
    !---------------------------------------------------------------------------

    use glc_fields           , only : cpl_bundles
    use glc_route_ice_runoff , only : route_ice_runoff
    use glc_override_frac    , only : do_frac_overrides

    ! input/output variabes
    type(ESMF_State)     :: exportState
    integer, intent(out) :: rc

    ! local variables
    integer :: nx, ny

    ! if doing frac overrides, these are the modified versions sent to the coupler;
    ! otherwise they point to the real fields
    real(r8), pointer :: ice_covered_to_cpl(:,:)
    real(r8), pointer :: topo_to_cpl(:,:)
    logical           :: fields_to_cpl_allocated  ! whether we allocated the above fields

    ! Note that there are two separate mask fields. Both of them provide information about
    ! where CISM is running. The difference is that ice_sheet_grid_mask includes icesheet
    ! areas that are diagtostic-only, whereas icemask_coupled_fluxes excludes icesheet
    ! areas where we are zeroing the fluxes sent to the coupler (thus, icesheets that are
    ! "diagnostic" in some sense). We need two separate maps, as opposed to a single map
    ! plus a scalar logical variable, in case we're running with multiple icesheet
    ! instances (e.g., Greenland & Antarctica), one of which is fully prognostic and one
    ! of which is diagnostic-only: in that case, ice_sheet_grid_mask would be non-zero
    ! over both Greenland and Antarctica, whereas icemask_coupled_fluxes would be non-zero
    ! over (e.g.)  Greenland, but 0 over Antarctica.

    ! mask of ice sheet grid coverage where we are potentially sending non-zero fluxes
    real(r8), allocatable :: icemask_coupled_fluxes(:,:)

    real(r8), allocatable :: glc_areas(:,:)
    real(r8), allocatable :: hflx_to_cpl(:,:)
    real(r8), allocatable :: rofl_to_cpl(:,:)
    real(r8), allocatable :: rofi_to_ocn(:,:)
    real(r8), allocatable :: rofi_to_ice(:,:)
    integer :: ns
    character(*), parameter :: subName = "(glc_import_export:export_fields) "
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    do ns = 1,num_icesheets
       associate( &
            ice_covered         => cpl_bundles(ns)%ice_covered, &
            topo                => cpl_bundles(ns)%topo, &
            rofi                => cpl_bundles(ns)%rofi, &
            rofl                => cpl_bundles(ns)%rofl, &
            hflx                => cpl_bundles(ns)%hflx, &
            ice_sheet_grid_mask => cpl_bundles(ns)%ice_sheet_grid_mask)

       nx = get_nx(instance_index=ns)
       ny = get_ny(instance_index=ns)

       ! glc_areas are constant in time, so we could just set it once in initialization;
       ! but for simplicity we just reset it each time
       allocate(glc_areas(nx,ny))
       call glad_get_areas(ice_sheet, instance_index = ns, areas=glc_areas)
       glc_areas(:,:) = glc_areas(:,:)/(radius*radius) ! convert from m^2 to radians^2

       ! If overrides of glc fraction are enabled (for testing purposes), then apply
       ! these overrides, otherwise use the real version of ice_covered and topo
       if (enable_frac_overrides) then
          allocate(ice_covered_to_cpl(lbound(ice_covered,1):ubound(ice_covered,1), &
               lbound(ice_covered,2):ubound(ice_covered,2)))
          allocate(topo_to_cpl(lbound(topo,1):ubound(topo,1), &
               lbound(topo,2):ubound(topo,2)))

          ice_covered_to_cpl = ice_covered
          topo_to_cpl = topo
          call do_frac_overrides(ice_covered_to_cpl, topo_to_cpl, ice_sheet_grid_mask)
          fields_to_cpl_allocated = .true.
       else
          ice_covered_to_cpl => ice_covered
          topo_to_cpl => topo
          fields_to_cpl_allocated = .false.
       end if

       allocate(icemask_coupled_fluxes(nx, ny))
       allocate(hflx_to_cpl(nx, ny))
       allocate(rofl_to_cpl(nx, ny))
       allocate(rofi_to_ocn(nx, ny))
       allocate(rofi_to_ice(nx, ny))

       if (ice_sheet%instances(ns)%zero_gcm_fluxes) then
          icemask_coupled_fluxes = 0._r8
          hflx_to_cpl = 0._r8
          rofl_to_cpl = 0._r8
          rofi_to_ocn = 0._r8
          rofi_to_ice = 0._r8
       else
          icemask_coupled_fluxes = ice_sheet_grid_mask
          hflx_to_cpl = hflx
          rofl_to_cpl = rofl
          call route_ice_runoff(rofi, rofi_to_ocn, rofi_to_ice)
       end if

       ! Fill export state for ice sheet

       ! area is constant in time, so if it were easy to just send this during
       ! initialization, we could do that - but currently, for ease of implementation, we
       ! resend it every coupling interval
       call state_setexport(NStateExp(ns), field_out_area, glc_areas, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       call state_setexport(NStateExp(ns), field_out_rofi_to_ocn, rofi_to_ocn, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_rofi_to_ice, rofi_to_ice, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_rofl_to_ocn, rofl_to_cpl, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_ice_covered, ice_covered_to_cpl, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_topo, topo_to_cpl, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_hflx_to_lnd, hflx_to_cpl, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_icemask, ice_sheet_grid_mask, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
       call state_setexport(NStateExp(ns), field_out_icemask_coupled_fluxes, icemask_coupled_fluxes, &
            instance_index=ns, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       ! Set scalars in export state
       call State_SetScalar(dble(get_nx_tot(instance_index=ns)), flds_scalar_index_nx, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call State_SetScalar(dble(get_ny_tot(instance_index=ns)), flds_scalar_index_ny, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Diagnose state data if appropriate
       if (dbug_flag > 1) then
          call State_diagnose(NStateExp(ns), trim(subname)//':ES',rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       deallocate(glc_areas)
       deallocate(icemask_coupled_fluxes)
       deallocate(hflx_to_cpl)
       deallocate(rofl_to_cpl)
       deallocate(rofi_to_ocn)
       deallocate(rofi_to_ice)
       if (fields_to_cpl_allocated) then
          deallocate(ice_covered_to_cpl)
          deallocate(topo_to_cpl)
       end if

       end associate
    end do

  end subroutine export_fields

  !===============================================================================

  integer function get_num_icesheets()
    ! ----------------------------------------------
    ! Return the num_icesheets value stored in this module
    ! ----------------------------------------------
    get_num_icesheets = num_icesheets
  end function get_num_icesheets

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname, ungridded_lbound, ungridded_ubound)

    ! intput/output variables
    integer             ,             intent(inout) :: num
    type(fld_list_type) ,             intent(inout) :: fldlist(:)
    character(len=*)    ,             intent(in)    :: stdname
    integer             , optional  , intent(in)    :: ungridded_lbound
    integer             , optional  , intent(in)    :: ungridded_ubound

    ! local variables
    integer :: rc
    character(len=*), parameter :: subname='(glc_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    ! Set up a list of field information

    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__)
       return
    endif
    fldlist(num)%stdname = trim(stdname)

    if (present(ungridded_lbound) .and. present(ungridded_ubound)) then
       fldlist(num)%ungridded_lbound = ungridded_lbound
       fldlist(num)%ungridded_ubound = ungridded_ubound
    end if

  end subroutine fldlist_add

  !===============================================================================

  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

    use NUOPC , only : NUOPC_IsConnected, NUOPC_Realize
    use ESMF  , only : ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_TYPEKIND_R8
    use ESMF  , only : ESMF_MAXSTR, ESMF_Field, ESMF_State, ESMF_Mesh, ESMF_StateRemove
    use ESMF  , only : ESMF_LogFoundError, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF  , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LOGERR_PASSTHRU

    ! intput/output variables
    type(ESMF_State)    , intent(inout) :: state
    type(fld_list_type) , intent(in)    :: fldList(:)
    integer             , intent(in)    :: numflds
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    character(len=*)    , intent(in)    :: tag
    type(ESMF_Mesh)     , intent(in)    :: mesh
    integer             , intent(inout) :: rc

    ! local variables
    integer                :: n
    type(ESMF_Field)       :: field
    character(len=80)      :: stdname
    character(CL)          :: msg
    character(len=*),parameter  :: subname='(glc_import_export:fldlist_realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1, numflds
       stdname = fldList(n)%stdname
       if (NUOPC_IsConnected(state, fieldName=stdname)) then
          if (stdname == trim(flds_scalar_name)) then
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe", &
                  ESMF_LOGMSG_INFO)
             ! Create the scalar field
             call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe", &
                  ESMF_LOGMSG_INFO)
             if (my_task == master_task) then
                write(stdout,'(a)') trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe only"
             end if
          else
             ! Create the field
             if (fldlist(n)%ungridded_lbound > 0 .and. fldlist(n)%ungridded_ubound > 0) then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, &
                     ungriddedLbound=(/fldlist(n)%ungridded_lbound/), &
                     ungriddedUbound=(/fldlist(n)%ungridded_ubound/), &
                     gridToFieldMap=(/2/), rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
                if (my_task == master_task) then
                   write(stdout,'(a,i8,a,i8)') trim(subname)// trim(tag)//" Field = "//trim(stdname)// &
                        " is connected using mesh with lbound ", fldlist(n)%ungridded_lbound,&
                        " and with ubound ",fldlist(n)%ungridded_ubound
                end if
             else
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
                if (my_task == master_task) then
                   write(stdout,'(a)') trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected using mesh"
                end if
             end if
          endif

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       else
          if (stdname /= trim(flds_scalar_name)) then
             call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(stdname) // " is not connected.", &
                  ESMF_LOGMSG_INFO)
             call ESMF_StateRemove(state, (/stdname/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
             if (my_task == master_task) then
                write(stdout,'(a)') trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is not connected "
             end if
          end if
       end if
    end do

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine SetScalarField(field, flds_scalar_name, flds_scalar_num, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------
      use ESMF, only : ESMF_Field, ESMF_DistGrid, ESMF_Grid
      use ESMF, only : ESMF_DistGridCreate, ESMF_GridCreate, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
      use ESMF, only : ESMF_FieldCreate, ESMF_GridCreate, ESMF_TYPEKIND_R8

      type(ESMF_Field) , intent(inout) :: field
      character(len=*) , intent(in)    :: flds_scalar_name
      integer          , intent(in)    :: flds_scalar_num
      integer          , intent(inout) :: rc

      ! local variables
      type(ESMF_Distgrid) :: distgrid
      type(ESMF_Grid)     :: grid
      character(len=*), parameter :: subname='(glc_import_export:SetScalarField)'
      ! ----------------------------------------------

      rc = ESMF_SUCCESS

      ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
      distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      field = ESMF_FieldCreate(name=trim(flds_scalar_name), grid=grid, typekind=ESMF_TYPEKIND_R8, &
           ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), gridToFieldMap=(/2/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    end subroutine SetScalarField

  end subroutine fldlist_realize

  !===============================================================================

  subroutine state_getimport(state, fldname, output, instance_index, rc)

    ! ----------------------------------------------
    ! Map import state field to output array
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)    , intent(in)    :: state
    character(len=*)    , intent(in)    :: fldname
    real(r8)            , intent(out)   :: output(:,:)
    integer             , intent(in)    :: instance_index  ! index of current ice sheet
    integer             , intent(out)   :: rc

    ! local variables
    real(R8), pointer         :: fldptr(:)
    type(ESMF_StateItem_Flag) :: itemFlag
    character(len=*), parameter :: subname='(glc_import_export:state_getimport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (chkErr(rc,__LINE__,u_FILE_u)) return

    ! If field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then
       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr, rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       ! determine output array
       call vector_to_spatial(instance_index, fldptr, output)
    end if

  end subroutine state_getimport

  !===============================================================================

  subroutine state_setexport(state, fldname, input, instance_index, rc)

    ! ----------------------------------------------
    ! Map input array to export state field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)    , intent(inout) :: state
    character(len=*)    , intent(in)    :: fldname
    real(r8)            , intent(in)    :: input(:,:)
    integer             , intent(in)    :: instance_index  ! index of current ice sheet
    integer             , intent(out)   :: rc

    ! local variables
    real(R8), pointer         :: fldptr(:)
    type(ESMF_StateItem_Flag) :: itemFlag
    character(len=*), parameter :: subname='(glc_import_export:state_setexport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (chkErr(rc,__LINE__,u_FILE_u)) return

    ! if field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then
       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr, rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       ! set fldptr values to input array
       call spatial_to_vector(instance_index, input, fldptr)
    end if

  end subroutine state_setexport

  !===============================================================================

  subroutine state_getfldptr(State, fldname, fldptr, rc)

    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    use ESMF , only : ESMF_State, ESMF_Field, ESMF_Mesh, ESMF_FieldStatus_Flag
    use ESMF , only : ESMF_StateGet, ESMF_FieldGet, ESMF_MeshGet
    use ESMF , only : ESMF_FIELDSTATUS_COMPLETE, ESMF_FAILURE

    ! input/output variables
    type(ESMF_State),  intent(in)    :: State
    character(len=*),  intent(in)    :: fldname
    real(R8), pointer, intent(out)   :: fldptr(:)
    integer,           intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: nnodes, nelements
    character(len=*), parameter :: subname='(glc_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (chkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (chkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
       rc = ESMF_FAILURE
       return
    else
       call ESMF_FieldGet(lfield, mesh=lmesh, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_MeshGet(lmesh, numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return

       if (nnodes == 0 .and. nelements == 0) then
          call ESMF_LogWrite(trim(subname)//": no local nodes or elements ", ESMF_LOGMSG_INFO)
          rc = ESMF_FAILURE
          return
       end if

       call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
       if (chkErr(rc,__LINE__,u_FILE_u)) return
    endif  ! status

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine state_getfldptr

end module glc_import_export
