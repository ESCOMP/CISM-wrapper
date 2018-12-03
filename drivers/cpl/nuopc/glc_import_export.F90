module glc_import_export

#include "shr_assert.h"

  use ESMF                , only : ESMF_GridComp, ESMF_State, ESMF_Mesh, ESMF_StateGet
  use ESMF                , only : ESMF_KIND_R8, ESMF_SUCCESS, ESMF_MAXSTR, ESMF_LOGMSG_INFO
  use ESMF                , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LogFoundError
  use ESMF                , only : ESMF_STATEITEM_NOTFOUND, ESMF_StateItem_Flag
  use ESMF                , only : operator(/=), operator(==)
  use NUOPC               , only : NUOPC_CompAttributeGet, NUOPC_Advertise, NUOPC_IsConnected
  use NUOPC_Model         , only : NUOPC_ModelGet
  use glc_constants       , only : verbose, stdout, stderr, tkfrz, zero_gcm_fluxes
  use glc_communicate     , only : my_task, master_task
  use glc_time_management , only : iyear,imonth,iday,ihour,iminute,isecond,runtype
  use med_constants_mod   , only : R8, CS

  implicit none
  private ! except

  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields

  private :: fldlist_add
  private :: fldlist_realize
  private :: state_getimport
  private :: state_setexport
  private :: state_getfldptr

  type fld_list_type
     character(len=128) :: stdname
  end type fld_list_type

  integer, parameter     :: fldsMax = 100
  integer                :: fldsToGlc_num = 0
  integer                :: fldsFrGlc_num = 0
  type (fld_list_type)   :: fldsToGlc(fldsMax)
  type (fld_list_type)   :: fldsFrGlc(fldsMax)

  integer :: debug_export = 1
  integer :: debug_import = 1
  character(*),parameter :: F01 = "('(glc_import): ',a,2(i8,2x),i8,2x,d21.6)"
  character(*),parameter :: F02 = "('(glc_export): ',a,2(i8,2x),i8,2x,d21.6)"

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine advertise_fields(gcomp, rc)

    ! uses
    use glc_constants         , only : glc_smb
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr
    use shr_nuopc_scalars_mod , only : flds_scalar_name

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)       :: importState
    type(ESMF_State)       :: exportState
    integer                :: dbrc
    integer                :: n
    character(len=*), parameter :: subname='(glc_import_export:advertise_fields)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Advertise export fields
    !--------------------------------

    call fldlist_add(fldsFrGlc_num, fldsFrglc, trim(flds_scalar_name))

    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Sg_ice_covered')           
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Sg_topo')                  
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Sg_icemask')               
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Sg_icemask_coupled_fluxes')
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Flgg_hflx')                
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Figg_rofi')                
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Fogg_rofi')                
    call fldlist_add(fldsFrGlc_num, fldsFrglc, 'Fogg_rofl')                

    ! Now advertise above export fields
    do n = 1,fldsFrGlc_num
       call NUOPC_Advertise(exportState, standardName=fldsFrGlc(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

    call fldlist_add(fldsToGlc_num, fldsToGlc, trim(flds_scalar_name))

    call fldlist_add(fldsToGlc_num, fldsToGlc, 'Sl_tsrf')  
    call fldlist_add(fldsToGlc_num, fldsToGlc, 'Flgl_qice')

    ! Now advertise import fields
    do n = 1,fldsToGlc_num
       call NUOPC_Advertise(importState, standardName=fldsToGlc(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    ! Set glc_smb
    ! true  => get surface mass balance from land model via coupler (in multiple elev classes)
    ! false => use PDD scheme in GLIMMER
    ! For now, we always use true

    glc_smb = .true.

  end subroutine advertise_fields

  !===============================================================================

  subroutine realize_fields(gcomp, Emesh, rc)

    ! uses
    use shr_nuopc_scalars_mod , only : flds_scalar_name, flds_scalar_num
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_Mesh)      :: Emesh
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    integer              :: dbrc
    character(len=*), parameter :: subname='(glc_import_export:realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=ExportState, &
         fldList=fldsFrGlc, &
         numflds=fldsFrGlc_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':cismExport',&
         mesh=Emesh, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=importState, &
         fldList=fldsToGlc, &
         numflds=fldsToGlc_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':cismImport',&
         mesh=Emesh, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine realize_fields

  !===============================================================================

  subroutine import_fields( importState, rc)

    !---------------------------------------------------------------------------
    ! Convert the input data from the mediator to cism
    !---------------------------------------------------------------------------

    ! uses
    use glc_fields            , only : tsfc, qsmb 
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variabes
    type(ESMF_State)     :: importState 
    integer, intent(out) :: rc

    ! local variables
    integer           :: dbrc
    character(*), parameter :: subName = "(glc_import_export:import_fields) "
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! Get cism import fields
    call state_getimport(importState, 'Sl_tsrf', tsfc, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_getimport(importState, 'Flgl_qice', qsmb, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    tsfc = tsfc - tkfrz

    !Jer hack fix: 
    !For some land points where CLM sees ocean, and all ocean points, CLM doesn't provide a temperature,
    !and so the incoming temperature is 0.d0.  This gets dropped to -273.15, in the above code.  So,
    !manually reverse this, below, to set to 0C.

    where (tsfc < -250.d0) tsfc=0.d0 

  end subroutine import_fields

  !===============================================================================

  subroutine export_fields( exportState, rc)

    !---------------------------------------------------------------------------
    ! Convert the cism data to export data to the mediator
    !---------------------------------------------------------------------------

    ! uses
    use glc_indexing          , only : nx, ny
    use glc_fields            , only : ice_covered, topo, rofi, rofl 
    use glc_fields            , only : hflx, ice_sheet_grid_mask
    use glc_route_ice_runoff  , only : route_ice_runoff    
    use glc_override_frac     , only : frac_overrides_enabled, do_frac_overrides
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variabes
    type(ESMF_State)     :: exportState
    integer, intent(out) :: rc

    ! local variables
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

    real(r8), allocatable :: hflx_to_cpl(:,:)
    real(r8), allocatable :: rofl_to_cpl(:,:)
    real(r8), allocatable :: rofi_to_ocn(:,:)
    real(r8), allocatable :: rofi_to_ice(:,:)

    integer               :: dbrc
    character(*), parameter :: subName = "(glc_import_export:export_fields) "
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! If overrides of glc fraction are enabled (for testing purposes), then apply
    ! these overrides, otherwise use the real version of ice_covered and topo
    if (frac_overrides_enabled()) then
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

    if (zero_gcm_fluxes) then
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

   ! Fill export state 

    call state_setexport(exportState, 'Fogg_rofi', rofi_to_ocn, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Figg_rofi', rofi_to_ice, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Fogg_rofl', rofl_to_cpl, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Sg_ice_covered', ice_covered_to_cpl, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Sg_topo', topo_to_cpl, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Flgg_hflx', hflx_to_cpl, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Sg_icemask', ice_sheet_grid_mask, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_setexport(exportState, 'Sg_icemask_coupled_fluxes', icemask_coupled_fluxes, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    deallocate(icemask_coupled_fluxes)
    deallocate(hflx_to_cpl)
    deallocate(rofl_to_cpl)
    deallocate(rofi_to_ocn)
    deallocate(rofi_to_ice)
    if (fields_to_cpl_allocated) then
       deallocate(ice_covered_to_cpl)
       deallocate(topo_to_cpl)
    end if

  end subroutine export_fields

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname)
    integer,                    intent(inout) :: num
    type(fld_list_type),        intent(inout) :: fldlist(:)
    character(len=*),           intent(in)    :: stdname

    ! local variables
    integer :: rc
    integer :: dbrc
    character(len=*), parameter :: subname='(glc_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    ! Set up a list of field information

    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
       return
    endif
    fldlist(num)%stdname = trim(stdname)

  end subroutine fldlist_add

  !===============================================================================

  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

    use NUOPC , only : NUOPC_IsConnected, NUOPC_Realize
    use ESMF  , only : ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_TYPEKIND_R8
    use ESMF  , only : ESMF_MAXSTR, ESMF_Field, ESMF_State, ESMF_Mesh, ESMF_StateRemove
    use ESMF  , only : ESMF_LogFoundError, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF  , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LOGERR_PASSTHRU

    type(ESMF_State)    , intent(inout) :: state
    type(fld_list_type) , intent(in)    :: fldList(:)
    integer             , intent(in)    :: numflds
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    character(len=*)    , intent(in)    :: tag
    type(ESMF_Mesh)     , intent(in)    :: mesh
    integer             , intent(inout) :: rc

    ! local variables
    integer                :: dbrc
    integer                :: n
    type(ESMF_Field)       :: field
    character(len=80)      :: stdname
    character(len=*),parameter  :: subname='(glc_import_export:fldlist_realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1, numflds
       stdname = fldList(n)%stdname
       if (NUOPC_IsConnected(state, fieldName=stdname)) then
          if (stdname == trim(flds_scalar_name)) then
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe", &
                  ESMF_LOGMSG_INFO, rc=dbrc)
             ! Create the scalar field
             call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          else
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected using mesh", &
                  ESMF_LOGMSG_INFO, rc=dbrc)
             ! Create the field
             field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          endif

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       else
          if (stdname /= trim(flds_scalar_name)) then
             call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(stdname) // " is not connected.", &
                  ESMF_LOGMSG_INFO, rc=dbrc)
             call ESMF_StateRemove(state, (/stdname/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
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
           ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    end subroutine SetScalarField

  end subroutine fldlist_realize

  !===============================================================================

  subroutine state_getimport(state, fldname, output, rc)

    ! ----------------------------------------------
    ! Map import state field to output array
    ! ----------------------------------------------

    ! uses
    use glc_indexing          , only : vector_to_spatial
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variables
    type(ESMF_State)    , intent(in)    :: state
    character(len=*)    , intent(in)    :: fldname
    real(r8)            , intent(out)   :: output(:,:)
    integer             , intent(out)   :: rc

    ! local variables
    real(R8), pointer         :: fldptr(:)
    type(ESMF_StateItem_Flag) :: itemFlag
    integer                   :: glcYMD ! glc model date
    integer                   :: glcTOD ! glc model sec 
    character(len=CS)         :: string
    integer                   :: n
    integer                   :: dbrc
    character(len=*), parameter :: subname='(glc_import_export:state_getimport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! If field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then

       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! determine output array
       call vector_to_spatial(fldptr, output)

       if (debug_import > 0 .and. my_task == master_task) then
          glcYMD = iyear*10000 + imonth*100 + iday
          glcTOD = ihour*3600 + iminute*60 + isecond
          do n = 1,size(fldptr)
             if (fldptr(n) /= 0.0_r8) then
                write(stdout,F01)' glcYMD, glcTOD, n,'// trim(fldname) // ' = ',glcYMD, glcTOD, n, fldptr(n)
             end if
          end do
       end if

    end if

  end subroutine state_getimport

  !===============================================================================

  subroutine state_setexport(state, fldname, input, rc)

    ! ----------------------------------------------
    ! Map input array to export state field 
    ! ----------------------------------------------

    ! uses
    use glc_indexing          , only : spatial_to_vector
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variables
    type(ESMF_State)    , intent(inout) :: state
    character(len=*)    , intent(in)    :: fldname
    real(r8)            , intent(in)    :: input(:,:)
    integer             , intent(out)   :: rc

    ! local variables
    real(R8), pointer         :: fldptr(:)
    type(ESMF_StateItem_Flag) :: itemFlag
    integer                   :: glcYMD ! glc model date
    integer                   :: glcTOD ! glc model sec 
    integer                   :: n
    character(len=CS)         :: string
    integer                   :: dbrc
    character(len=*), parameter :: subname='(glc_import_export:state_setexport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! if field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then

       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! set fldptr values to input array
       call spatial_to_vector(input, fldptr)

       if (debug_export > 0 .and. my_task == master_task) then
          glcYMD = iyear*10000 + imonth*100 + iday
          glcTOD = ihour*3600 + iminute*60 + isecond
          do n = 1,size(fldptr)
             if (fldptr(n) /= 0.0_r8) then
                write(stdout,F02) ' glcYMD, glcTOD, n,' // trim(fldname) // ' = ',glcYMD, glcTOD, n, fldptr(n)
             end if
          end do
       end if

    end if

  end subroutine state_setexport

  !===============================================================================

  subroutine state_getfldptr(State, fldname, fldptr, rc)

    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    ! uses
    use ESMF                  , only : ESMF_State, ESMF_Field, ESMF_Mesh, ESMF_FieldStatus_Flag
    use ESMF                  , only : ESMF_StateGet, ESMF_FieldGet, ESMF_MeshGet
    use ESMF                  , only : ESMF_FIELDSTATUS_COMPLETE, ESMF_FAILURE
    use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr

    ! input/output variables
    type(ESMF_State),  intent(in)    :: State
    character(len=*),  intent(in)    :: fldname
    real(R8), pointer, intent(out)   :: fldptr(:)
    integer,           intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: dbrc
    integer                     :: nnodes, nelements
    character(len=*), parameter :: subname='(glc_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
       rc = ESMF_FAILURE
       return
    else
       call ESMF_FieldGet(lfield, mesh=lmesh, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_MeshGet(lmesh, numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (nnodes == 0 .and. nelements == 0) then
          call ESMF_LogWrite(trim(subname)//": no local nodes or elements ", ESMF_LOGMSG_INFO, rc=dbrc)
          rc = ESMF_FAILURE
          return
       end if

       call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif  ! status

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine state_getfldptr

end module glc_import_export
