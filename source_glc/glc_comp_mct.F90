module glc_comp_mct

! !USES:

  use shr_sys_mod
  use shr_kind_mod     , only: IN=>SHR_KIND_IN, R8=>SHR_KIND_R8, &
                               CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_file_mod     , only: shr_file_getunit, shr_file_getlogunit, shr_file_getloglevel, &
                               shr_file_setlogunit, shr_file_setloglevel, shr_file_setio, &
                               shr_file_freeunit
  use shr_mpi_mod      , only: shr_mpi_bcast
  use mct_mod
  use esmf_mod

  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod
  use seq_flds_indices

  use glc_constants,   only : verbose,stdout,stderr,nml_in,radius,radian,tkfrz
  use glc_errormod,    only : glc_success
  use glc_InitMod,     only : glc_initialize
  use glc_RunMod,      only : glc_run
  use glc_FinalMod,    only : glc_final
  use glc_communicate, only : init_communicate
  use glc_time_management, only: iyear,imonth,iday,ihour,iminute,isecond
  use glc_global_grid, only: glc_grid, region_mask
  use glc_global_fields,only: glc_nec

! !PUBLIC TYPES:
  implicit none
  save
  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: glc_init_mct
  public :: glc_run_mct
  public :: glc_final_mct

!--------------------------------------------------------------------------
! Private data interfaces
!--------------------------------------------------------------------------

  !--- stdin input stuff ---
  character(CS) :: str                  ! cpp  defined model name
  
  !--- other ---
  integer(IN)   :: errorcode            ! glc error code
  
  character(CS) :: myModelName = 'glc'   ! user defined model name
  integer(IN)   :: my_task               ! my task in mpi communicator mpicom 
  integer(IN)   :: master_task=0         ! task number of master task

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CONTAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: glc_init_mct
!
! !DESCRIPTION:
!     initialize glc model
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------

  subroutine glc_init_mct( EClock, cdata, x2g, g2x, NLFilename )

! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Clock)         , intent(in)    :: EClock
    type(seq_cdata)          , intent(inout) :: cdata
    type(mct_aVect)          , intent(inout) :: x2g, g2x
    character(len=*), optional  , intent(in) :: NLFilename ! Namelist filename

!EOP

    !--- local variables ---
    integer(IN)              :: ierr        ! error code
    integer(IN)              :: i,j,n,nxg,nyg
    integer(IN)              :: COMPID
    integer(IN)              :: mpicom
    integer(IN)              :: lsize
    type(mct_gsMap), pointer :: gsMap
    type(mct_gGrid), pointer :: dom
    type(seq_infodata_type), pointer :: infodata   ! Input init object
    integer(IN)              :: shrlogunit, shrloglev  
    character(CL)            :: starttype

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_init_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_init_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_init_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_init_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_init_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_init_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_init_mct) "
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

    ! Set cdata pointers

    call seq_cdata_setptrs(cdata, ID=COMPID, mpicom=mpicom, &
         gsMap=gsMap, dom=dom, infodata=infodata)

    call mpi_comm_rank(mpicom, my_task, ierr)

    !--- open log file ---
    if (my_task == master_task) then
       stdout = shr_file_getUnit()
       call shr_file_setIO('glc_modelio.nml',stdout)
    else
       stdout = 6
    endif
    stderr = stdout
    nml_in = shr_file_getUnit()

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)

    call seq_infodata_GetData( infodata, start_type=starttype)

    errorCode = glc_Success
    if (verbose .and. my_task == master_task) then
       write(stdout,F00) ' Starting'
       call shr_sys_flush(stdout)
    endif
    call init_communicate(mpicom)
    call glc_initialize(errorCode)
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' GLC Initial Date ',iyear,imonth,iday,ihour,iminute,isecond
       write(stdout,F01) ' Initialize Done', errorCode
       call shr_sys_flush(stdout)
    endif

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    lsize = nxg*nyg

    ! Initialize MCT gsmap

    call glc_SetgsMap_mct(mpicom, COMPID, gsMap)

    ! Initialize MCT domain

    call glc_domain_mct(lsize,gsMap,dom)

    ! Set flags in infodata

    call seq_infodata_PutData(infodata, glc_present=.true., &
       glc_prognostic = .true., glc_nx=nxg, glc_ny=nyg)

    ! Initialize MCT attribute vectors

    call mct_aVect_init(g2x, rList=seq_flds_g2x_fields, lsize=lsize)
    call mct_aVect_zero(g2x)

    call mct_aVect_init(x2g, rList=seq_flds_x2g_fields, lsize=lsize)
    call mct_aVect_zero(x2g)

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

   type(ESMF_Clock)            ,intent(in)    :: EClock
   type(seq_cdata)             ,intent(inout) :: cdata
   type(mct_aVect)             ,intent(inout) :: x2g        ! driver -> glc
   type(mct_aVect)             ,intent(inout) :: g2x        ! glc    -> driver
   
!EOP
   !--- local ---
   integer(IN)   :: ccsmYMD           ! ccsm model date
   integer(IN)   :: ccsmTOD           ! ccsm model sec
   integer(IN)   :: glcYMD            ! glc model date
   integer(IN)   :: glcTOD            ! glc model sec 
   integer(IN)   :: n                 ! index
   integer(IN)   :: nf                ! fields loop index
   integer(IN)   :: ki                ! index of ifrac
   integer(IN)   :: lsize             ! size of AttrVect
   integer(IN)   :: nxg,nyg           ! global grid size
   real(R8)      :: lat               ! latitude
   real(R8)      :: lon               ! longitude
   integer(IN)   :: shrlogunit, shrloglev  
   logical       :: stop_alarm        ! is it time to stop
   logical       :: rest_alarm        ! is it time to write a restart
   logical       :: done              ! time loop logical
   logical       :: do_recv = .true.  ! turn off recv

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

    lsize = mct_avect_lsize(x2g)
    nxg = glc_grid%nx
    nyg = glc_grid%ny

    ! UNPACK

!-----------------------------------------------------------------------
!  unpack and distribute recv buffer
!-----------------------------------------------------------------------

!!    do_recv = .false.     ! tcx temporary 
    if (do_recv) then
    if (glc_nec >=  1) call glc_import_mct(x2g, 1,index_x2g_Ss_tsrf01, &
                       index_x2g_Ss_topo01,index_x2g_Fgss_qice01)
    if (glc_nec >=  2) call glc_import_mct(x2g, 2,index_x2g_Ss_tsrf02, &
                       index_x2g_Ss_topo02,index_x2g_Fgss_qice02)
    if (glc_nec >=  3) call glc_import_mct(x2g, 3,index_x2g_Ss_tsrf03, &
                       index_x2g_Ss_topo03,index_x2g_Fgss_qice03)
    if (glc_nec >=  4) call glc_import_mct(x2g, 4,index_x2g_Ss_tsrf04, &
                       index_x2g_Ss_topo04,index_x2g_Fgss_qice04)
    if (glc_nec >=  5) call glc_import_mct(x2g, 5,index_x2g_Ss_tsrf05, &
                       index_x2g_Ss_topo05,index_x2g_Fgss_qice05)
    if (glc_nec >=  6) call glc_import_mct(x2g, 6,index_x2g_Ss_tsrf06, &
                       index_x2g_Ss_topo06,index_x2g_Fgss_qice06)
    if (glc_nec >=  7) call glc_import_mct(x2g, 7,index_x2g_Ss_tsrf07, &
                       index_x2g_Ss_topo07,index_x2g_Fgss_qice07)
    if (glc_nec >=  8) call glc_import_mct(x2g, 8,index_x2g_Ss_tsrf08, &
                       index_x2g_Ss_topo08,index_x2g_Fgss_qice08)
    if (glc_nec >=  9) call glc_import_mct(x2g, 9,index_x2g_Ss_tsrf09, &
                       index_x2g_Ss_topo09,index_x2g_Fgss_qice09)
    if (glc_nec >= 10) call glc_import_mct(x2g,10,index_x2g_Ss_tsrf10, &
                       index_x2g_Ss_topo10,index_x2g_Fgss_qice10)
    endif

    errorCode = glc_Success

    call seq_timemgr_EClockGetData(EClock,curr_ymd=ccsmYMD, curr_tod=ccsmTOD)
    stop_alarm = seq_timemgr_StopAlarmIsOn( EClock )
    rest_alarm = seq_timemgr_RestartAlarmIsOn( EClock )

    glcYMD = iyear*10000 + imonth*100 + iday
    glcTOD = ihour*3600 + iminute*60 + isecond
    done = .false.
    if (glcYMD == ccsmYMD .and. glcTOD == ccsmTOD) done = .true.
!    if (verbose .and. my_task == master_task) then
!       write(stdout,F01) ' Run Starting ',glcYMD,glcTOD
!       call shr_sys_flush(stdout)
!    endif

    do while (.not. done) 
       if (glcYMD > ccsmYMD .or. (glcYMD == ccsmYMD .and. glcTOD > ccsmTOD)) then
          write(stdout,*) subname,' ERROR overshot coupling time ',glcYMD,glcTOD,ccsmYMD,ccsmTOD
          call shr_sys_abort('glc error overshot time')
       endif
       call glc_run
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       if (glcYMD == ccsmYMD .and. glcTOD == ccsmTOD) done = .true.
!       if (verbose .and. my_task == master_task) then
!          write(stdout,F01) ' GLC  Date ',glcYMD,glcTOD
!       endif
    enddo

!    if (verbose .and. my_task == master_task) then
!       write(stdout,F01) ' Run Done',glcYMD,glcTOD
!       call shr_sys_flush(stdout)
!    endif
    
    ! PACK

    if (glc_nec >=  1) call glc_export_mct(g2x, 1,index_g2x_Sg_frac01, &
                       index_g2x_Sg_thck01  ,index_g2x_Sg_topo01, &
                       index_g2x_Fsgg_hflx01,index_g2x_Fsgg_roff01)
    if (glc_nec >=  2) call glc_export_mct(g2x, 2,index_g2x_Sg_frac02, &
                       index_g2x_Sg_thck02  ,index_g2x_Sg_topo02, &
                       index_g2x_Fsgg_hflx02,index_g2x_Fsgg_roff02)
    if (glc_nec >=  3) call glc_export_mct(g2x, 3,index_g2x_Sg_frac03, &
                       index_g2x_Sg_thck03  ,index_g2x_Sg_topo03, &
                       index_g2x_Fsgg_hflx03,index_g2x_Fsgg_roff03)
    if (glc_nec >=  4) call glc_export_mct(g2x, 4,index_g2x_Sg_frac04, &
                       index_g2x_Sg_thck04  ,index_g2x_Sg_topo04, &
                       index_g2x_Fsgg_hflx04,index_g2x_Fsgg_roff04)
    if (glc_nec >=  5) call glc_export_mct(g2x, 5,index_g2x_Sg_frac05, &
                       index_g2x_Sg_thck05  ,index_g2x_Sg_topo05, &
                       index_g2x_Fsgg_hflx05,index_g2x_Fsgg_roff05)
    if (glc_nec >=  6) call glc_export_mct(g2x, 6,index_g2x_Sg_frac06, &
                       index_g2x_Sg_thck06  ,index_g2x_Sg_topo06, &
                       index_g2x_Fsgg_hflx06,index_g2x_Fsgg_roff06)
    if (glc_nec >=  7) call glc_export_mct(g2x, 7,index_g2x_Sg_frac07, &
                       index_g2x_Sg_thck07  ,index_g2x_Sg_topo07, &
                       index_g2x_Fsgg_hflx07,index_g2x_Fsgg_roff07)
    if (glc_nec >=  8) call glc_export_mct(g2x, 8,index_g2x_Sg_frac08, &
                       index_g2x_Sg_thck08  ,index_g2x_Sg_topo08, &
                       index_g2x_Fsgg_hflx08,index_g2x_Fsgg_roff08)
    if (glc_nec >=  9) call glc_export_mct(g2x, 9,index_g2x_Sg_frac09, &
                       index_g2x_Sg_thck09  ,index_g2x_Sg_topo09, &
                       index_g2x_Fsgg_hflx09,index_g2x_Fsgg_roff09)
    if (glc_nec >= 10) call glc_export_mct(g2x,10,index_g2x_Sg_frac10, &
                       index_g2x_Sg_thck10  ,index_g2x_Sg_topo10, &
                       index_g2x_Fsgg_hflx10,index_g2x_Fsgg_roff10)
    
    ! log output for model date

    if (my_task == master_task) then
       call seq_timemgr_EClockGetData(EClock,curr_ymd=ccsmYMD, curr_tod=ccsmTOD)
       write(stdout,F01) ' CCSM Date ', ccsmYMD,ccsmTOD
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       write(stdout,F01) ' GLC  Date ',glcYMD,glcTOD
       call shr_sys_flush(stdout)
    end if

    !----------------------------------------------------------------------------
    ! Reset shr logging to original values
    !----------------------------------------------------------------------------
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
!EOP
subroutine glc_final_mct()

    integer(IN)                           :: shrlogunit, shrloglev  

   !--- formats ---
   character(*), parameter :: F00   = "('(glc_final_mct) ',8a)"
   character(*), parameter :: F01   = "('(glc_final_mct) ',a,8i8)"
   character(*), parameter :: F91   = "('(glc_final_mct) ',73('-'))"
   character(*), parameter :: subName = "(glc_final_mct) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! Reset shr logging to my log file
   !----------------------------------------------------------------------------
   call shr_file_getLogUnit (shrlogunit)
   call shr_file_getLogLevel(shrloglev)
   call shr_file_setLogUnit (stdout)

   if (my_task == master_task) then
      write(stdout,F91) 
      write(stdout,F00) trim(myModelName),': end of main integration loop'
      write(stdout,F91) 
   end if
      
   errorCode = glc_Success

   call glc_final(errorCode)

   if (verbose .and. my_task == master_task) then
      write(stdout,F01) ' Done',errorCode
      call shr_sys_flush(stdout)
   endif

   !----------------------------------------------------------------------------
   ! Reset shr logging to original values
   !----------------------------------------------------------------------------
   call shr_file_setLogUnit (shrlogunit)
   call shr_file_setLogLevel(shrloglev)
   call shr_sys_flush(stdout)

 end subroutine glc_final_mct

!=================================================================================
  subroutine glc_import_mct(x2g,ndx,index_tsrf,index_topo,index_qice)

    use glc_global_fields, only: tsfc, topo, qice       ! from coupler

    type(mct_aVect),intent(inout) :: x2g
    integer(IN), intent(in) :: ndx
    integer(IN), intent(in) :: index_tsrf
    integer(IN), intent(in) :: index_topo
    integer(IN), intent(in) :: index_qice

    integer(IN) :: j,jj,i,g,nxg,nyg,n

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_import_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_import_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_import_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_import_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_import_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_import_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_import_mct) "
    !-------------------------------------------------------------------

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    do j = 1, nyg           ! S to N
       jj = nyg - j + 1     ! reverse j index for glint grid (N to S)
       do i = 1, nxg
          g = (j-1)*nxg + i   ! global index (W to E, S to N)
          tsfc(i,jj,ndx) = x2g%rAttr(index_tsrf,g) - tkfrz
          topo(i,jj,ndx) = x2g%rAttr(index_topo,g)
          qice(i,jj,ndx) = x2g%rAttr(index_qice,g)
       enddo
    enddo

    if (verbose) then
       write(stdout,*) subname,' x2g tsrf ',ndx,minval(x2g%rAttr(index_tsrf,:)),maxval(x2g%rAttr(index_tsrf,:))
       write(stdout,*) subname,' x2g topo ',ndx,minval(x2g%rAttr(index_topo,:)),maxval(x2g%rAttr(index_topo,:))
       write(stdout,*) subname,' x2g qice ',ndx,minval(x2g%rAttr(index_qice,:)),maxval(x2g%rAttr(index_qice,:))
       call shr_sys_flush(stdout)
    endif

  end subroutine glc_import_mct

!=================================================================================
  subroutine glc_export_mct(g2x,ndx,index_frac,index_thck,index_topo,index_hflx,index_roff)

    use glc_global_fields, only: gfrac, gthck, gtopo, ghflx, groff   ! to coupler

    type(mct_aVect),intent(inout) :: g2x
    integer(IN), intent(in) :: ndx
    integer(IN), intent(in) :: index_frac
    integer(IN), intent(in) :: index_thck
    integer(IN), intent(in) :: index_topo
    integer(IN), intent(in) :: index_hflx
    integer(IN), intent(in) :: index_roff

    integer(IN) :: j,jj,i,g,nxg,nyg,n

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_export_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_export_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_export_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_export_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_export_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_export_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_export_mct) "
    !-------------------------------------------------------------------

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    do j = 1, nyg           ! S to N
       jj = nyg - j + 1     ! reverse j index for glint grid (N to S)
       do i = 1, nxg
          g = (j-1)*nxg + i   ! global index (W to E, S to N)
          g2x%rAttr(index_frac,g) = gfrac(i,jj,ndx)
          g2x%rAttr(index_thck,g) = gthck(i,jj,ndx)
          g2x%rAttr(index_topo,g) = gtopo(i,jj,ndx)
          g2x%rAttr(index_hflx,g) = ghflx(i,jj,ndx)
          g2x%rAttr(index_roff,g) = groff(i,jj,ndx)
       enddo
    enddo

    if (verbose) then
       write(stdout,*) subname,' g2x frac ',ndx,minval(g2x%rAttr(index_frac,:)),maxval(g2x%rAttr(index_frac,:))
       write(stdout,*) subname,' g2x thck ',ndx,minval(g2x%rAttr(index_thck,:)),maxval(g2x%rAttr(index_thck,:))
       write(stdout,*) subname,' g2x topo ',ndx,minval(g2x%rAttr(index_topo,:)),maxval(g2x%rAttr(index_topo,:))
       write(stdout,*) subname,' g2x hflx ',ndx,minval(g2x%rAttr(index_hflx,:)),maxval(g2x%rAttr(index_hflx,:))
       write(stdout,*) subname,' g2x roff ',ndx,minval(g2x%rAttr(index_roff,:)),maxval(g2x%rAttr(index_roff,:))
       call shr_sys_flush(stdout)
    endif

  end subroutine glc_export_mct

!=================================================================================

  subroutine glc_SetgsMap_mct( mpicom_g, GLCID, gsMap_g )

    integer        , intent(in)  :: mpicom_g
    integer        , intent(in)  :: GLCID
    type(mct_gsMap), intent(out) :: gsMap_g

    ! Local Variables

    integer,allocatable :: gindex(:)
    integer :: i, j, n, nxg, nyg
    integer :: lsize,gsize
    integer :: ier

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_SetgsMap_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_SetgsMap_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_SetgsMap_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_SetgsMap_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_SetgsMap_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_SetgsMap_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_SetgsMap_mct) "
    !-------------------------------------------------------------------

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    lsize = nxg*nyg
    gsize = nxg*nyg

    ! Initialize MCT global seg map

    allocate(gindex(lsize))
    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       gindex(n) = n
    enddo
    enddo

    call mct_gsMap_init( gsMap_g, gindex, mpicom_g, GLCID, lsize, gsize )

    deallocate(gindex)

  end subroutine glc_SetgsMap_mct

!===============================================================================

  subroutine glc_domain_mct( lsize, gsMap_g, dom_g )

    integer        , intent(in)    :: lsize
    type(mct_gsMap), intent(inout) :: gsMap_g
    type(mct_ggrid), intent(out)   :: dom_g      

    ! Local Variables

    integer :: g,i,j,n,nxg,nyg    ! index
    real(r8), pointer :: data(:)  ! temporary
    integer , pointer :: idata(:) ! temporary

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_domain_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_domain_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_domain_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_domain_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_domain_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_domain_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_domain_mct) "
    !-------------------------------------------------------------------

    nxg = glc_grid%nx
    nyg = glc_grid%ny

    ! Initialize mct domain type

    call mct_gGrid_init( GGrid=dom_g, CoordChars=trim(seq_flds_dom_coord), &
       OtherChars=trim(seq_flds_dom_other), lsize=lsize )

    ! Initialize attribute vector with special value

    allocate(data(lsize))
    dom_g%data%rAttr(:,:) = -9999.0_R8
    dom_g%data%iAttr(:,:) = -9999
    data(:) = 0.0_R8     
    call mct_gGrid_importRAttr(dom_g,"mask" ,data,lsize) 
    call mct_gGrid_importRAttr(dom_g,"frac" ,data,lsize) 

    ! Determine global gridpoint number attribute, GlobGridNum, which is set automatically by MCT

    call mct_gsMap_orderedPoints(gsMap_g, my_task, idata)
    call mct_gGrid_importIAttr(dom_g,'GlobGridNum',idata,lsize)

    ! Fill in correct values for domain components

    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       data(n) = glc_grid%lons(i)/radian
    end do
    end do
    call mct_gGrid_importRattr(dom_g,"lon",data,lsize) 

    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       data(n) = glc_grid%lats(j)/radian
    end do
    end do
    call mct_gGrid_importRattr(dom_g,"lat",data,lsize) 

    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       data(n) = glc_grid%box_areas(i,j)/(radius*radius)
    end do
    end do
    call mct_gGrid_importRattr(dom_g,"area",data,lsize) 

    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       data(n) = region_mask(i,j)
    end do
    end do
    call mct_gGrid_importRattr(dom_g,"mask",data,lsize) 

    do j = 1,nyg
    do i = 1,nxg
       n = (j-1)*nxg + i
       data(n) = region_mask(i,j)
    end do
    end do
    call mct_gGrid_importRattr(dom_g,"frac",data,lsize) 

    deallocate(data)
    deallocate(idata)

    if (verbose) then
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
