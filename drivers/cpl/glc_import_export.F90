module glc_import_export

#include "shr_assert.h"

  use shr_sys_mod
  use shr_kind_mod,        only: IN=>SHR_KIND_IN, R8=>SHR_KIND_R8
  use shr_kind_mod,        only: CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use glc_constants,       only: verbose, stdout, stderr, tkfrz, glc_nec
  use glc_communicate,     only: my_task, master_task
  use glc_global_grid,     only: glc_grid
  use glc_cpl_indices

  implicit none
  save
  public

  ! Public interfaces
  public :: glc_import

!=================================================================================
contains
!=================================================================================

   subroutine glc_import(x2g, glc_nec, &
        index_tsrf, index_topo, index_qice)

    !-------------------------------------------------------------------
    use glc_global_fields, only: tsfc, topo, qsmb       ! from coupler

    real(r8)   , intent(in) :: x2g(:,:)
    integer(IN), intent(in) :: glc_nec
    integer(IN), intent(in) :: index_tsrf(:)
    integer(IN), intent(in) :: index_topo(:)
    integer(IN), intent(in) :: index_qice(:)

    integer(IN) :: j,jj,i,g,nxg,nyg,n,elev_class
    character(*), parameter :: subName = "(glc_import) "
    !-------------------------------------------------------------------

    SHR_ASSERT((size(index_tsrf) >= glc_nec), subName//' ERROR in size of index_tsrf')
    SHR_ASSERT((size(index_topo) >= glc_nec), subName//' ERROR in size of index_topo')
    SHR_ASSERT((size(index_qice) >= glc_nec), subName//' ERROR in size of index_qice')

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    do j = 1, nyg           ! S to N
       jj = nyg - j + 1     ! reverse j index for glint grid (N to S)
       do i = 1, nxg
          g = (j-1)*nxg + i   ! global index (W to E, S to N)

          do elev_class = 1, glc_nec
             tsfc(i,jj,elev_class) = x2g(index_tsrf(elev_class), g) - tkfrz
             topo(i,jj,elev_class) = x2g(index_topo(elev_class), g)
             qsmb(i,jj,elev_class) = x2g(index_qice(elev_class), g)
          enddo
       enddo
    enddo

    if (verbose .and. my_task==master_task) then
       do elev_class = 1, glc_nec
          write(stdout,*) ' '
          write(stdout,*) subname,' x2g tsrf ',elev_class, &
               minval(x2g(index_tsrf(elev_class),:)), &
               maxval(x2g(index_tsrf(elev_class),:))
          write(stdout,*) subname,' x2g topo ',elev_class, &
               minval(x2g(index_topo(elev_class),:)), &
               maxval(x2g(index_topo(elev_class),:))
          write(stdout,*) subname,' x2g qice ',elev_class, &
               minval(x2g(index_qice(elev_class),:)), &
               maxval(x2g(index_qice(elev_class),:))
       end do
       call shr_sys_flush(stdout)
    endif

  end subroutine glc_import

!=================================================================================

  subroutine glc_export(g2x, glc_nec, &
       index_frac, index_topo, index_hflx, &
       index_rofi_to_ocn, index_rofi_to_ice, index_rofl)

    !-------------------------------------------------------------------
    use glc_global_fields   , only: gfrac, gtopo, grofi, grofl, ghflx   ! to coupler
    use glc_route_ice_runoff, only: route_ice_runoff
    use glc_override_frac   , only: frac_overrides_enabled, do_frac_overrides

    real(r8)    ,intent(inout) :: g2x(:,:)
    integer(IN), intent(in)    :: glc_nec
    integer(IN), intent(in)    :: index_frac(:)
    integer(IN), intent(in)    :: index_topo(:)
    integer(IN), intent(in)    :: index_hflx(:)
    integer(IN), intent(in)    :: index_rofi_to_ocn
    integer(IN), intent(in)    :: index_rofi_to_ice
    integer(IN), intent(in)    :: index_rofl

    real(r8), pointer :: gfrac_to_cpl(:,:,:)   ! if overriding gfrac, this is the modified version, 
                                               ! sent to the coupler; otherwise it points to the real gfrac
    logical :: gfrac_to_cpl_allocated          ! whether we allocated gfrac_to_cpl
    integer(IN) :: j,jj,i,g,nxg,nyg,n,elev_class
    character(*), parameter :: subName = "(glc_export) "
    !-------------------------------------------------------------------

    SHR_ASSERT((size(index_frac) >= glc_nec), subName//' ERROR in size of index_frac')
    SHR_ASSERT((size(index_topo) >= glc_nec), subName//' ERROR in size of index_topo')
    SHR_ASSERT((size(index_hflx) >= glc_nec), subName//' ERROR in size of index_hflx')

    ! If overrides of glc fraction are enabled (for testing purposes), then apply
    ! these overrides, otherwise use the real version of gfrac
    if (frac_overrides_enabled()) then
       allocate(gfrac_to_cpl(lbound(gfrac,1):ubound(gfrac,1), &
                             lbound(gfrac,2):ubound(gfrac,2), &
                             lbound(gfrac,3):ubound(gfrac,3)))
       gfrac_to_cpl = gfrac
       call do_frac_overrides(gfrac_to_cpl)
       gfrac_to_cpl_allocated = .true.
    else
       gfrac_to_cpl => gfrac
       gfrac_to_cpl_allocated = .false.
    end if

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    do j = 1, nyg           ! S to N
       jj = nyg - j + 1     ! reverse j index for glint grid (N to S)
       do i = 1, nxg
          g = (j-1)*nxg + i ! global index (W to E, S to N)

          call route_ice_runoff(grofi(i,jj), &
               rofi_to_ocn=g2x(index_rofi_to_ocn, g), &
               rofi_to_ice=g2x(index_rofi_to_ice, g))
          
          g2x(index_rofl, g) = grofl(i,jj)

          do elev_class = 1, glc_nec
             g2x(index_frac(elev_class), g) = gfrac_to_cpl(i,jj,elev_class)
             g2x(index_topo(elev_class), g) = gtopo(i,jj,elev_class)
             g2x(index_hflx(elev_class), g) = ghflx(i,jj,elev_class)
          enddo
       enddo
    enddo

    if (verbose .and. my_task==master_task) then
       do elev_class = 1, glc_nec
          write(stdout,*) subname,' g2x frac ',elev_class, &
               minval(g2x(index_frac(elev_class),:)), &
               maxval(g2x(index_frac(elev_class),:))
          write(stdout,*) subname,' g2x topo ',elev_class, &
               minval(g2x(index_topo(elev_class),:)), &
               maxval(g2x(index_topo(elev_class),:))
          write(stdout,*) subname,' g2x hflx ',elev_class, &
               minval(g2x(index_hflx(elev_class),:)), &
               maxval(g2x(index_hflx(elev_class),:))
       end do

       write(stdout,*) subname,' g2x rofi to ocn ', &
            minval(g2x(index_rofi_to_ocn,:)), &
            maxval(g2x(index_rofi_to_ocn,:))
       write(stdout,*) subname,' g2x rofi to ice ', &
            minval(g2x(index_rofi_to_ice,:)), &
            maxval(g2x(index_rofi_to_ice,:))
       write(stdout,*) subname,' g2x rofl ', &
            minval(g2x(index_rofl,:)), &
            maxval(g2x(index_rofl,:))

       call shr_sys_flush(stdout)
    endif

    if (gfrac_to_cpl_allocated) then
       deallocate(gfrac_to_cpl)
    end if

  end subroutine glc_export

end module glc_import_export
