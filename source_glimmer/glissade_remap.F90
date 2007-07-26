! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
! +                                                           + 
! +  glissade_remap.F90                                       + 
! +                                                           + 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! This is the incremental remapping module for GLISSADE.
!
! Author: William Lipscomb
!         Los Alamos National Laboratory
!         Group T-3, MS B216
!         Los Alamos, NM 87545
!         USA
!         <lipscomb@lanl.gov>
!
! This incremental remapping transport scheme is based on code 
! from CICE, a sea ice model originally developed at Los Alamos
! National Lab.
!
! At some future time, this code may become part of GLIMMER.
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
 
      module glissade_remap

! Contains routines related to the incremental remapping transport scheme.
!
! See these references:
! 
! Dukowicz, J. K., and J. R. Baumgardner, 2000: Incremental
!  remapping as a transport/advection algorithm, J. Comput. Phys.,
!  160, 318-335.
!
! Lipscomb, W. H., and E. C. Hunke, 2004: Modeling sea ice
!  transport using incremental remapping, Mon. Wea. Rev., 132,
!  1341-1354.
!
! Various options related to tracers and curvilinear grids have
! been removed to simplify the code.
!--------------------------------------------------------------  
 
      use glc_constants
      use glide_types

      implicit none
      private
      public :: init_remap, remap_driver

!lipscomb - The following parameters are useful for debugging.
!lipscomb - Commented out for now.
      public :: itest, jtest, ktest, dij, debug

      logical, parameter :: debug = .false.
 
      integer, parameter ::   &
           itest = 34, jtest = 14, dij = 1, ktest = 1 

      ! parameters related to remapping

      integer, parameter ::    &
         ngroups = 5          ! max number of triangles that can contribute
                              ! to transport across a given cell edge

!=======================================================================

      contains

!=======================================================================
! 
!BOP
! 
! !IROUTINE: init_remap - initialize grid quantities used by remapping
! 
! !INTERFACE:
! 
      subroutine init_remap (nx_block, ny_block,   &
                             dew,      dns,        &
                             grid)

! 
! !DESCRIPTION: 
! 
! Grid quantities used by the incremental remapping transport scheme 
! NOTE: Only rectangular grids are supported.
! 
! !REVISION HISTORY:
! 
! author William H. Lipscomb, LANL

      implicit none

      ! input/output variables

      integer, intent(in) ::    &
          nx_block, ny_block       ! block size

      real(r8), intent(in) ::   &
          dew, dns                 ! grid cell dimensions

      type (glide_gridwk), intent(inout) :: grid

      ! grid cell dimensions

      grid%HTE(:,:) = dns         ! east cell edges
      grid%HTN(:,:) = dew         ! north cell edges

      grid%dxt(:,:) = dew         ! x-dimension of cell through center
                                  ! = p5 * (HTN(i,j-1) - HTN(i,j))
      grid%dyt(:,:) = dns         ! y-dimension of cell through center
                                  ! = p5 * (HTE(i-1,j) - HTE(i,j))

      grid%tarea(:,:) = grid%dxt(:,:) * grid%dyt(:,:)
      grid%tarear(:,:) = c1/grid%tarea(:,:)

      ! mask
      ! = 1. in all cells where ice is allowed, = 0. otherwise
      ! in most cases should be set to 1. everywhere
      grid%mask(:,:) = c1

      ! grid cell geometric averages
      ! Note: On a rectangular grid, odd functions of x and y average to zero.

      grid%xav  (:,:) = c0
      grid%yav  (:,:) = c0
      grid%xxav (:,:) = dew*dew/12._r8
      grid%xyav (:,:) = c0
      grid%yyav (:,:) = dns*dns/12._r8
        
      end subroutine init_remap

!=======================================================================

      subroutine remap_driver (nx_block,    ny_block,     &
                               ntrace,      nghost,       &
                               grid,        dt,           &
                               uvel,        vvel,         &
                               hice,        trcr,         &
                               conservation_check_in,     &
                               monotonicity_check_in,     &
                               dp_midpt_in)
                               
      use glide_types

      ! input/output variables

      integer, intent(in) ::      &
           nx_block,      &! no. of grid points in EW direction
           ny_block,      &! no. of grid points in NS direction
           ntrace,        &! no. of tracer fields
           nghost          ! no. of ghost cells (= 1 for now)
      
      type(glide_gridwk), intent(in) ::   &
           grid           ! grid information (hte, htn, tarea, etc.)

      real(r8) ::      &
           dt        ! time step

      real(r8), dimension (nx_block, ny_block), intent(in) :: &
           uvel,     &! u component of velocity
           vvel       ! v component of velocity

      real(r8), dimension (nx_block, ny_block), intent(inout) :: &
           hice      ! ice layer thickness

      real(r8), dimension (nx_block, ny_block, ntrace), intent(inout) :: &
           trcr      ! tracer fields

      logical, intent(in), optional ::     &
         conservation_check_in,   &! if true, check conservation
         monotonicity_check_in,   &! if true, check monotonicity
         dp_midpt_in               ! if true, find departure points using

      ! local variables

      real (r8), dimension (nx_block,ny_block) ::  &
           dpx          ,&! x coordinates of departure points at cell corners 
           dpy            ! y coordinates of departure points at cell corners
 
      real (r8), dimension (nx_block,ny_block) ::            &
           himask         ,&! = 1. if ice is present, = 0. otherwise
           hic            ,&! ice thickness at geometric center of cell
           hix, hiy         ! limited derivative of ice thickness wrt x and y
 
      real (r8), dimension (nx_block,ny_block,ntrace) ::     &
           trc            ,&! tracer values at geometric center of cell
           trx, try         ! limited derivative of tracer wrt x and y

      real (r8), dimension (nx_block,ny_block) ::      &
           hiflxe, hiflxn   ! ice volume transports across E and N cell edges
 
      real (r8), dimension (nx_block,ny_block,ntrace) :: &
           htflxe, htflxn   ! volume*tracer transports across E and N cell edges
 
      real (r8), dimension (nx_block,ny_block,ngroups) ::     &
           triarea        ,&! area of east-edge departure triangle
           xp1, yp1       ,&! x and y coordinates of special triangle points
           xp2, yp2       ,&! (need 3 points to integrate quadratic functions)
           xp3, yp3
 
      integer, dimension (nx_block,ny_block,ngroups) ::              &
           iflux          ,&! i index of cell contributing transport
           jflux            ! j index of cell contributing transport
 
      logical ::         &        
           abort_flag    ! if true, abort the code

      ! variables used for optional bug checks
 
      logical ::         &
         l_conservation_check,   &! if true, check conservation
         l_monotonicity_check,   &! if true, check monotonicity
         l_dp_midpt               ! if true, find departure points using
                                  ! corrected midpoint velocity

      real (r8) ::               &
           hsum_init      ,&! initial global ice volume
           hsum_final       ! final global ice volume
 
      real (r8), dimension(ntrace) ::          &
           htsum_init     ,&! initial global ice volume*tracer
           htsum_final      ! final global ice volume*tracer
 
      real (r8), dimension (:,:,:), allocatable ::     &
           tmin         ,&! local min tracer
           tmax           ! local max tracer
 
      integer :: alloc_error
 
!lipscomb - debug
      integer :: i, j

      if (present(conservation_check_in)) then
         l_conservation_check = conservation_check_in
      else
         l_conservation_check = .false.
      endif

      if (present(monotonicity_check_in)) then
         l_monotonicity_check = monotonicity_check_in
      else
         l_monotonicity_check = .false.
      endif

      if (present(dp_midpt_in)) then
         l_dp_midpt = dp_midpt_in
      else
         l_dp_midpt = .false.
      endif

    !-------------------------------------------------------------------
    ! Initialize
    !-------------------------------------------------------------------
    ! The following assumes that we are solving only these equations:
    !
    !         dh/dt = del*(u*h)    where h = thickness, u = 2D velocity
    !
    !      d(hT)/dt = del*(u*hT)   where T = a tracer
    !
    ! but not equations of the type
    ! 
    !     d(hTq)/dt = del*(u*hTq)  where q = a tracer whose transport depends
    !                              on the transport of hT
    !
    ! The original code from CICE solves the latter equation as well.
    !-------------------------------------------------------------------

!lipscomb - CICE boundary updates are commented out for now.
    !-------------------------------------------------------------------
    ! Ghost cell updates 
    !-------------------------------------------------------------------
!      call update_ghost_cells (hice,               bndy_info,       & 
!                               field_loc_center,   field_type_scalar)
!      call update_ghost_cells (trcr,               bndy_info,       & 
!                               field_loc_center,   field_type_scalar)
!      call update_ghost_cells (uvel,               bndy_info,       & 
!                               field_loc_NEcorner, field_type_vector) 
!      call update_ghost_cells (vvel,               bndy_info,       & 
!                               field_loc_NEcorner, field_type_vector) 

    !------------------------------------------------------------------- 
    ! Compute masks. 
    ! Cells are masked out where the field value has no physical meaning 
    ! (e.g., temperatures in an ice-free grid cell). 
    !------------------------------------------------------------------- 

      call make_masks (nx_block,      ny_block,          & 
                       hice(:,:),     himask(:,:))

      if (l_conservation_check) then
 
    !-------------------------------------------------------------------
    ! Compute initial values of globally conserved quantities.
    !-------------------------------------------------------------------
 
!lipscomb - Replace with an MPI subroutine when code is parallel.

         call global_sum(nx_block,       ny_block,             &
                         nghost,         ntrace,               &
                         grid%tarea(:,:),                      &
                         hice(:,:),      hsum_init,            &
                         trcr(:,:,:),    htsum_init(:))

      endif  ! conservation check

      if (l_monotonicity_check) then
 
         allocate(tmin(nx_block,ny_block,ntrace),     &
                  tmax(nx_block,ny_block,ntrace),     &
                  STAT=alloc_error)
 
!            if (alloc_error /= 0) call abort_ice ('allocation error')
 
         tmin(:,:,:) = c0
         tmax(:,:,:) = c0
 
    !-------------------------------------------------------------------
    ! Compute local max and min of tracer fields.
    !-------------------------------------------------------------------
 
         call local_max_min(nx_block,        ny_block,            &
                            nghost,          ntrace,              &
                            trcr  (:,:,:),                        &
                            tmin  (:,:,:),   tmax  (:,:,:),       &
                            himask(:,:))

!lipscomb - For parallel code, need a boundary update of tmax and tmin
!           before call to quasilocal
         
        call quasilocal_max_min (nx_block,    ny_block,     &
                                 nghost,      ntrace,       &
                                 tmin(:,:,:), tmax(:,:,:))

      endif   ! monotonicity check

    !------------------------------------------------------------------- 
    ! Construct scalar fields as linear functions of x and y, limiting
    ! gradients to preserve monotonicity. 
    !------------------------------------------------------------------- 

      call construct_fields(nx_block,            ny_block,           & 
                            nghost,              ntrace,             & 
                            grid%HTN  (:,:),     grid%HTE  (:,:),    & 
                            grid%mask (:,:),     grid%xav  (:,:),    & 
                            grid%yav  (:,:),     grid%xxav (:,:),    & 
                            grid%xyav (:,:),     grid%yyav (:,:),    &
                            grid%dxt  (:,:),     grid%dyt  (:,:),    &
                            hice      (:,:),     hic       (:,:),    &
                            hix       (:,:),     hiy       (:,:),    &
                            himask    (:,:),                         &
                            trcr      (:,:,:),   trc       (:,:,:),  &
                            trx       (:,:,:),   try       (:,:,:))

!lipscomb - Boundary updates are commented out for now
    !-------------------------------------------------------------------
    ! Ghost cell updates for centroid values and gradients
    !-------------------------------------------------------------------
 
!      call update_ghost_cells (hic,              bndy_info,            &
!                               field_loc_center, field_type_scalar)
!      call update_ghost_cells (hix,              bndy_info,            &
!                               field_loc_center, field_type_vector)
!      call update_ghost_cells (hiy,              bndy_info,            &
!                               field_loc_center, field_type_vector)
 
!      call update_ghost_cells (trc,              bndy_info,            &
!                               field_loc_center, field_type_scalar)
!      call update_ghost_cells (trx,              bndy_info,            &
!                               field_loc_center, field_type_vector)
!      call update_ghost_cells (try,              bndy_info,            &
!                               field_loc_center, field_type_vector)

      call departure_points(nx_block,            ny_block,             &
                            nghost,              dt,                   &
                            uvel        (:,:),   vvel   (:,:),         &
                            dpx         (:,:),   dpy    (:,:),         &
                            grid%HTN    (:,:),   grid%HTE    (:,:),    &
                            grid%dxt    (:,:),   grid%dyt    (:,:),    &
                            l_dp_midpt,          abort_flag)

      if (abort_flag) then
         ! instead of aborting, just print a warning
!!            call abort_ice('remap transport: bad departure points')
      endif

!lipscomb - Boundary updates commented out for now 
!         call update_ghost_cells (dpx,                bndy_info,     &
!                                  field_loc_NEcorner, field_type_vector)
!         call update_ghost_cells (dpy,                bndy_info,     &
!                                  field_loc_NEcorner, field_type_vector)

    !-------------------------------------------------------------------
    ! Transports for east cell edges.
    !-------------------------------------------------------------------
 
    !-------------------------------------------------------------------
    ! Compute areas and vertices of departure triangles.
    !-------------------------------------------------------------------
 
      call locate_triangles_east (nx_block,       ny_block,           &
                                  nghost,                             & 
                                  dpx     (:,:),  dpy     (:,:),      & 
                                  grid%HTN(:,:),  grid%HTE(:,:),      & 
                                  xp1,            yp1,                &
                                  xp2,            yp2,                &
                                  xp3,            yp3,                &
                                  iflux,          jflux,              & 
                                  triarea) 
 
    !-------------------------------------------------------------------
    ! Given triangle vertices, compute coordinates of triangle points
    !  needed for transport integrals.
    !-------------------------------------------------------------------
 
      call triangle_coordinates (nx_block, ny_block,            &
                                 triarea,                       &
                                 xp1,      yp1,                 & 
                                 xp2,      yp2,                 & 
                                 xp3,      yp3)
 
    !-------------------------------------------------------------------
    ! Compute the transport across east cell edges by summing contributions
    ! from each triangle.
    !-------------------------------------------------------------------
 
      call transport_integrals(nx_block,   ny_block,           &
                               nghost,     ntrace,             &
                               triarea,                        &
                               iflux,      jflux,              &
                               xp1,        yp1,                &
                               xp2,        yp2,                &
                               xp3,        yp3,                &
                               hic(:,:),   hix   (:,:),        &
                               hiy(:,:),   hiflxe(:,:),        &
                               trc(:,:,:), trx   (:,:,:),      &
                               try(:,:,:), htflxe(:,:,:))
 
    !-------------------------------------------------------------------
    ! Repeat for north edges
    !-------------------------------------------------------------------

    !------------------------------------------------------------------- 
    ! Compute areas and vertices of departure triangles. 
    !------------------------------------------------------------------- 
 
      call locate_triangles_north(nx_block,       ny_block,           &
                                  nghost,                             & 
                                  dpx     (:,:),  dpy     (:,:),      & 
                                  grid%HTN(:,:),  grid%HTE(:,:),      & 
                                  xp1,            yp1,                &
                                  xp2,            yp2,                &
                                  xp3,            yp3,                &
                                  iflux,          jflux,              & 
                                  triarea) 
 
    !------------------------------------------------------------------- 
    ! Given triangle vertices, compute coordinates of triangle points 
    !  needed for transport integrals. 
    !------------------------------------------------------------------- 
 
      call triangle_coordinates (nx_block, ny_block,            & 
                                 triarea,                       &
                                 xp1,      yp1,                 & 
                                 xp2,      yp2,                 & 
                                 xp3,      yp3)

    !------------------------------------------------------------------- 
    ! Compute the transport across east cell edges by summing contributions 
    ! from each triangle. 
    !------------------------------------------------------------------- 
 
      call transport_integrals(nx_block,   ny_block,           & 
                               nghost,     ntrace,             &
                               triarea,                        &
                               iflux,      jflux,              & 
                               xp1,        yp1,                &
                               xp2,        yp2,                &
                               xp3,        yp3,                &
                               hic(:,:),   hix   (:,:),        & 
                               hiy(:,:),   hiflxn(:,:),        & 
                               trc(:,:,:), trx   (:,:,:),      & 
                               try(:,:,:), htflxn(:,:,:)) 

    !------------------------------------------------------------------- 
    ! Updates thickness and tracers by summing fluxes across cell edges.
    !-------------------------------------------------------------------
 
      call update_fields(nx_block,         ny_block,           &
                         nghost,           ntrace,             &
                         grid%tarear(:,:),                     &
                         hiflxe(:,:),      hiflxn(:,:),        &
                         hice  (:,:),                          &
                         htflxe(:,:,:),    htflxn(:,:,:),      &
                         trcr  (:,:,:),                        &
                         abort_flag)
 
      if (abort_flag) then
!        call abort_ice ('remap transport: negative thickness')
      endif

!---!-------------------------------------------------------------------
!---! Optional conservation and monotonicity checks
!---!-------------------------------------------------------------------
 
    !-------------------------------------------------------------------
    ! Compute final values of globally conserved quantities.
    ! Check global conservation of volume and volume*tracers. 
    !-------------------------------------------------------------------
      
      if (l_conservation_check) then
 
!lipscomb - Replace global_sum with MPI subroutine when code is parallel
         call global_sum(nx_block,       ny_block,             &
                         nghost,         ntrace,               &
                         grid%tarea(:,:),                      &
                         hice(:,:),      hsum_final,           &
                         trcr(:,:,:),    htsum_final(:))

         call global_conservation(hsum_init,     hsum_final,      &
                                  ntrace,                         &
                                  htsum_init(:), htsum_final(:),  &
                                  abort_flag)

         if (abort_flag) then
!            call abort_ice ('remap transport: conservation error')
            stop
         endif
 
      endif                     ! l_conservation_check

    !-------------------------------------------------------------------
    ! Check tracer monotonicity.
    !-------------------------------------------------------------------

      if (l_monotonicity_check) then
         call check_monotonicity(nx_block,     ny_block,             &
                                 nghost,       ntrace,               &
                                 tmin(:,:,:),  tmax(:,:,:),          &
                                 hice(:,:),    trcr(:,:,:),          &
                                 abort_flag)

         if (abort_flag) then
!            call abort_ice('remap transport: monotonicity error')
            stop
         endif

         deallocate(tmin, tmax, STAT=alloc_error)
         if (alloc_error /= 0) then
!            call abort_ice ('deallocation error')
            stop
         endif

      endif                     ! l_monotonicity_check

      deallocate(tmin, tmax, STAT=alloc_error)
!         if (alloc_error /= 0) call abort_ice ('deallocation error')

      end subroutine remap_driver

!=======================================================================
!
!BOP
!
! !IROUTINE: make_masks - make ice thickness and tracer masks
!
! !INTERFACE:
!
      subroutine make_masks (nx_block, ny_block,     &
                             him,      himask)
!
! !DESCRIPTION:
!
! Make thickness and tracer masks.
!
! If ice is absent (him < eps), then the cell is masked out for tracer
! gradient calculations.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                         &
           nx_block, ny_block  ! block dimensions
 
      real (r8), dimension (nx_block, ny_block),          &
           intent(inout) ::                                          &
           him            ! mean ice thickness in each grid cell
 
      real (r8), dimension (nx_block, ny_block),          &
           intent(out) ::                                            &
           himask         ! = 1. if ice is present, else = 0.
!
!EOP
!
      integer ::                                     &
           i, j         ,&! horizontal indices
           nt             ! tracer index
 
    !-------------------------------------------------------------------
    ! ice thickness mask
    !-------------------------------------------------------------------
 
      do j = 1, ny_block
      do i = 1, nx_block
         if (him(i,j) > eps) then
            himask(i,j) = c1
         else
            himask(i,j) = c0
         endif
      enddo
      enddo
 
      end subroutine make_masks

!=======================================================================
!
!BOP
!
! !IROUTINE: construct_fields - construct ice thickness and tracer fields
!
! !INTERFACE:
!
      subroutine construct_fields (nx_block, ny_block,     &
                                   nghost,   ntrace,       &
                                   HTN,      HTE,          &
                                   gridmask, xav,          &
                                   yav,      xxav,         &
                                   xyav,     yyav,         &
                                   dxt,      dyt,          &
                                   him,      hic,          &
                                   hix,      hiy,          &
                                   himask,                 &
                                   trm,      trc,          &
                                   trx,      try)
!
! !DESCRIPTION:
!
! Construct fields of ice thickness and tracers.
!
! !REVISION HISTORY:
!
! authors William H. Lipscomb, LANL
!         John R. Baumgardner, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::      &
          nx_block,      &! no. of grid points in EW direction
          ny_block,      &! no. of grid points in NS direction
          ntrace,        &! no. of tracer fields
          nghost          ! no. of ghost cells

      real (r8), dimension (nx_block,ny_block), intent(in) ::  &
         gridmask          ,&! ice/boundary mask (T-cell)
         HTN               ,&! length of northern edge of T-cell (m)
         HTE               ,&! length of eastern edge of T-cell (m)
         xav,  yav         ,&! mean T-cell values of x, y
         xxav, xyav, yyav  ,&! mean T-cell values of xx, xy, yy
         dxt               ,&! grid cell width (m)
         dyt                 ! grid cell height (m)
 
      real (r8), dimension (nx_block,ny_block),          &
         intent(in) ::                                              &
         him            ,&! mean ice thickness
         himask           ! = 1. if ice is present, = 0. otherwise
 
      real (r8), dimension (nx_block,ny_block,ntrace),   &
         intent(in), optional ::                                    &
         trm              ! mean tracer
 
      real (r8), dimension (nx_block,ny_block),          &
         intent(out) ::                                             &
         hic            ,&! ice thickness at geometric center of cell
         hix, hiy         ! limited derivative of ice thickness wrt x and y
 
      real (r8), dimension (nx_block,ny_block,ntrace),   &
         intent(out), optional ::                                   &
         trc            ,&! tracer at geometric center of cell
         trx, try         ! limited derivative of tracer wrt x and y
!
!EOP
!
      integer ::                                    &
         i, j           ,&! horizontal indices
         nt, nt1        ,&! tracer indices
         ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8), dimension (nx_block,ny_block) ::        &
         hxav, hyav       ! x,y coordinates of center of ice mass
 
    !-------------------------------------------------------------------
    ! Compute field values at the geometric center of each grid cell,
    ! and compute limited gradients in the x and y directions.
    !
    ! For second order accuracy, each state variable is approximated as
    !  a field varying linearly over x and y within each cell.
    ! The integrated value of hice(x,y) over the cell must
    !  equal hice(i,j)*tarea(i,j), where tarea(i,j) is the cell area.
    ! Similarly, the integrated value of hice(x,y)*tice(x,y) must
    !  equal hice(i,j)*tice(i,j)*tarea(i,j).
    !
    ! These integral conditions are satisfied for linear fields if we
    ! stipulate the following:
    ! (1) The mean ice thickness, hice(i,j), is equal to the value at
    !     the cell centroid: the point where an equal grid cell area
    !     lies to the north and south, and to the east and west.
    !     For rectangular cells, the centroid coincides with the 
    !     geometric center.
    ! (2) The mean ice tracer, tice(i,j), is equal to the
    !     value at the center of ice mass: the point where an equal
    !     mass lies to the north and south, and to the east and west.
    !
    ! We want to find the value of each state variable at a standard
    ! reference point, which we choose to be the geometric center of
    ! the cell.  The geometric center is located at the intersection
    ! of the line joining the midpoints of the north and south edges
    ! with the line joining the midpoints of the east and west edges.
    ! To find the value at the geometric center, we must know the
    ! location of the cell centroid/center of mass relative to the
    ! geometric center, along with the field gradients.
    !
    ! The gradients are first computed from the difference between
    ! values in the neighboring cells, then limited by requiring that
    ! no new extrema are created within the cell.
    !-------------------------------------------------------------------

      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      ! ice thickness gradient

      call limited_gradient (nx_block, ny_block,     &
                             nghost,                 &
                             him,      gridmask,     &
                             xav,      yav,          &
                             HTN,      HTE,          &
                             dxt,      dyt,          &
                             hix,      hiy)
 
      hic (:,:) = c0
      hxav(:,:) = c0
      hyav(:,:) = c0
 
      do j = jlo,jhi
      do i = ilo,ihi
         if (himask(i,j) > eps) then ! ice is present
 
            ! ice thickness at geometric center
            hic(i,j) = him(i,j) - xav(i,j)*hix(i,j)         &
                                - yav(i,j)*hiy(i,j)

            ! center of ice mass for each cell
            hxav(i,j) = (hix(i,j)*xxav(i,j)                 &
                       + hiy(i,j)*xyav(i,j)                 &
                       + hic(i,j)*xav (i,j)) / him(i,j)
            hyav(i,j) = (hix(i,j)*xyav(i,j)                 &
                       + hiy(i,j)*yyav(i,j)                 &
                       + hic(i,j)*yav (i,j)) / him(i,j)
         endif
      enddo    ! i
      enddo    ! j

      ! tracers
 
      if (present(trm)) then
 
       do nt = 1, ntrace
 
          trc  (:,:,nt) = c0
 
          call limited_gradient(nx_block,     ny_block,     &
                                nghost,                     &
                                trm(:,:,nt),  himask,       &
                                hxav,         hyav,         &
                                HTN,          HTE,          &
                                dxt,          dyt,          &
                                trx(:,:,nt),  try(:,:,nt)) 
 
          do j = jlo,jhi
          do i = ilo,ihi
             if (himask(i,j) > eps) then ! ice is present
 
               ! tracer value at geometric center
                trc(i,j,nt) = trm(i,j,nt) - trx(i,j,nt)*hxav(i,j)     &
                                          - try(i,j,nt)*hyav(i,j)
             endif
          enddo            ! i
          enddo            ! j

       enddo                    ! ntrace
 
      endif                     ! present (trm)
 
      end subroutine construct_fields

!=======================================================================
! !IROUTINE: limited_gradient - limited gradient of a scalar field
!
! !INTERFACE:
!
      subroutine limited_gradient (nx_block, ny_block,     &
                                   nghost,                 &
                                   phi,      phimask,      &
                                   cnx,      cny,          &
                                   HTN,      HTE,          &
                                   dxt,      dyt,          &
                                   gx,       gy)
!
! !DESCRIPTION:
!
! Compute a limited gradient of the scalar field phi.
! "Limited" means that we do not create new extrema in phi.  For
! instance, field values at the cell corners can neither exceed the
! maximum of phi(i,j) in the cell and its eight neighbors, nor fall
! below the minimum.
!
! !REVISION HISTORY:
!
! authors William H. Lipscomb, LANL
!         John R. Baumgardner, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                   &
          nx_block, ny_block,&! block dimensions
          nghost              ! number of ghost cells
 
      real (r8), dimension (nx_block,ny_block), intent (in) :: &
          phi    ,&! input tracer field (mean values in each grid cell)
          cnx    ,&! x-coordinate of phi relative to geometric center of cell
          cny    ,&! y-coordinate of phi relative to geometric center of cell
          dxt    ,&! grid cell width (m)
          dyt    ,&! grid cell height (m)
          phimask,&! phimask = 1. if phi has physical meaning, = 0 otherwise
          HTN    ,&! length of northern edge of T-cell
          HTE      ! length of eastern edge of T-cell
 
      real (r8), dimension (nx_block,ny_block), intent(out) ::  &
          gx     ,&! limited x-direction gradient
          gy       ! limited y-direction gradient
!
!EOS
!
      integer ::                               &
          i, j,              &! standard indices
          ilo,ihi,jlo,jhi     ! beginning and end of physical domain
 
      real (r8) ::                                  &
          phi_nw, phi_n, phi_ne, &! values of phi in 8 neighbor cells
          phi_w,         phi_e,  &
          phi_sw, phi_s, phi_se, &
          qmn, qmx     ,&! min and max value of phi within grid cell
          pmn, pmx     ,&! min and max value of phi among neighbor cells
          w1, w2, w3, w4 ! work variables
 
      real (r8) ::                                  &
          gxtmp, gytmp   ! temporary term for x- and y- limited gradient
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      gx(:,:) = c0
      gy(:,:) = c0
 
      do j = jlo, jhi
      do i = ilo, ihi
         if (phimask(i,j) > eps) then

         ! Store values of phi in the 8 neighbor cells.
         ! Note: phimask = 1. or 0.  If phimask = 1., use the true value;
         !  if phimask = 0., use the home cell value so that non-physical
         !  values of phi do not contribute to the gradient.
            phi_nw = phimask(i-1,j+1) * phi(i-1,j+1)                   &
               + (c1-phimask(i-1,j+1))* phi(i,j)
            phi_n  = phimask(i,j+1)   * phi(i,j+1)                     &
               + (c1-phimask(i,j+1))  * phi(i,j)
            phi_ne = phimask(i+1,j+1) * phi(i+1,j+1)                   &
               + (c1-phimask(i+1,j+1))* phi(i,j)
            phi_w  = phimask(i-1,j)   * phi(i-1,j)                     &  
               + (c1-phimask(i-1,j))  * phi(i,j)
            phi_e  = phimask(i+1,j)   * phi(i+1,j)                     &
               + (c1-phimask(i+1,j))  * phi(i,j)
            phi_sw = phimask(i-1,j-1) * phi(i-1,j-1)                   &
               + (c1-phimask(i-1,j-1))* phi(i,j)
            phi_s  = phimask(i,j-1)   * phi(i,j-1)                     &
               + (c1-phimask(i,j-1))  * phi(i,j)
            phi_se = phimask(i+1,j-1) * phi(i+1,j-1)                   &
               + (c1-phimask(i+1,j-1))* phi(i,j)
 
         ! unlimited gradient components
         ! (factors of two cancel out)
            gxtmp = (phi_e - phi(i,j)) / (dxt(i,j)   + dxt(i+1,j))     &
                  + (phi(i,j) - phi_w) / (dxt(i-1,j) + dxt(i,j)  )
            gytmp = (phi_n - phi(i,j)) / (dyt(i,j)   + dyt(i,j+1))     &
                  + (phi(i,j) - phi_s) / (dyt(i,j-1) + dyt(i,j)  )
 
         ! minimum and maximum among the nine local cells
            pmn = min (phi_nw, phi_n,  phi_ne, phi_w, phi(i,j),        &
                       phi_e,  phi_sw, phi_s,  phi_se)
            pmx = max (phi_nw, phi_n,  phi_ne, phi_w, phi(i,j),        &
                       phi_e,  phi_sw, phi_s,  phi_se)
 
            pmn = pmn - phi(i,j)
            pmx = pmx - phi(i,j)
 
         ! minimum and maximum deviation of phi within the cell
            w1  =  (p5*HTN(i,j)   - cnx(i,j)) * gxtmp                  &
                 + (p5*HTE(i,j)   - cny(i,j)) * gytmp
            w2  =  (p5*HTN(i,j-1) - cnx(i,j)) * gxtmp                  &
                 - (p5*HTE(i,j)   + cny(i,j)) * gytmp
            w3  = -(p5*HTN(i,j-1) + cnx(i,j)) * gxtmp                  &
                 - (p5*HTE(i-1,j) + cny(i,j)) * gytmp
            w4  =  (p5*HTE(i-1,j) - cny(i,j)) * gytmp                  &
                 - (p5*HTN(i,j)   + cnx(i,j)) * gxtmp
 
            qmn = min (w1, w2, w3, w4)
            qmx = max (w1, w2, w3, w4)
 
         ! the limiting coefficient
            if (abs(qmn) > c0) then ! 'abs(qmn) > eps' not sufficient
               w1 = max(c0, pmn/qmn)
            else
               w1 = c1
            endif

            if (abs(qmx) > c0) then
               w2 = max(c0, pmx/qmx)
            else
               w2 = c1
            endif

            w1 = min(c1, w1, w2)

         ! Limit the gradient components
            gx(i,j) = gxtmp * w1
            gy(i,j) = gytmp * w1

         endif  ! phimask > eps

      enddo  ! i
      enddo  ! j

      end subroutine limited_gradient
 
!=======================================================================
! !IROUTINE: departure_points - compute departure points of trajectories
!
! !INTERFACE:
!
      subroutine departure_points (nx_block,   ny_block,               &
                                   nghost,     dt,                     &
                                   uvel,       vvel,                   &
                                   dpx,        dpy,                    &
                                   HTN,        HTE,                    &
                                   dxt,        dyt,                    &
                                   l_dp_midpt, abort_flag)
!
! !DESCRIPTION:
!
! Given velocity fields on cell corners, compute departure points
! of trajectories, using a midpoint approximation if desired.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                           &
         nx_block, ny_block,  &! block dimensions
         nghost                ! number of ghost cells
 
      real (r8), intent(in) ::                              &
         dt               ! time step (s)
 
      real (r8), dimension (nx_block,ny_block), intent(in) ::  &
         uvel           ,&! x-component of velocity (m/s)
         vvel           ,&! y-component of velocity (m/s)
         HTN            ,&! length of northern edge of T-cell (m)
         HTE            ,&! length of eastern edge of T-cell (m)
         dxt            ,&! width of T-cell through the middle (m)
         dyt              ! height of T-cell through the middle (m)
 
      real (r8), dimension (nx_block,ny_block), intent(out) :: &
         dpx            ,&! x coordinates of departure points at cell corners
         dpy              ! y coordinates of departure points at cell corners
 
      logical, intent(in) ::                           &
         l_dp_midpt       ! if true, find departure points using
                          ! corrected midpoint velocity
 
      logical, intent(out), optional ::                &
         abort_flag       ! flag for departure point out of bounds
                          ! of neighboring cells
!
!EOP
!
      integer ::                                       &
         i, j           ,&! horizontal indices
         i2, j2         ,&! horizontal indices of neighbor cell
         ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8) ::                                          &
         mpx, mpy       ,&! coordinates of midpoint of back trajectory
         u1t, v1t       ,&! transformed velocity, SW corner
         u2t, v2t       ,&! transformed velocity, SE corner
         u3t, v3t       ,&! transformed velocity, NE corner
         u4t, v4t       ,&! transformed velocity, NW corner     
         ump, vmp       ,&! velocity at midpoint of trajectory 
         cxt, cyt       ,&! transformed cell corner coordinates
         mpxt, mpyt     ,&! midpoint transformed to cell-ctr coordinates
         mpxs, mpys       ! midpoint in stretched coordinates
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost

    !-------------------------------------------------------------------
    ! Estimate departure points.
    ! This estimate is 1st-order accurate; improve accuracy by
    !  using midpoint approximation below.
    !-------------------------------------------------------------------
 
      dpx(:,:) = c0
      dpy(:,:) = c0
 
      do j = jlo, jhi
      do i = ilo, ihi
         dpx(i,j) = -dt*uvel(i,j)
         dpy(i,j) = -dt*vvel(i,j)
      enddo
      enddo
 
    !-------------------------------------------------------------------
    ! Check for values out of bounds (more than one grid cell away).
    !-------------------------------------------------------------------
 
      if (present(abort_flag)) then
 
         abort_flag = .false.
 
         do j = jlo, jhi
         do i = ilo, ihi
            if ((dpx(i,j) < -HTN(i,j)) .or.                            &
                (dpx(i,j) >  HTN(i+1,j)) .or.                          &
                (dpy(i,j) < -HTE(i,j)) .or.                            &
                (dpy(i,j) >  HTE(i,j+1))) then
               abort_flag = .true.
               write (6,*) ' '
               write (6,*) 'Warning: Departure points out of bounds in remap'
               write (6,*) 'i, j =', i, j
               write (6,*) 'dpx, dpy =', dpx(i,j), dpy(i,j)
            endif
         enddo
         enddo

      endif                     ! present(abort_flag)
 
      if (l_dp_midpt) then ! find dep pts using corrected midpt velocity 
 
         do j = jlo, jhi
         do i = ilo, ihi
 
    !-------------------------------------------------------------------
    ! Estimate midpoint of backward trajectory relative to corner (i,j).
    !-------------------------------------------------------------------
            mpx = p5 * dpx(i,j)
            mpy = p5 * dpy(i,j)
 
    !-------------------------------------------------------------------
    ! Determine the indices (i2,j2) of the cell where the trajectory lies.
    ! Compute the coordinates (cxt,cyt) of corner (i,j) relative to
    !  center (i2,j2) in the (i2,j2) reference frame.
    !-------------------------------------------------------------------
 
            if (mpx >= c0 .and. mpy >= c0) then    ! cell (i+1,j+1)
               i2 = i+1
               j2 = j+1
               cxt  = -p5*HTN(i2,j2-1)
               cyt  = -p5*HTE(i2-1,j2)
            elseif (mpx < c0 .and. mpy < c0) then  ! cell (i,j)
               i2 = i
               j2 = j
               cxt  = p5*HTN(i2,j2)
               cyt  = p5*HTE(i2,j2)
            elseif (mpx >= c0 .and. mpy < c0) then ! cell (i+1,j)
               i2 = i+1
               j2 = j
               cxt = -p5*HTN(i2,j2)
               cyt =  p5*HTE(i2-1,j2)
            elseif (mpx < c0 .and. mpy >= c0) then ! cell (i,j+1)
               i2 = i
               j2 = j+1
               cxt =  p5*HTN(i2,j2-1)
               cyt = -p5*HTE(i2,j2)
            endif
            
    !-------------------------------------------------------------------
    ! Transform coordinates of the trajectory midpoint to the (i2,j2)
    ! reference frame.
    !-------------------------------------------------------------------
 
            mpxt = cxt + mpx
            mpyt = cyt + mpy 
 
    !-------------------------------------------------------------------
    ! Transform (mpxt,mpyt) to a stretched coordinate system in which
    ! the coordinates of the four corners relative to the center are
    ! (-1,-1), (1,-1), (1,1), and (-1,1).
    !-------------------------------------------------------------------
 
            mpxs = (c2*mpxt*dyt(i2,j2)) / (dxt(i2,j2)*dyt(i2,j2)) 
            mpys = (c2*mpyt*dxt(i2,j2)) / (dxt(i2,j2)*dyt(i2,j2)) 
 
            ! velocities at corners of cell (i2,j2)

            u1t = uvel(i2-1,j2-1)
            v1t = vvel(i2-1,j2-1)
            u2t = uvel(i2,  j2-1)
            v2t = vvel(i2,  j2-1)
            u3t = uvel(i2,  j2)
            v3t = vvel(i2,  j2)
            u4t = uvel(i2-1,j2)
            v4t = vvel(i2-1,j2)
 
    !-------------------------------------------------------------------
    ! Using a bilinear approximation, estimate the velocity at the
    ! trajectory midpoint.
    !-------------------------------------------------------------------
 
            ump = p25 * ( u1t*(mpxs-c1)*(mpys-c1)     &
                        - u2t*(mpxs+c1)*(mpys-c1)     &
                        + u3t*(mpxs+c1)*(mpys+c1)     &
                        - u4t*(mpxs-c1)*(mpys+c1) )
 
            vmp = p25 * ( v1t*(mpxs-c1)*(mpys-c1)     &
                        - v2t*(mpxs+c1)*(mpys-c1)     &
                        + v3t*(mpxs+c1)*(mpys+c1)     &
                        - v4t*(mpxs-c1)*(mpys+c1) )
 
    !-------------------------------------------------------------------
    ! Use the midpoint velocity to estimate the coordinates of the
    ! departure point relative to corner (i,j).
    !-------------------------------------------------------------------
 
            dpx(i,j) = -dt * ump
            dpy(i,j) = -dt * vmp
            
         enddo                  ! i
         enddo                  ! j
 
      endif                     ! l_dp_mipdt

      end subroutine departure_points

!=======================================================================
!
!BOP
!
! !IROUTINE: locate_triangles_east - triangle info for east edges
!
! !INTERFACE:
!
      subroutine locate_triangles_east (nx_block,  ny_block,     &
                                        nghost,                  &
                                        dpx,       dpy,          &
                                        HTN,       HTE,          &
                                        xp1,       yp1,          &
                                        xp2,       yp2,          &
                                        xp3,       yp3,          &
                                        iflux,     jflux,        &
                                        triarea)
!
! !DESCRIPTION:
!
! Compute areas and vertices of transport triangles for east cell edges.
!
! !REVISION HISTORY:
!
! authors William H. Lipscomb, LANL
!         John R. Baumgardner, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                      &
         nx_block, ny_block,&! block dimensions
         nghost              ! number of ghost cells
 
      real (r8), dimension (nx_block,ny_block),        &
           intent(in) ::                                          &
         dpx            ,&! x coordinates of departure points at cell corners
         dpy            ,&! y coordinates of departure points at cell corners
         HTN            ,&! length of northern edge of T-cell (m)
         HTE              ! length of southern edge of T-cell
 
      real (r8), dimension(nx_block,ny_block,ngroups), &
           intent(out) ::                                         &
         triarea        ,&! area of east-edge departure triangle
         xp1, yp1       ,&     
         xp2, yp2       ,&
         xp3, yp3
 
      integer, intent(out), dimension (nx_block, ny_block, ngroups) :: &
         iflux          ,&! i index of cell contributing east transport
         jflux            ! j index of cell contributing east transport
!
!EOP
!
      integer ::                                  &
         i, j           ,&! horizontal indices of cell edge
         i2, j2         ,&! horizontal indices of cell contributing transport
         ng             ,&! triangle group index
         ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8) ::                                     &
         x1, y1, x2, y2 ,&! x,y coordinates of departure points, as in DB
         xa, ya, xb, yb ,&! x,y coordinates of points where the lines joining
                          ! (x1,y1) and (x2,y2) cross cell edges, as in DB
         xca, yca       ,&! transformed coordinates of corner point a
         xda, yda       ,&! transformed coordinates of departure point a
         xxa, yxa       ,&! transformed coordinates of intersection point xa
         xya, yya       ,&! transformed coordinates of intersection point ya
         xcb, ycb       ,&! transformed coordinates of corner point b
         xdb, ydb       ,&! transformed coordinates of departure point b
         xxb, yxb       ,&! transformed coordinates of intersection point xb
         xyb, yyb       ,&! transformed coordinates of intersection point yb
         xic, yic       ,&! transformed coordinates of point where the
                          ! line joining dep pts intersects the face
         w1               ! work variable
 
      integer, dimension (nx_block,ny_block,ngroups) ::  &
         fluxsign         ! = 1 for positive flux, -1 for negative
 
      logical :: cnd1, cnd2, cnd3   ! conditionals
 
    !-------------------------------------------------------------------
    ! Triangle notation:
    ! For each edge, there are 20 triangles that can contribute,
    ! but many of these are mutually exclusive.  It turns out that
    ! at most 5 triangles can contribute to transport integrals at once.
    !
    ! For the east edge, these triangles are referred to as:
    ! (1) NE, NE1, N, N2
    ! (2) SE, SE1, S, S2
    ! (3) NE2, N1, SE2, S1
    ! (4) H1a, H1b, E1a, E2b
    ! (5) H2a, H2b, N2a, N2b
    !
    ! See Figure 3 in DB for pictures of these triangles.
    ! See Table 1 in DB for logical conditions.
    !
    ! Many triangle vertices lie at points whose coordinates are
    ! (x1,y1), (x2,y2), (xa,0), (xb,0), (0,ya), and (0,yb) in a
    ! reference frame whose origin is the cell corner.  These
    ! coordinates must be transformed to the reference frame whose
    ! origin is the geometric center of the T-cell in which the triangle
    ! is located.
    !-------------------------------------------------------------------
 
    !-------------------------------------------------------------------
    ! Initialize
    !-------------------------------------------------------------------
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      do ng = 1, ngroups
         fluxsign(:,:,ng) = 0
         iflux   (:,:,ng) = 0
         jflux   (:,:,ng) = 0
         xp1     (:,:,ng) = c0
         xp2     (:,:,ng) = c0
         xp3     (:,:,ng) = c0
         yp1     (:,:,ng) = c0
         yp2     (:,:,ng) = c0
         yp3     (:,:,ng) = c0
      enddo
 
    !-------------------------------------------------------------------
    ! Main loop
    !-------------------------------------------------------------------
 
      do j = jlo, jhi
      do i = ilo-1, ihi      ! includes W edge of cells with index ilo
 
    !-------------------------------------------------------------------
    ! coordinates of departure points
    !-------------------------------------------------------------------
         x1 = dpx(i,j)
         y1 = dpy(i,j)
         x2 = dpx(i,j-1)
         y2 = dpy(i,j-1)
         w1 =  c1 / (y2 - HTE(i,j)  - y1)
         xa = (x1*(y2 - HTE(i,j)) - x2*y1) * w1
         xb = (x1*y2 - x2*(y1 + HTE(i,j))) * w1
         if (abs(xb-xa) > eps) then
            ya = xa * HTE(i,j) / (xb - xa)
            yb = ya + HTE(i,j)
         else
            ya = c0
            yb = c0
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in NE cell
    ! (All corner cells follow the same pattern.)
    !-------------------------------------------------------------------
         ! load horizontal indices of NE cell
         i2 = i+1
         j2 = j+1
 
         ! find vertex coordinates relative to center of NE cell
 
         xca = -p5*HTN(i2,j2-1)                         ! corner pt
         yca = -p5*HTE(i2-1,j2)
 
         xda = xca + x1                                 ! departure pt
         yda = yca + y1
 
         xxa = xca + xa                                 ! xa
         yxa = yca
 
         xya = xca                                      ! ya
         yya = yca + ya
 
         ! vertices of 2 potential group 1 triangles
         ng = 1
 
         cnd1 = xa > c0 .and. y1 > c0 .and. x1 >= c0   ! NE (group 1)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         cnd2 = xa < c0 .and. y1 > c0 .and. x1 >= c0   ! NE1 (group 1)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! vertices of potential group 3 triangle
         ng = 3
 
         cnd3 = xa > c0 .and. y1 > c0 .and. x1 < c0    ! NE2 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xya
            yp3     (i,j,ng) = yya
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in N cell
    !-------------------------------------------------------------------
         i2 = i
         j2 = j+1
 
         xca =  p5*HTN(i2,j2-1)
         yca = -p5*HTE(i2,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xxa = xca + xa
         yxa = yca
 
         xya = xca
         yya = yca + ya
 
         ng = 1
 
         cnd1 =  xa < c0 .and. y1 > c0 .and. x1 < c0    ! N (group 1)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         cnd2 = xa > c0 .and. y1 > c0 .and. x1 < c0     ! N2 (group 1)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ng = 3
 
         cnd3 =  xa < c0 .and. y1 > c0 .and. x1 >= c0   ! N1 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xya
            yp3     (i,j,ng) = yya
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in SE cell
    !-------------------------------------------------------------------
         i2 = i+1
         j2 = j-1
 
         xcb = -p5*HTN(i2,j2)
         ycb =  p5*HTE(i2-1,j2)
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xxb = xcb + xb
         yxb = ycb
 
         xyb = xcb
         yyb = ycb + yb
 
         ng = 2
 
         cnd1 = xb > c0 .and. y2 < c0 .and. x2 >= c0    ! SE (group 2)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         cnd2 = xb < c0 .and. y2 < c0 .and. x2 >= c0    ! SE1 (group 2)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ng = 3
 
         cnd3 = xb > c0 .and. y2 < c0 .and. x2 < c0     ! SE2 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xyb
            yp3     (i,j,ng) = yyb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in S cell
    !-------------------------------------------------------------------
         i2 = i
         j2 = j-1
 
         xcb = p5*HTN(i2,j2)
         ycb = p5*HTE(i2,j2)
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xxb = xcb + xb
         yxb = ycb
 
         xyb = xcb
         yyb = ycb + yb
 
         ng = 2
 
         cnd1 = xb < c0 .and. y2 < c0 .and. x2 < c0     ! S (group 2)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         cnd2 = xb > c0 .and. y2 < c0 .and. x2 < c0     ! S2 (group 2)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ng = 3
 
         cnd3 = xb < c0 .and. y2 < c0 .and. x2 >= c0    ! S1 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xyb
            yp3     (i,j,ng) = yyb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! redefine departure points if not in home or north cells
    !-------------------------------------------------------------------
 
         if (y1 > c0) then
            x1 = xa
            y1 = c0
         endif
 
         if (y2 < c0) then
            x2 = xb
            y2 = c0
         endif
 
         ! quantity used to compute intersection point
 
         if (abs(xb-xa) > eps) then
            w1 = min (c1, max(c0, xb/(xb-xa)))
         else
            w1 = c0
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles inside home cell
    ! Note that home and facing cells follow the same pattern.
    !-------------------------------------------------------------------
 
         ! load horizontal indices
         i2 = i
         j2 = j
 
         ! triangle vertices relative to center of home cell
 
         xca =  p5*HTN(i2,j2)
         yca =  p5*HTE(i2,j2)
 
         xcb =  p5*HTN(i2,j2-1)
         ycb = -p5*HTE(i2,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xic = p5 * (w1*(HTN(i2,j2)-HTN(i2,j2-1)) + HTN(i2,j2-1))
         yic = (w1 - p5) * HTE(i2,j2)
 
         ! contribution from triangle that includes the
         ! E cell edge (part of convex quadrilateral inside home cell)
 
         ng = 4
 
         cnd1 = xa*xb >= c0 .and. xa+xb < c0            ! H1a (group 4)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xcb
            yp2     (i,j,ng) = ycb
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
        ! contribution from triangle lying along the upper part
        ! of E edge for case of line xa-xb intersecting the edge
 
         cnd2 = xa*xb < c0 .and. x1 < c0                ! H1b (group 4)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! contribution from triangle touching but not
         ! lying along the E edge (other part of convex quadrilateral)
 
         ng = 5
 
         cnd1 = xa*xb >= c0 .and. xa+xb < c0            ! H2a (group 5)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xda
            yp2     (i,j,ng) = yda
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! contribution from triangle lying along the lower part
         ! of E edge for case of line xa-xb intersecting the edge
 
         cnd2 = xa*xb < c0 .and. x2 < c0                ! H2b (group 5)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux  (i,j,ng)  = i2
            jflux  (i,j,ng)  = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in E cell
    !-------------------------------------------------------------------
 
         i2 = i+1
         j2 = j
 
         xca = -p5*HTN(i2,j2)
         yca =  p5*HTE(i2-1,j2)
 
         xcb = -p5*HTN(i2,j2-1)
         ycb = -p5*HTE(i2-1,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xic = -p5 * (w1*(HTN(i2,j2)-HTN(i2,j2-1)) + HTN(i2,j2-1))
         yic = (w1 - p5) * HTE(i2-1,j2)
 
         ! contribution from triangle that includes the
         ! W cell edge (part of convex quadrilateral inside E cell)
 
         ng = 4
 
         cnd1 = xa*xb >= c0 .and. xa+xb > c0            ! E1a (group 4)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xcb
            yp2     (i,j,ng) = ycb
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle lying along the upper part
         ! of W edge for case of line xa-xb intersecting the edge
 
         cnd2 = xa*xb < c0 .and. x1 > c0                ! E1b (group 4)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle touching but not
         ! lying along the W edge (other part of convex quadrilateral)
 
         ng = 5
 
         cnd1 = xa*xb >= c0 .and. xa+xb > c0            ! E2a (group 5)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xda
            yp2     (i,j,ng) = yda
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle lying along the lower part
         ! of W edge for case of line xa-xb intersecting the edge
 
         cnd2 = xa*xb < c0 .and. x2 > c0                ! E2b (group 5)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
      enddo                     ! i
      enddo                     ! j
 
    !-------------------------------------------------------------------
    ! compute triangle areas with appropriate sign
    !-------------------------------------------------------------------
 
      do ng = 1, ngroups
         do j = 1, ny_block
         do i = 1, nx_block
 
            w1 = p5 * abs( (xp2(i,j,ng)-xp1(i,j,ng)) *     &
                           (yp3(i,j,ng)-yp1(i,j,ng))       &
                         - (yp2(i,j,ng)-yp1(i,j,ng)) *     &
                           (xp3(i,j,ng)-xp1(i,j,ng)) )
 
            triarea(i,j,ng) = fluxsign(i,j,ng) * w1
 
            if (abs(triarea(i,j,ng)) <= eps) triarea(i,j,ng) = c0 
 
         enddo                  ! i
         enddo                  ! j
      enddo                     ! ng
 
      end subroutine locate_triangles_east
 
!=======================================================================
!
!BOP
!
! !IROUTINE: locate_triangles_north - triangle info for north edges
!
! !INTERFACE:
!
      subroutine locate_triangles_north(nx_block,  ny_block,     &
                                        nghost,                  &
                                        dpx,       dpy,          &
                                        HTN,       HTE,          &
                                        xp1,       yp1,          &
                                        xp2,       yp2,          &
                                        xp3,       yp3,          &
                                        iflux,     jflux,        &
                                        triarea)
!
! !DESCRIPTION:
!
! Compute areas and vertices of transport triangles for north cell edges.
! Note: The north edges follow the same pattern as the east edges.
! With some work, it would be possible to have a single generic
!  subroutine for both east and north edges.
!
! !REVISION HISTORY:
!
! authors William H. Lipscomb, LANL
!         John R. Baumgardner, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                          &
         nx_block, ny_block,&! block dimensions
         nghost              ! number of ghost cells
 
      real (r8), dimension (nx_block,ny_block),            &
           intent(in) ::                                              &
         dpx            ,&! x coordinates of departure points at cell corners
         dpy            ,&! y coordinates of departure points at cell corners
         HTN            ,&! length of northern edge of T-cell (m)
         HTE              ! length of southern edge of T-cell
 
      real (r8), dimension (nx_block, ny_block, ngroups),  &
           intent(out) ::                                             &
         triarea        ,&! area of north-edge departure triangle
         xp1,   yp1     ,&
         xp2,   yp2     ,&
         xp3,   yp3
 
      integer, intent(out), dimension (nx_block, ny_block, ngroups) :: &
         iflux          ,&! i index of cell contributing north transport
         jflux            ! j index of cell contributing north transport
!
!EOP
!
      integer ::                                      &
         i, j           ,&! horizontal indices of cell edge
         i2, j2         ,&! horizontal indices of cell contributing transport
         ng             ,&! triangle group index
         ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8) ::                                         &
         x1, y1, x2, y2 ,&! x,y coordinates of departure points, as in DB
         xa, ya, xb, yb ,&! x,y coordinates of points where the lines joining
                          ! (x1,y1) and (x2,y2) cross cell edges, as in DB
         xca, yca       ,&! transformed coordinates of corner point a
         xda, yda       ,&! transformed coordinates of departure point a
         xxa, yxa       ,&! transformed coordinates of intersection point xa
         xya, yya       ,&! transformed coordinates of intersection point ya
         xcb, ycb       ,&! transformed coordinates of corner point b
         xdb, ydb       ,&! transformed coordinates of departure point b
         xxb, yxb       ,&! transformed coordinates of intersection point xb
         xyb, yyb       ,&! transformed coordinates of intersection point yb
         xic, yic       ,&! transformed coordinates of point where the
                          ! line joining dep pts intersects the face
         w1               ! work variable
 
      integer, dimension (nx_block,ny_block,ngroups) ::  &
         fluxsign         ! = 1 for positive flux, -1 for negative
 
      logical :: cnd1, cnd2, cnd3   ! conditionals
 
    !-------------------------------------------------------------------
    ! Triangle notation:
    ! For each edge, there are 20 triangles that can contribute,
    ! but many of these are mutually exclusive.  It turns out that
    ! at most 5 triangles can contribute to transport integrals at once.
    !
    ! For the north edge, these triangles are referred to as:
    ! (1) NW, NW1, W, W2
    ! (2) NE, NE1, E, E2
    ! (3) NW2, W1, NE2, E1
    ! (4) H1a, H1b, N1a, N1b
    ! (5) H2a, H2b, N2a, N2b
    !
    ! See Figure 3 in DB for pictures of these triangles.
    ! See Table 1 in DB for logical conditions.
    !
    ! Many triangle vertices lie at points whose coordinates are
    ! (x1,y1), (x2,y2), (xa,0), (xb,0), (0,ya), and (0,yb) in a
    ! reference frame whose origin is the cell corner.  These
    ! coordinates must be transformed to the reference frame whose
    ! origin is the geometric center of the T-cell in which the triangle
    ! is located.
    !-------------------------------------------------------------------
 
    !-------------------------------------------------------------------
    ! Initialize
    !-------------------------------------------------------------------
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      do ng = 1, ngroups
         fluxsign(:,:,ng) = 0
         iflux   (:,:,ng) = 0
         jflux   (:,:,ng) = 0
         xp1     (:,:,ng) = c0
         xp2     (:,:,ng) = c0
         xp3     (:,:,ng) = c0
         yp1     (:,:,ng) = c0
         yp2     (:,:,ng) = c0
         yp3     (:,:,ng) = c0
      enddo
 
    !-------------------------------------------------------------------
    ! Main loop
    !-------------------------------------------------------------------
 
      do j = jlo-1, jhi  ! includes S edge of cells with index jlo
      do i = ilo, ihi
 
    !-------------------------------------------------------------------
    ! coordinates of departure points
    !-------------------------------------------------------------------
         x2 = dpx(i,j)
         y2 = dpy(i,j)
         x1 = dpx(i-1,j)
         y1 = dpy(i-1,j)
         w1 =  c1 / (x1 - HTN(i,j)  - x2)
         ya = (x1*y2 - y1*(HTN(i,j) + x2)) * w1
         yb = (y2*(x1 - HTN(i,j)) - x2*y1) * w1
         if (abs(ya-yb) > eps) then
            xa = ya*HTN(i,j) / (ya - yb)
            xb = xa - HTN(i,j)
         else
            xa = c0
            xb = c0
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in NW cell
    !-------------------------------------------------------------------
         i2 = i-1
         j2 = j+1
 
         xca =  p5*HTN(i2,j2-1)                         ! corner pt
         yca = -p5*HTE(i2,j2)
 
         xda = xca + x1                                 ! departure pt
         yda = yca + y1
 
         xya = xca                                      ! ya
         yya = yca + ya
 
         xxa = xca + xa                                 ! xa
         yxa = yca
 
         ng = 1
 
         cnd1 = ya > c0 .and. x1 < c0 .and. y1 >= c0    ! NW (group 1)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         cnd2 = ya < c0 .and. x1 < c0 .and. y1 >= c0    ! NW1 (group 1)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ng = 3
 
         cnd3 = ya > c0 .and. x1 < c0 .and. y1 < c0     ! NW2 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xxa
            yp3     (i,j,ng) = yxa
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in W cell
    !-------------------------------------------------------------------
         i2 = i-1
         j2 = j
 
         xca = p5*HTN(i2,j2)
         yca = p5*HTE(i2,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xya = xca
         yya = yca + ya
 
         xxa = xca + xa
         yxa = yca
 
         ng = 1
 
         cnd1 = ya < c0 .and. x1 < c0 .and. y1 < c0     ! W (group 1)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         cnd2 = ya > c0 .and. x1 < c0 .and. y1 < c0     ! W2 (group 1)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xxa
            yp2     (i,j,ng) = yxa
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ng = 3
 
         cnd3 = ya < c0 .and. x1 < c0 .and. y1 >= c0    ! W1 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xya
            yp2     (i,j,ng) = yya
            xp3     (i,j,ng) = xxa
            yp3     (i,j,ng) = yxa
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in NE cell
    !-------------------------------------------------------------------
         i2 = i+1
         j2 = j+1
 
         xcb = -p5*HTN(i2,j2-1)
         ycb = -p5*HTE(i2-1,j2)
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xyb = xcb
         yyb = ycb + yb
 
         xxb = xcb + xb
         yxb = ycb
 
         ng = 2
 
         cnd1 = yb > c0 .and. x2 > c0 .and. y2 >= c0    ! NE (group 2)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         cnd2 = yb < c0 .and. x2 > c0  .and. y2 >= c0   ! NE1 (group 2)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ng = 3
 
         cnd3 = yb > c0 .and. x2 > c0 .and. y2 < c0     ! NE2 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xxb
            yp3     (i,j,ng) = yxb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in E cell
    !-------------------------------------------------------------------
         i2 = i+1
         j2 = j
 
         xcb = -p5*HTN(i2,j2)
         ycb =  p5*HTE(i2-1,j2)
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xyb = xcb
         yyb = ycb + yb
 
         xxb = xcb + xb
         yxb = ycb
 
         ng = 2
 
         cnd1 = yb < c0 .and. x2 > c0 .and. y2 < c0     ! E (group 2)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         cnd2 = yb > c0 .and. x2 > c0 .and. y2 < c0     ! E2 (group 2)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xxb
            yp2     (i,j,ng) = yxb
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ng = 3
 
         cnd3 = yb < c0 .and. x2 > c0 .and. y2 >= c0    ! E1 (group 3)
         if (cnd3) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xyb
            yp2     (i,j,ng) = yyb
            xp3     (i,j,ng) = xxb
            yp3     (i,j,ng) = yxb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! redefine departure points if not in home or north cells
    !-------------------------------------------------------------------
         if (x1 < c0) then
            x1 = c0
            y1 = ya
         endif
 
         if (x2 > c0) then
            x2 = c0
            y2 = yb
         endif
 
         ! quantity used to compute intersection point
 
         if (abs(yb-ya) > eps) then
            w1 = min (c1, max(c0, yb/(yb-ya)))
         else
            w1 = c0
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles inside home cell
    !-------------------------------------------------------------------
         i2 = i
         j2 = j
 
         xca = -p5*HTN(i2,j2)
         yca =  p5*HTE(i2-1,j2)
 
         xcb =  p5*HTN(i2,j2)
         ycb =  p5*HTE(i2,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xic = (p5 - w1) * HTN(i2,j2)          ! intersection w/ N edge
         yic = p5 * (w1*(HTE(i2-1,j2)-HTE(i2,j2)) + HTE(i2,j2))
 
         ! contribution from triangle that includes the
         ! N cell edge (part of convex quadrilateral inside home cell)
 
         ng = 4
 
         cnd1 = ya*yb >= c0 .and. ya+yb < c0            ! H1a (group 4)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xcb
            yp2     (i,j,ng) = ycb
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! contribution from triangle lying along the left part of
         ! the N edge for case of line ya-yb intersecting the edge
 
         cnd2 = ya*yb < c0 .and. y1 < c0                ! H1b (group 4)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! contribution from triangle touching but not lying
         ! along the N edge (other part of convex quadrilateral)
 
         ng = 5
 
         cnd1 = ya*yb >= c0 .and. ya+yb < c0            ! H2a (group 5)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xda
            yp2     (i,j,ng) = yda
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
         ! contribution from triangle lying along the right part
         ! of the N edge for case of line ya-yb intersecting the edge
 
         cnd2 = ya*yb < c0 .and. y2 < c0                ! H2b (group 5)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = 1
         endif
 
    !-------------------------------------------------------------------
    ! contribution from triangles in N cell
    !-------------------------------------------------------------------
         i2 = i
         j2 = j+1
 
         xca = -p5*HTN(i2,j2-1)
         yca = -p5*HTE(i2-1,j2)
 
         xcb =  p5*HTN(i2,j2-1)
         ycb = -p5*HTE(i2,j2)
 
         xda = xca + x1
         yda = yca + y1
 
         xdb = xcb + x2
         ydb = ycb + y2
 
         xic = (p5 - w1)*HTN(i2,j2-1)
         yic = -p5 * (w1*(HTE(i2-1,j2)-HTE(i2,j2)) + HTE(i2,j2))
 
         ! contribution from triangle that includes the
         ! S cell edge (part of convex quadrilateral inside home cell)
 
         ng = 4
 
         cnd1 = ya*yb >= c0 .and. ya+yb > c0            ! N1a (group 4)
         if (cnd1) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xcb
            yp2     (i,j,ng) = ycb
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle lying along the left part
         ! of the S edge for case of line ya-yb intersecting the edge
 
         cnd2 = ya*yb < c0 .and. y1 > c0                ! N1b (group 4)
         if (cnd2) then
            xp1     (i,j,ng) = xca
            yp1     (i,j,ng) = yca
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xda
            yp3     (i,j,ng) = yda
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle touching but not
         ! lying along the S edge (other part of convex quadrilateral)
 
         ng = 5
 
         cnd1 = ya*yb >= c0 .and. ya+yb > c0            ! N2a (group 5)
         if (cnd1) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xda
            yp2     (i,j,ng) = yda
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
         ! contribution from triangle lying along the right part
         ! of the S edge for case of line ya-yb intersecting the edge
 
         cnd2 = ya*yb < c0 .and. y2 > c0                ! N2b (group 5)
         if (cnd2) then
            xp1     (i,j,ng) = xcb
            yp1     (i,j,ng) = ycb
            xp2     (i,j,ng) = xic
            yp2     (i,j,ng) = yic
            xp3     (i,j,ng) = xdb
            yp3     (i,j,ng) = ydb
            iflux   (i,j,ng) = i2
            jflux   (i,j,ng) = j2
            fluxsign(i,j,ng) = -1
         endif
 
      enddo                     ! i loop
      enddo                     ! j loop
 
    !-------------------------------------------------------------------
    ! compute triangle areas with appropriate sign
    !-------------------------------------------------------------------
 
      do ng = 1, ngroups
         do j = 1, ny_block
         do i = 1, nx_block
 
            w1 = p5 * abs( (xp2(i,j,ng)-xp1(i,j,ng)) *     &
                           (yp3(i,j,ng)-yp1(i,j,ng))       &
                         - (yp2(i,j,ng)-yp1(i,j,ng)) *     &
                           (xp3(i,j,ng)-xp1(i,j,ng)) )
 
            triarea(i,j,ng) = fluxsign(i,j,ng) * w1
 
            if (abs(triarea(i,j,ng)) <= eps) triarea(i,j,ng) = c0 
 
         enddo                  ! i
         enddo                  ! j
      enddo                     ! ng
 
      end subroutine locate_triangles_north
 
!=======================================================================
!
!BOP
!
! !IROUTINE: triangle_coordinates - find coordinates of quadrature points
!
! !INTERFACE:
!
      subroutine triangle_coordinates (nx_block, ny_block,           &
                                       triarea,                      &
                                       xp1,      yp1,                &
                                       xp2,      yp2,                &
                                       xp3,      yp3)
!
! !DESCRIPTION:
!
! For each triangle, find the coordinates of the quadrature points needed
!  to compute integrals of quadratic polynomials.
!
! The formula is as follows:
!
! I2 = integral of f(x,y,x^2,xy,y^2)
!    = A * [f(x1,y1) + f(x2,y2) + f(x3,y3)]
! where A is the traingle area, and the three points are located
! halfway between the midpoint and the three vertices of the triangle.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                         &
           nx_block, ny_block  ! block dimensions
 
      real (r8), intent(in),                              &
           dimension (nx_block, ny_block, ngroups) ::                &
           triarea             ! areas of departure triangles
 
      real (r8), intent(inout),                           &
           dimension (nx_block, ny_block, ngroups) ::                &
           xp1, yp1          ,&
           xp2, yp2          ,&
           xp3, yp3
!
!EOP
!
      integer ::                                     &
           i, j              ,&! horizontal indices
           ng                  ! triangle index
 
      real (r8) ::                                        &
           xp0, yp0            ! coordinates of triangle midpoint
 
      ! Compute coordinates of 3 points needed for quadratic integrals.
      ! These points lie halfway between the midpoint and vertices.

      do ng = 1, ngroups
         do j = 1, ny_block
         do i = 1, nx_block
 
            if (abs(triarea(i,j,ng)) > eps) then
 
            ! coordinates of midpoint
               xp0 = p333 * (xp1(i,j,ng) + xp2(i,j,ng) + xp3(i,j,ng))
               yp0 = p333 * (yp1(i,j,ng) + yp2(i,j,ng) + yp3(i,j,ng))

            ! coordinates of the 3 points needed for integrals
 
               xp1(i,j,ng) = p5 * (xp1(i,j,ng) + xp0)
               yp1(i,j,ng) = p5 * (yp1(i,j,ng) + yp0)
 
               xp2(i,j,ng) = p5 * (xp2(i,j,ng) + xp0)
               yp2(i,j,ng) = p5 * (yp2(i,j,ng) + yp0)
 
               xp3(i,j,ng) = p5 * (xp3(i,j,ng) + xp0)
               yp3(i,j,ng) = p5 * (yp3(i,j,ng) + yp0)
 
            endif               ! triarea > eps
 
         enddo                  ! i
         enddo                  ! j
      enddo                  ! ng
  
      end subroutine triangle_coordinates
 
!=======================================================================
!
!BOP
!
! !IROUTINE: transport_integrals - compute transports across each edge
!
! !INTERFACE:
!
      subroutine transport_integrals (nx_block, ny_block,           &
                                      nghost,   ntrace,             &
                                      triarea,                      &
                                      iflux,    jflux,              &
                                      xp1,      yp1,                &
                                      xp2,      yp2,                &
                                      xp3,      yp3,                &
                                      hic,      hix,                &
                                      hiy,      hiflx,              &
                                      trc,      trx,                &
                                      try,      htflx)
!
! !DESCRIPTION:
!
! Compute the transports across each edge by integrating the ice thickness
! and tracers over each departure triangle.
! Input variables have the same meanings as in the main subroutine.
! Repeated use of certain sums makes the calculation more efficient.
! Integral formulas are described in triangle_coordinates subroutine.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                       &
           nx_block, ny_block, &! block dimensions
           nghost,             &! number of ghost cells
           ntrace               ! number of tracers

      real (r8), intent(in),                            &
           dimension (nx_block, ny_block, ngroups) ::              &
           triarea,   &
           xp1, yp1,  &
           xp2, yp2,  &
           xp3, yp3
 
      integer, intent(in),                         &
           dimension (nx_block, ny_block, ngroups) ::              &
           iflux,     &
           jflux
 
      real (r8), intent(in),                            &
           dimension (nx_block, ny_block) ::                       &
           hic, hix, hiy
 
      real (r8), intent(out),                           &
           dimension (nx_block, ny_block) ::                       &
           hiflx
 
      real (r8), intent(in),                            &
           dimension (nx_block, ny_block, ntrace), optional ::     &
           trc, trx, try
 
      real (r8), intent(out),                           &
           dimension (nx_block, ny_block, ntrace), optional ::     &
           htflx
!
!EOP
!
      integer ::                                   &
           i, j          ,&! horizontal indices of edge
           i2, j2        ,&! horizontal indices of cell contributing transport
           ng            ,&! triangle index
           nt, nt1       ,&! tracer indices
           ilo,ihi,jlo,jhi ! beginning and end of physical domain
 
      real (r8) ::                                      &
           h1, h2, h3         ! ice thickness at internal points
 
      real (r8), dimension (nx_block, ny_block) ::      &
           hsum, hxsum, hysum     ! sum of h, h*x, and h*y
 
      real (r8), dimension (nx_block, ny_block, ntrace) ::   &
           htsum             ! sum of h*tracer
 
    !-------------------------------------------------------------------
    ! Initialize
    !-------------------------------------------------------------------
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      hiflx(:,:) = c0
      if (present(htflx)) then
         do nt = 1, ntrace
            htflx(:,:,nt) = c0
         enddo
      endif
 
    !-------------------------------------------------------------------
    ! Main loop
    !-------------------------------------------------------------------
 
      do ng = 1, ngroups
 
         do j = jlo-1, jhi
         do i = ilo-1, ihi
            if (abs(triarea(i,j,ng)) > eps) then
 
               i2 = iflux(i,j,ng)
               j2 = jflux(i,j,ng)
 
            ! volume transports
            ! Weighting factor of 1/3 is incorporated into the ice
            ! thickness terms h1, h2, and h3.
               h1 = p333 * (hic(i2,j2) + xp1(i,j,ng)*hix(i2,j2)     &
                                       + yp1(i,j,ng)*hiy(i2,j2))
               h2 = p333 * (hic(i2,j2) + xp2(i,j,ng)*hix(i2,j2)     &
                                       + yp2(i,j,ng)*hiy(i2,j2))
               h3 = p333 * (hic(i2,j2) + xp3(i,j,ng)*hix(i2,j2)     &
                                       + yp3(i,j,ng)*hiy(i2,j2))
               hsum(i,j) = h1 + h2 + h3
               hiflx(i,j) = hiflx(i,j) + triarea(i,j,ng)*hsum(i,j)
 
            ! quantities needed for tracer transports
               hxsum(i,j) = h1 * xp1(i,j,ng)     &
                          + h2 * xp2(i,j,ng)     &
                          + h3 * xp3(i,j,ng)     
 
               hysum(i,j) = h1 * yp1(i,j,ng)     &
                          + h2 * yp2(i,j,ng)     &
                          + h3 * yp3(i,j,ng)
 
            endif    ! triarea > eps

         enddo     ! i
         enddo     ! j

         ! volume * tracer transports
 
         if (present(htflx)) then
 
            do nt = 1, ntrace
 
               do j = jlo-1, jhi
               do i = ilo-1, ihi
                  if (abs(triarea(i,j,ng)) > eps) then
 
                     i2 = iflux(i,j,ng)
                     j2 = jflux(i,j,ng)
 
                     htsum(i,j,nt) =  hsum(i,j) * trc(i2,j2,nt)       &
                                   + hxsum(i,j) * trx(i2,j2,nt)       &
                                   + hysum(i,j) * try(i2,j2,nt)
 
                     htflx(i,j,nt) = htflx(i,j,nt)                    &
                                 + triarea(i,j,ng) * htsum(i,j,nt)
 
                  endif         ! triarea > eps
               enddo            ! i
               enddo            ! j
            enddo               ! ntrace
         endif                  ! present(htflx)
      enddo                     ! ng
 
      end subroutine transport_integrals
 
!=======================================================================
!
!BOP
!
! !IROUTINE: update_fields - compute new thickness and tracers
!
! !INTERFACE:
!
      subroutine update_fields (nx_block, ny_block,          &
                                nghost,   ntrace,            &
                                tarear,                      &
                                hiflxe,   hiflxn,   him,     &
                                htflxe,   htflxn,   trm,     &
                                abort_flag)
!
! !DESCRIPTION:
!
! Given transports through cell edges, compute new thickness and tracers.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                          &
         nx_block, ny_block, &! block dimensions
         nghost,             &! number of ghost cells
         ntrace               ! number of tracers
 
      real (r8), dimension (nx_block, ny_block),           &
         intent(in) ::                                                &
         hiflxe, hiflxn ,&! volume transport across east and north cell edges
         tarear           ! 1/tarea
 
      real (r8), dimension (nx_block, ny_block),           &
         intent(inout) ::                                             &
         him              ! mean ice thickness
 
      real (r8), dimension (nx_block, ny_block, ntrace),   &
         intent(in), optional ::                                      &
         htflxe, htflxn   ! hice*tracer transport across E and N cell edges
 
      real (r8), dimension (nx_block, ny_block, ntrace),   &
         intent(inout), optional ::                                   &
         trm              ! mean tracers
 
      logical, intent(out), optional ::               &
         abort_flag       ! if true, abort on return
!
!EOP
!
      integer ::                                      &
         i, j           ,&! horizontal indices
         nt, nt1, nt2   ,&! tracer indices
         ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8), dimension(nx_block,ny_block,ntrace) ::    &
         htold            ! starting thickness*tracer
 
      real (r8) ::                                         &
         w1, w2           ! work variables
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
    !-------------------------------------------------------------------
    ! Save starting values of thickness*tracer
    !-------------------------------------------------------------------
 
      if (present(trm)) then
         do nt = 1, ntrace
            do j = jlo, jhi
            do i = ilo, ihi
               htold(i,j,nt) = him(i,j) * trm(i,j,nt)
            enddo               ! i
            enddo               ! j
         enddo                  ! nt
      endif                     ! present(trm)
 
    !-------------------------------------------------------------------
    ! Update ice thickness
    !-------------------------------------------------------------------
 
      if (present(abort_flag)) abort_flag = .false.
 
      do j = jlo, jhi
      do i = ilo, ihi
 
         w1 = hiflxe(i,j) - hiflxe(i-1,j)                              &
            + hiflxn(i,j) - hiflxn(i,j-1)
         him(i,j) = him(i,j) - w1*tarear(i,j)
 
         if (him(i,j) < -eps) then    ! abort with negative value
            if (present(abort_flag)) abort_flag = .true.
         elseif (him(i,j) < c0) then   ! set to zero
            him(i,j) = c0
         endif
 
      enddo
      enddo
 
      if (abort_flag) then
         do j = jlo, jhi
         do i = ilo, ihi
            if (him(i,j) < -eps) then
               w1 = hiflxe(i,j) - hiflxe(i-1,j)                        &
                  + hiflxn(i,j) - hiflxn(i,j-1)
               write (6,*) ' '
               write (6,*) 'New thickness < 0, i, j =', i, j
               write (6,*) 'Old thickness =', him(i,j) + w1*tarear(i,j)
               write (6,*) 'New thickness =', him(i,j)
               write (6,*) 'Transport =', -w1*tarear(i,j)
            endif
         enddo                  ! i
         enddo                  ! j
      endif                     ! him_negative
 
    !-------------------------------------------------------------------
    ! Update tracers
    !-------------------------------------------------------------------
 
      if (present(trm)) then
         do nt = 1, ntrace 
            trm(:,:,nt) = c0
 
            do j = jlo, jhi
            do i = ilo, ihi
               if (him(i,j) > c0) then ! grid cells with positive areas
                   w1  = htflxe(i,j,nt) - htflxe(i-1,j,nt)             &
                       + htflxn(i,j,nt) - htflxn(i,j-1,nt)
                   trm(i,j,nt) = (htold(i,j,nt) - w1*tarear(i,j))      &
                                  / him(i,j)
               endif            ! him > 0.
            enddo               ! i
            enddo               ! j
         enddo                  ! nt
      endif                     ! present(trm)

      end subroutine update_fields

!=======================================================================
!
!BOP
!
! !IROUTINE: global_sum - compute global sums of conserved quantities
!
! !INTERFACE:
!
      subroutine global_sum(nx_block,  ny_block,            &
                            nghost,    ntrace,              &
                            tarea,                          &
                            hice,      hice_sum,            &
                            trcr,      trcr_sum)  
!
! !DESCRIPTION:
!
! Compute global sums of conserved quantities.
! Actually, these sums are only local for now.  This subroutine
!  should be replaced when the code is run on multiple processors.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::       &
           nx_block, ny_block,     &! block sizes
           ntrace,        &! no. of tracer fields
           nghost          ! no. of ghost cells (= 1 for now)

      real (r8), dimension(nx_block, ny_block), intent(in) :: &
           tarea,              &! grid cell area
           hice                 ! ice thickness

      real (r8), dimension(nx_block, ny_block, ntrace),   &
           intent(in) :: &
           trcr                 ! tracer fields

      real (r8), intent(out) ::                           &
           hice_sum             ! total ice volume

      real (r8), dimension(ntrace), intent(out) ::        &
           trcr_sum             ! total volume*tracer
! 
!EOP
!
! local variables

      integer ::   &
           i, j, nt,               &! counting indices
           ilo, ihi, jlo, jhi       ! beginning and end of physical grid

      hice_sum = c0

      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      do j = jlo, jhi
      do i = ilo, ihi
         hice_sum = hice_sum + tarea(i,j)*hice(i,j)
      enddo
      enddo

      do nt = 1, ntrace
         trcr_sum(nt) = c0
         do j = jlo, jhi
         do i = ilo, ihi
            trcr_sum(nt) = trcr_sum(nt) + tarea(i,j)*hice(i,j)*trcr(i,j,nt)
         enddo
         enddo
      enddo

      end subroutine global_sum
!=======================================================================
!
!BOP
!
! !IROUTINE: global_conservation - check for changes in conserved quantities
!
! !INTERFACE:
!
      subroutine global_conservation (hsum_init,  hsum_final,     &
                                      ntrace,                     &
                                      htsum_init, htsum_final,    &
                                      abort_flag)
!
! !DESCRIPTION:
!
! Check whether values of conserved quantities have changed.
! An error probably means that ghost cells are treated incorrectly.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (r8), intent(in) ::                               & 
         hsum_init   ,&! initial global ice volume
         hsum_final    ! final global ice volume
 
      integer, intent(in) ::                                 &
         ntrace        ! number of tracers

      real (r8), dimension(ntrace), intent(in), optional ::  &
         htsum_init  ,&! initial global ice volume*tracer
         htsum_final   ! final global ice volume*tracer
 
      logical, intent(out), optional ::     &
         abort_flag    ! if true, abort on return
!
!EOP
!
      integer ::                            &
           nt            ! tracer index
 
      real (r8) ::                               &
           diff          ! difference between initial and final values
 
 
      if (present(abort_flag)) abort_flag = .false.
 
      if (hsum_init > eps) then
         diff = hsum_final - hsum_init
         if (abs(diff/hsum_init) > eps) then
            if (present(abort_flag)) abort_flag = .true.
            write (6,*)
            write (6,*) 'Ice volume conservation error'
            write (6,*) 'Initial global volume =', hsum_init
            write (6,*) 'Final global volume =', hsum_final
            write (6,*) 'Fractional error =', abs(diff)/hsum_init
            write (6,*) 'hsum_final-hsum_init =', diff
         endif
      endif
 
      if (present(htsum_init)) then
       do nt = 1, ntrace
         if (abs(htsum_init(nt)) > eps) then
            diff = htsum_final(nt) - htsum_init(nt)
            if (abs(diff/htsum_init(nt)) > eps) then
               if (present(abort_flag)) abort_flag = .true.
               write (6,*)
               write (6,*) 'volume*tracer conserv error'
               write (6,*) 'tracer index =', nt
               write (6,*) 'Initial global volume*tracer =',     &
                                  htsum_init(nt)
               write (6,*) 'Final global volume*tracer =',       &
                                  htsum_final(nt)
               write (6,*) 'Fractional error =',               &
                                  abs(diff)/htsum_init(nt)
               write (6,*) 'htsum_final-htsum_init =', diff
            endif
         endif
       enddo
      endif                     ! present(htsum_init)
 
      end subroutine global_conservation
 
!=======================================================================
!BOP
!
! !IROUTINE: local_max_min - compute local max and min of a scalar field
!
! !INTERFACE:
!
      subroutine local_max_min (nx_block, ny_block,     &
                                nghost,   ntrace,       &
                                trm,                    &
                                tmin,     tmax,         &
                                phimask)
!
! !DESCRIPTION:
!
! At each grid point, compute the local max and min of a scalar
! field phi: i.e., the max and min values in the nine-cell region
! consisting of the home cell and its eight neighbors.
! 
! To extend to the neighbors of the neighbors (25 cells in all),
! follow this call with a call to quasilocal_max_min.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::            &
           nx_block, ny_block, &! block dimensions
           nghost,             &! number of ghost cells
           ntrace               ! number of tracers

      real (r8), intent(in),                 &
           dimension(nx_block,ny_block) ::              &
           phimask             ! ice mask
 
      real (r8), intent(in),                 &
           dimension (nx_block,ny_block,ntrace) ::      &
           trm
 
      real (r8), intent(out),                &
           dimension (nx_block,ny_block,ntrace) ::      &
           tmin         ,&! local min tracer
           tmax           ! local max tracer
!
!EOP
!
      integer ::                        &
           i, j         ,&! horizontal indices
           nt, nt1      ,&! tracer indices
           ilo,ihi,jlo,jhi     ! beginning and end of physical domain
 
      real (r8) ::                                   &
           phi_nw, phi_n, phi_ne ,&! field values in 8 neighbor cells
           phi_w, phi_e,          &
           phi_sw, phi_s, phi_se
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      do nt = 1, ntrace
 
!-----------------------------------------------------------------------
!  Store values of trm in the 8 neighbor cells.
!  If himask = 1, use the true value; otherwise use the home cell value
!  so that non-physical values of phi do not contribute to the gradient.
!-----------------------------------------------------------------------
 
         do j = jlo, jhi
            do i = ilo, ihi
 
               phi_nw = phimask(i-1,j+1) * trm(i-1,j+1,nt)     &
                  + (c1-phimask(i-1,j+1))* trm(i,  j,  nt)
               phi_n  = phimask(i,  j+1) * trm(i,  j+1,nt)     &
                  + (c1-phimask(i,  j+1))* trm(i,  j,  nt)
               phi_ne = phimask(i+1,j+1) * trm(i+1,j+1,nt)     &
                  + (c1-phimask(i+1,j+1))* trm(i,  j,  nt)
               phi_w  = phimask(i-1,j)   * trm(i-1,j,  nt)     &
                  + (c1-phimask(i-1,j))  * trm(i,  j,  nt)
               phi_e  = phimask(i+1,j)   * trm(i+1,j,  nt)     &
                  + (c1-phimask(i+1,j))  * trm(i,  j,  nt)
               phi_sw = phimask(i-1,j-1) * trm(i-1,j-1,nt)     &
                  + (c1-phimask(i-1,j-1))* trm(i,  j,  nt)
               phi_s  = phimask(i,  j-1) * trm(i,  j-1,nt)     &
                  + (c1-phimask(i,  j-1))* trm(i,  j,  nt)
               phi_se = phimask(i+1,j-1) * trm(i+1,j-1,nt)     &
                  + (c1-phimask(i+1,j-1))* trm(i,  j,  nt)
 
!-----------------------------------------------------------------------
!     Compute the minimum and maximum among the nine local cells.
!-----------------------------------------------------------------------
 
               tmax(i,j,nt) = max (phi_nw, phi_n,  phi_ne, phi_w,     &
                      trm(i,j,nt), phi_e,  phi_sw, phi_s,  phi_se)
 
               tmin(i,j,nt) = min (phi_nw, phi_n,  phi_ne, phi_w,     &
                      trm(i,j,nt), phi_e,  phi_sw, phi_s,  phi_se)

            enddo	   ! i
         enddo             ! j
      enddo                ! nt
 
      end subroutine local_max_min
 
!=======================================================================
!BOP
!
! !IROUTINE: quasilocal_max_min - look one grid cell farther away
!
! !INTERFACE:
!
      subroutine quasilocal_max_min (nx_block, ny_block,     &
                                     nghost,   ntrace,       &
                                     tmin,     tmax)
!
! !DESCRIPTION:
!
! Extend the local max and min by one grid cell in each direction.
! Incremental remapping is monotone for the "quasilocal" max and min,
! but in rare cases may violate monotonicity for the local max and min.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                 &
           nx_block, ny_block, &! block dimensions
           nghost,             &! number of ghost cells
           ntrace               ! number of tracers

      real (r8), intent(inout),                   &
           dimension (nx_block,ny_block,ntrace) ::           &
           tmin         ,&! local min tracer
           tmax           ! local max tracer
!
!EOP
!
      integer ::                             &
           i, j          ,&! horizontal indices
           nt            ,&! tracer index
           ilo,ihi,jlo,jhi ! beginning and end of physical domain
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      do nt = 1, ntrace
 
         do j = jlo, jhi
         do i = ilo, ihi
 
            tmax(i,j,nt) =                                                 &
              max (tmax(i-1,j+1,nt), tmax(i,j+1,nt), tmax(i+1,j+1,nt),     &
                   tmax(i-1,j,  nt), tmax(i,j,  nt), tmax(i+1,j,  nt),     &
                   tmax(i-1,j-1,nt), tmax(i,j-1,nt), tmax(1+1,j-1,nt))
 
            tmin(i,j,nt) =                                                 &
              min (tmin(i-1,j+1,nt), tmin(i,j+1,nt), tmin(i+1,j+1,nt),     &
                   tmin(i-1,j,  nt), tmin(i,j,  nt), tmin(i+1,j,  nt),     &
                   tmin(i-1,j-1,nt), tmin(i,j-1,nt), tmin(1+1,j-1,nt))
 
         enddo                  ! i
         enddo                  ! j
 
      enddo
 
      end subroutine quasilocal_max_min
!======================================================================
!
!BOP
!
! !IROUTINE: check_monotonicity - check bounds on new tracer values
!
! !INTERFACE:
!
      subroutine check_monotonicity (nx_block, ny_block,     &
                                     nghost,   ntrace,       &
                                     tmin,     tmax,         &
                                     him,      trm,          &
                                     abort_flag)
!
! !DESCRIPTION:
!
! At each grid point, make sure that the new tracer values
! fall between the local max and min values before transport.
!
! !REVISION HISTORY:
!
! author William H. Lipscomb, LANL
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer, intent(in) ::                 &
           nx_block, ny_block, &! block dimensions
           nghost,             &! number of ghost cells
           ntrace               ! number of tracers
 
      real (r8), intent(in),                      &
           dimension (nx_block,ny_block) ::                  &
           him            ! new ice thickness
 
      real (r8), intent(in),                      &
           dimension (nx_block,ny_block,ntrace) ::           &
           trm            ! new tracers
 
      real (r8), intent(in),                      &
           dimension (nx_block,ny_block,ntrace) ::           &
           tmin         ,&! local min tracer
           tmax           ! local max tracer
 
      logical, intent(out), optional ::      &
         abort_flag    ! if true, abort on return
!
!EOP
!
      integer ::                             &
           i, j           ,&! horizontal indices
           nt, nt1, nt2   ,&! tracer indices
           ilo,ihi,jlo,jhi  ! beginning and end of physical domain
 
      real (r8) ::                                &
           w1, w2         ! work variables
 
      logical, dimension (nx_block, ny_block) ::  &
           l_check        ! if true, check monotonicity
 
      ilo = 1 + nghost
      ihi = nx_block - nghost
      jlo = 1 + nghost
      jhi = ny_block - nghost
 
      if (present(abort_flag)) abort_flag = .false.
 
      do nt = 1, ntrace
 
    !-------------------------------------------------------------------
    ! Load logical array to identify tracers that need checking.
    !-------------------------------------------------------------------
 
         do j = jlo, jhi
            do i = ilo, ihi
               if (him(i,j) > eps) then 
                  l_check(i,j) = .true.
               else
                  l_check(i,j) = .false.
               endif
            enddo
         enddo
 
    !-------------------------------------------------------------------
    ! Make sure new values lie between tmin and tmax
    !-------------------------------------------------------------------
 
         do j = jlo, jhi
         do i = ilo, ihi
 
            if (l_check(i,j)) then
               ! w1 and w2 allow for roundoff error when abs(trm) is big
               w1 = max(c1, abs(tmin(i,j,nt)))
               w2 = max(c1, abs(tmax(i,j,nt)))
               if (trm(i,j,nt) < tmin(i,j,nt)-w1*eps) then
                  if (present(abort_flag)) abort_flag = .true.
                  write (6,*) ' '
                  write (6,*) 'new tracer < tmin'
                  write (6,*) 'i, j, nt =', i, j, nt
                  write (6,*) 'new tracer =', trm (i,j,nt)
                  write (6,*) 'tmin ='      , tmin(i,j,nt)
                  write (6,*) 'ice thickness ='  , him(i,j)
               elseif (trm(i,j,nt) > tmax(i,j,nt)+w2*eps) then
                  if (present(abort_flag)) abort_flag = .true.
                  write (6,*) ' '
                  write (6,*) 'new tracer > tmax'
                  write (6,*) 'i, j, nt =', i, j, nt
                  write (6,*) 'new tracer =', trm (i,j,nt)
                  write (6,*) 'tmax ='      , tmax(i,j,nt)
                  write (6,*) 'ice thickness ='  , him(i,j)
               endif
            endif
 
         enddo                  ! i
         enddo                  ! j
 
      enddo                     ! nt
 
      end subroutine check_monotonicity

!=======================================================================

      end module glissade_remap 

!=======================================================================
