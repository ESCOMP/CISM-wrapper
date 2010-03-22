!=======================================================================
!BOP
!
! !MODULE: glc_glint_interp - drivers for upscaling, downscaling between glc, glint
!
   module glc_glint_interp

! !DESCRIPTION:
!
! This module contains routines for upscaling and downscaling fields between the global 
!  glc grid (with multiple elevation classes per gridcell) and the local Glimmer grid.
!
!lipscomb - to do - Incorporate these subroutines in Glint?
!
! !REVISION HISTORY:
!  Author: William Lipscomb, LANL
!
! !USES:
!
  use glc_kinds_mod
  use glc_constants, only: glc_nec, hec_max, lapse, stdout
  use glc_exit_mod

  use glint_type, only: glint_instance, upscale

!lipscomb - debug
  use glc_constants, only: verbose, itest, jtest, jjtest, itest_local, jtest_local
 
!EOP
!=======================================================================

  private
  public glc_glint_downscaling, get_glc_upscaled_fields
 
  contains

!================================================================================

  subroutine glc_glint_downscaling (instance,            &
                                    tsfc_g,     qice_g,  &
                                    topo_g,     gmask)
 
    use glint_interp, only: interp_to_local
    use glimmer_paramets, only: thk0
 
    ! Downscale fields from the global grid (with multiple elevation classes)
    !  to the local grid.

!lipscomb - to do - Do not pass nec 
    type(glint_instance), intent(inout) :: instance
    real(r8),dimension(:,:,:),intent(in) :: tsfc_g       ! Surface temperature (C)
    real(r8),dimension(:,:,:),intent(in) :: qice_g       ! Surface mass balance (m)
    real(r8),dimension(:,:,:),intent(in) :: topo_g       ! Surface elevation (m)
    integer ,dimension(:,:),  intent(in),optional :: gmask ! = 1 where global data are valid
                                                           ! = 0 elsewhere

    real(r8), parameter :: maskval = 0.0_r8    ! value written to masked out gridcells

    integer ::       &
       nec,          &      ! number of elevation classes
       i, j, n,      &      ! indices 
       nxl, nyl             ! local grid dimensions

 
!lipscomb - to do - Might want to find a less memory-intensive way to do downscaling

    real(r8), dimension(:,:,:), allocatable ::   &
       tsfc_l,    &! interpolation of global sfc temperature to local grid
       qice_l,    &! interpolation of global mass balance to local grid
       topo_l      ! interpolation of global topography in each elev class to local grid

    real(r8) :: fact

    nec = size(qice_g,3)
    nxl = instance%lgrid%size%pt(1)
    nyl = instance%lgrid%size%pt(2)

    allocate(tsfc_l(nxl,nyl,nec))
    allocate(topo_l(nxl,nyl,nec))
    allocate(qice_l(nxl,nyl,nec))

    if (verbose) then
       write (stdout,*) ' ' 
       write (stdout,*) 'Interpolate fields to local grid'
    endif

!   Downscale global fields for each elevation class to local grid
!   Set local fields to zero where interpolation from the global grid is invalid (instance%downs%lmask = 0).

!lipscomb - to do - Is topo_g downscaled correctly?

    if (present(gmask)) then   ! set local field = maskval where the global field is masked out

       do n = 1, nec
          call interp_to_local(instance%lgrid, tsfc_g(:,:,n), instance%downs, localdp=tsfc_l(:,:,n), &
                               gmask = gmask, maskval=maskval)
          call interp_to_local(instance%lgrid, topo_g(:,:,n), instance%downs, localdp=topo_l(:,:,n), &
                               gmask = gmask, maskval=maskval)
          call interp_to_local(instance%lgrid, qice_g(:,:,n), instance%downs, localdp=qice_l(:,:,n), &
                               gmask = gmask, maskval=maskval)
       enddo

    else    ! global field values are assumed to be valid everywhere

       do n = 1, nec
          call interp_to_local(instance%lgrid, tsfc_g(:,:,n), instance%downs, localdp=tsfc_l(:,:,n))
          call interp_to_local(instance%lgrid, topo_g(:,:,n), instance%downs, localdp=topo_l(:,:,n))
          call interp_to_local(instance%lgrid, qice_g(:,:,n), instance%downs, localdp=qice_l(:,:,n))
       enddo

    endif

!lipscomb - debug - Write topo in each elevation class of global cell
    write(stdout,*) ' '
    write(stdout,*) 'Global cell =', itest, jjtest
    do n = 1, nec
       write(stdout,*) n, topo_g(itest,jjtest, n)
    enddo

    do j = 1, nyl
    do i = 1, nxl
 
        if ( (instance%downs%xloc(i,j,1) == itest .and. instance%downs%yloc(i,j,1) == jjtest) .or.  &
             (instance%downs%xloc(i,j,2) == itest .and. instance%downs%yloc(i,j,2) == jjtest) .or.  &
             (instance%downs%xloc(i,j,3) == itest .and. instance%downs%yloc(i,j,3) == jjtest) .or.  &
             (instance%downs%xloc(i,j,4) == itest .and. instance%downs%yloc(i,j,4) == jjtest) ) then
            write(stdout,*) i, j, thk0 * instance%model%geometry%usrf(i,j)
        endif
    enddo
    enddo
    
    if (verbose) then
       i = itest_local
       j = jtest_local
       write (stdout,*) ' ' 
       write (stdout,*) 'Interpolated to local cells: i, j =', i, j
       do n = 1, glc_nec
          write (stdout,*) ' '
          write (stdout,*) 'n =', n
          write (stdout,*) 'tsfc_l =', tsfc_l(i,j,n)
          write (stdout,*) 'topo_l =', topo_l(i,j,n)
          write (stdout,*) 'qice_l =', qice_l(i,j,n)
       enddo
       call flushm(stdout)
    endif
!lipscomb - end debug


!   Interpolate tsfc and qice to local topography using values in the neighboring 
!    elevation classes.
!   If the local topography is outside the bounds of the global elevations classes,
!    extrapolate the temperature using the prescribed lapse rate.

    do j = 1, nyl
    do i = 1, nxl

       usrf = instance%model%geometry%usrf(i,j) * thk0   ! actual sfc elevation (m)

       if (usrf <= topo_l(i,j,1)) then
          instance%acab(i,j) = qice_l(i,j,1)
          instance%artm(i,j) = tsfc_l(i,j,1) + lapse*(topo_l(i,j,1)-usrf)
       elseif (usrf > topo_l(i,j,nec)) then
          instance%acab(i,j) = qice_l(i,j,nec)
          instance%artm(i,j) = tsfc_l(i,j,nec) - lapse*(usrf-topo_l(i,j,nec))
       else
          do n = 2, nec
             if (usrf > topo_l(i,j,n-1) .and. usrf <= topo_l(i,j,n)) then
                fact = (topo_l(i,j,n) - usrf) / (topo_l(i,j,n) - topo_l(i,j,n-1)) 
                instance%acab(i,j) = fact*qice_l(i,j,n-1) + (1._r8-fact)*qice_l(i,j,n)
                instance%artm(i,j) = fact*tsfc_l(i,j,n-1) + (1._r8-fact)*tsfc_l(i,j,n)
                exit
             endif
          enddo
       endif   ! usrf

!lipscomb - debug
       if (verbose) then
          if (i==itest_local .and. j==jtest_local) then
             n = 4  
             write (stdout,*) ' '
             write (stdout,*) 'Interpolated values, i, j, n =', i, j, n
             write (stdout,*) 'usrf =', usrf
             write (stdout,*) 'acab =', instance%acab(i,j)
             write (stdout,*) 'artm =', instance%artm(i,j)
             write (stdout,*) 'topo(n-1) =', topo_l(i,j,n-1)
             write (stdout,*) 'topo(n) =', topo_l(i,j,n)
             write (stdout,*) 'qice(n-1) =', qice_l(i,j,n-1)
             write (stdout,*) 'qice(n) =', qice_l(i,j,n)
             write (stdout,*) 'tsfc(n-1) =', tsfc_l(i,j,n-1)
             write (stdout,*) 'tsfc(n) =', tsfc_l(i,j,n)
             write (stdout,*) 'fact = ', (topo_l(i,j,n) - usrf) / (topo_l(i,j,n) - topo_l(i,j,n-1)) 
          endif
       endif

    enddo  ! i
    enddo  ! j

  end subroutine glc_glint_downscaling

!================================================================================

  subroutine get_glc_upscaled_fields(instance,    nec,      &
                                     nxl,         nyl,      &
                                     nxg,         nyg,      &
                                     gfrac,       gthck,    &
                                     gtopo,       ghflx,    &
                                     groff)

    ! Upscales and returns fields required by glc for passing to coupler
    !
    ! This subroutine replaces get_i_upscaled_fields in glint_type.F90.
 
    use glimmer_paramets

    ! Arguments ----------------------------------------------------------------------------
 
    type(glint_instance),     intent(in)  :: instance      ! the model instance
    integer,                  intent(in)  :: nec           ! number of elevation classes
    integer,                  intent(in)  :: nxl,nyl       ! local grid dimensions 
    integer,                  intent(in)  :: nxg,nyg       ! global grid dimensions 

    real(r8),dimension(nxg,nyg,nec),intent(out) :: gfrac   ! ice-covered fraction [0,1]
    real(r8),dimension(nxg,nyg,nec),intent(out) :: gthck   ! ice thickness (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: gtopo   ! surface elevation (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: ghflx   ! heat flux (m)
    real(r8),dimension(nxg,nyg,nec),intent(out) :: groff   ! runoff/calving flux (kg/m^2/s)
 
    ! Internal variables ----------------------------------------------------------------------
 
    real(r8),dimension(nxl,nyl) :: temp
    real(r8),dimension(nxl,nyl) :: ltopo_temp

    integer :: i, j            ! indices
 
    integer :: il, jl, ig, jg

    ltopo_temp(:,:) = thk0 * instance%model%geometry%usrf(:,:)
    
    if (verbose) then
       ig = itest
       jg = jjtest
       il = itest_local
       jl = jtest_local
       write(stdout,*) 'In get_glc_upscaled_fields'
       write(stdout,*) 'il, jl =', il, jl
       write(stdout,*) 'ig, jg =', ig, jg
       write(stdout,*) 'nxl, nyl =', nxl,nyl
       write(stdout,*) 'nxg, nyg =', nxg,nyg
       write(stdout,*) 'topo =', ltopo_temp(il,jl) 
       call flushm(stdout)
    endif

!lipscomb - debug
    write(stdout,*) ' '
    do jl = jtest_local-5, jtest_local+5
    do il = itest_local-2, itest_local+2
       write(stdout,*) 'il, jl, ltopo:', il, jl, ltopo_temp(il,jl)
    enddo
    enddo
 
    ! ice fraction

    do j = 1, nyl
    do i = 1, nxl
       if (ltopo_temp(i,j) > 0._r8) then
          temp(i,j) = 1._r8
       else
          temp(i,j) = 0._r8
       endif
    enddo
    enddo

    if (verbose) then
       il = itest_local 
       jl = jtest_local 
       write(stdout,*) 'local ifrac =', temp(il, jl)
       write(stdout,*) 'local topo =', ltopo_temp(il,jl)
       write(stdout,*) 'local out_mask =', instance%out_mask(il,jl)
    endif

    call mean_to_global_mec(instance%ups,                       &
                            nxl,                nyl,            &
                            nxg,                nyg,            &
                            nec,                hec_max,        &
                            temp,               gfrac,          &
                            ltopo_temp,         instance%out_mask)

    ! ice thickness

    temp(:,:) = thk0 * instance%model%geometry%thck(:,:)

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                nyl,        &
                            nxg,                nyg,        &
                            nec,                hec_max,    &
                            temp,               gthck,      &
                            ltopo_temp,         instance%out_mask)


    ! surface elevation

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            ltopo_temp,          gtopo,     &
                            ltopo_temp,          instance%out_mask)


    ! heat flux

!lipscomb - to do - Copy runoff into temp array
    temp(:,:) = 0._r8

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            temp,                ghflx,     &
                            ltopo_temp,          instance%out_mask)
 
!lipscomb - to do - Copy runoff into temp array
    temp(:,:) = 0._r8

    call mean_to_global_mec(instance%ups,                   &
                            nxl,                 nyl,       &
                            nxg,                 nyg,       &
                            nec,                 hec_max,   &
                            temp,                groff,     &
                            ltopo_temp,          instance%out_mask)

    if (verbose) then

       write(stdout,*) ' '
       write(stdout,*) 'global ifrac:'
       do n = 1, nec
          write(stdout,*) n, gfrac(ig, jg, n)
       enddo

       write(stdout,*) ' '
       write(stdout,*) 'global gtopo:'
       do n = 1, nec
          write(stdout,*) n, gtopo(ig, jg, n)
       enddo

       write(stdout,*) ' '
       write(stdout,*) 'global gthck:'
       do n = 1, nec
          write(stdout,*) n, gthck(ig, jg, n)
       enddo

!       write(stdout,*) ' '
!       write(stdout,*) 'global ghflx:'
!       do n = 1, nec
!          write(stdout,*) n, ghflx(ig, jg, n)
!       enddo

!       write(stdout,*) ' '
!       write(stdout,*) 'global groff:'
!       do n = 1, nec
!          write(stdout,*) n, groff(ig, jg, n)
!       enddo

    endif    ! verbose

  end subroutine get_glc_upscaled_fields
 
!================================================================================

  subroutine mean_to_global_mec(ups,                &
                                nxl,      nyl,      &
                                nxg,      nyg,      &
                                nec,      hec_max,  &
                                local,    global,   &
                                ltopo,    mask)
 
    ! Upscale from the local domain to a global domain with multiple elevation classes
    ! by areal averaging.
    !
    ! This subroutine is adapted from subroutine mean_to_global in GLIMMER.
    ! The difference is that local topography is upscaled to multiple elevation classes
    !  in each global grid cell.
    !
    ! Note: This method is not the inverse of the interp_to_local routine.
    ! Also note that each local grid cell is assumed to have the same area.
    ! It would be nice to have a more sophisticated routine.
 
    ! Arguments
 
    type(upscale),            intent(in)    :: ups     ! upscaling indexing data
    integer,                  intent(in)    :: nxl,nyl ! local grid dimensions 
    integer,                  intent(in)    :: nxg,nyg ! global grid dimensions 
    integer,                  intent(in)    :: nec     ! number of elevation classes 
    real(r8),dimension(0:nec),intent(in)    :: hec_max ! max elevation in each class 
    real(r8),dimension(nxl,nyl),  intent(in)      :: local   ! data on local grid
    real(r8),dimension(nxg,nyg,nec),intent(out)   :: global  ! data on global grid
    real(r8),dimension(nxl,nyl),  intent(in)      :: ltopo   ! surface elevation on local grid (m)
    integer, dimension(nxl,nyl),intent(in),optional :: mask ! mask for upscaling

!    type(upscale)                :: ups     ! upscaling indexing data
!    integer                      :: nxl,nyl ! local grid dimensions 
!    integer                      :: nxg,nyg ! global grid dimensions 
!    integer                      :: nec     ! number of elevation classes 
!    real(r8),dimension(0:nec)     :: hec_max ! max elevation in each class 
!    real(r8),dimension(nxl,nyl)     :: local   ! data on local grid
!    real(r8),dimension(nxg,nyg,nec) :: global  ! data on global grid
!    real(r8),dimension(nxl,nyl)    :: ltopo   ! surface elevation on local grid (m)
!    integer, dimension(nxl,nyl) :: mask ! mask for upscaling
 
    ! Internal variables
 
    integer ::  &
       i, j, n,    &! indices
       ig, jg       ! indices

    integer, dimension(nxl,nyl) ::  &
        tempmask,    &! temporary mask
        gboxec        ! elevation class associated with local topography

    integer, dimension(nxg,nyg,nec) ::  &
        gnumloc       ! no. of local cells within each global cell in each elevation class

    integer :: il, jl
    real(r8) :: lsum, gsum
 
    if (present(mask)) then
       tempmask(:,:) = mask(:,:)
    else
       tempmask(:,:) = 1
    endif
 
    ! Compute global elevation class for each local grid cell
    ! Also compute number of local cells within each global cell in each elevation class

    gboxec(:,:) = 0
    gnumloc(:,:,:) = 0

    do n = 1, nec
       do j = 1, nyl
       do i = 1, nxl
          if (ltopo(i,j) >= hec_max(n-1) .and. ltopo(i,j) < hec_max(n)) then
             gboxec(i,j) = n
             if (tempmask(i,j)==1) then
                ig = ups%gboxx(i,j)
                jg = ups%gboxy(i,j)
                gnumloc(ig,jg,n) = gnumloc(ig,jg,n) + 1
             endif
          endif
       enddo
       enddo
    enddo

    global(:,:,:) = 0._r8

    do j = 1, nyl
    do i = 1, nxl
       ig = ups%gboxx(i,j)
       jg = ups%gboxy(i,j)
       n = gboxec(i,j)
!lipscomb - bug check
       if (n==0) then
          write(stdout,*) 'Bug, local topography out of bounds'
          write(stdout,*) 'i, j, topo:', i, j, ltopo(i,j)
          write(stdout,*) 'hec_max(0) =', hec_max(0)
          call exit_glc(sigAbort, 'Local topography out of bounds')
       endif

!lipscomb - debug
       if (verbose .and. ig==itest .and. jg==jjtest) then
!!          write(stdout,*) 'ig, jg, il, jl = ', ig, jg, i, j
       endif

!lipscomb - debug
       if (verbose .and. i==itest_local .and. j==jtest_local) then
          write(stdout,*) ' '
          write(stdout,*) 'il, jl =', i, j
          write(stdout,*) 'ig, jg, n =', ig, jg, n
          write(stdout,*) 'Old global val =', global(ig,jg,n)
          write(stdout,*) 'local, mask =', local(i,j), tempmask(i,j)
       endif

       global(ig,jg,n) = global(ig,jg,n) + local(i,j)*tempmask(i,j)

!lipscomb - debug
       if (verbose .and. i==itest_local .and. j==jtest_local) then
          write(stdout,*) 'New global val =', global(ig,jg,n)
       endif

    enddo
    enddo
 
    do n = 1, nec
       do j = 1, nyg
       do i = 1, nxg
          if (gnumloc(i,j,n) /= 0) then
             global(i,j,n) = global(i,j,n) / gnumloc(i,j,n)
          else
             global(i,j,n) = 0._r8
          endif
       enddo
       enddo
    enddo

    ! conservation check

    lsum = 0._r8
    do j = 1, nyl
    do i = 1, nxl
       lsum = lsum + local(i,j)*tempmask(i,j)
    enddo
    enddo

    gsum = 0._r8
    do n = 1, nec
    do j = 1, nyg
    do i = 1, nxg
       gsum = gsum + global(i,j,n)*gnumloc(i,j,n)
    enddo
    enddo
    enddo

!lipscomb - to do - Use a less arbitrary error threshold
    if (abs(gsum-lsum) > 1.0) then 
       write(stdout,*) 'local and global sums disagree'
       write (stdout,*) 'lsum, gsum =', lsum, gsum 
       call exit_glc(sigAbort, 'Upscaling conservation error')
    endif

  end subroutine mean_to_global_mec
  
!================================================================================

  end module glc_glint_interp

!================================================================================


