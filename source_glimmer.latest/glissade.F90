! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
! +                                                           + 
! +  glissade.F90                                             + 
! +                                                           + 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! This is the driver module for GLISSADE, a set of prototype 
! ice sheet dynamics modules.
!
! Author: William Lipscomb
!         Los Alamos National Laboratory
!         Group T-3, MS B216
!         Los Alamos, NM 87545
!         USA
!         <lipscomb@lanl.gov>
!
! The main development so far is implementation of an incremental
! remapping transport scheme based on code from CICE, a sea ice
! model originally developed at Los Alamos National Lab.
!
! At some future time, this code may become part of GLIMMER.
!
! For now, the glissade code is accessed by use statements in glide.F90.
!
! Code with the 'glissade' prefix lives in a source directory
! separate from the rest of the GLIMMER source.  To compile using
! this source code, you must either set the appropriate path in
! Makefile.In or copy the glissade code into the fortran source
! directory.
!  
! To avoid ifdef's, there is a dummy module called glissade.F90
! in the glimmer source.  If called, this module will return an
! error message.  To run with the actual glissade code, replace
! or rename the dummy module.
!  
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

  module glissade

    use glimmer_global, only: sp, dp
    use glc_constants

    implicit none
    private

    public :: init_glissade, thck_remap_evolve

    contains

!****************************************************************************
!lipscomb - For now, this subroutine is just a wrapper for init_remap 

  subroutine init_glissade(model)

    use glide_types
    use glissade_remap

    ! input arguments

    type(glide_global_type), intent(inout) :: model

    call init_remap(model%general%ewn,    & 
                    model%general%nsn,    &
                    model%numerics%dew,   &
                    model%numerics%dns,   & 
                    model%gridwk)

  end subroutine init_glissade

!****************************************************************************
!lipscomb - This subroutine is patterned after thck_lin_evolve from GLIMMER.
!lipscomb - Later on, this subroutine will be moved down a level and
!           replaced here by a driver module.

  subroutine thck_remap_evolve(model,newtemps,logunit,l_advtemp)

    ! Solve the continuity equation for ice thickness and temperature, as well
    !  as any tracers, using an incremental remapping transport scheme.
    ! This subroutine first computes the horizontal velocities, then calls the
    !  remapping driver.
 
    use glimmer_physcon, only: scyr
    use glimmer_paramets, only: thk0, tim0

    use glissade_remap

!lipscomb - Later, may want to pass in all the model information needed
!            instead of using glide_types
!lipscomb - Later, may want to write glissade_velo module
    use glide_types
    use glide_velo
    use glide_thck, only: geomders, stagvarb
 
    implicit none
 
    ! in-out arguments
 
    type(glide_global_type), intent(inout) :: model
    integer,intent(in) :: logunit      ! unit for logging
 
    logical, intent(in) ::   &
         newtemps,    &! true when recalculating Glen's A
         l_advtemp     ! if true, advect temperature and tracers
                       ! if false, advect thickness only
 
    ! local variables
 
    integer, parameter ::   &
          ntrace = 2         ! no. of tracer fields
                             ! 1 = temperature, 2 = ice age
 
    logical, parameter ::  &
        predict_correct = .false.      ! if true, use predicted thickness
                                       ! to correct the velocity 
    integer :: i, j, k, k1, k2, nt
 
    integer ::   &
         ewn,    &! model%general%ewn
         nsn,    &! model%general%nsn
         upn,    &! model%general%upn, number of vertical levels
         nlyr     ! number of ice layers, upn - 1
 
    real(dp), dimension(model%general%ewn, model%general%nsn) ::     &
         workh,      &! work array for thck
         worku,      &! work array for uvel
         workv,      &! work array for vvel
                      ! NOTE: These array bounds are different from uvel, vvel!
         e1, e2       ! internal energy before and after vertical remapping
 
    real(dp), dimension(model%general%ewn, model%general%nsn,   &
                        model%general%upn-1) ::     &
         workl        ! thickness of a single layer 
 
    real(dp), dimension(model%general%ewn, model%general%nsn,     &
                        ntrace,            0:model%general%upn) ::  &
         workt        ! tracer array (temperature, age)
                      ! Note: Array bounds are different from temper%temp
 
    real(dp), dimension(model%general%upn-1) :: &
         dsigma       ! model%velowk%dups, fractional thickness in layer k
      
    real(dp), dimension(model%general%ewn, model%general%nsn,   &
                        0:model%general%upn-1) ::     &
         zi1,        &! depths of layer interfaces after horizontal remapping
                      ! and accumulation/ablation
         zi2          ! depths of layer interfaces in sigma coordinates
 
    real (dp), dimension(model%general%ewn, model%general%nsn, ntrace) ::  &
         ht_init,    &! initial sum over ice sheet of h*tracer
         ht_final     ! final sum over ice sheet of h*tracer
 
    real(dp) ::   &
         dt,             &! time step (s)
         hlyr,           &! layer thickness
         hmlt,           &! thickness of melted ice
         dz,             &! difference of two depths
         grad             ! tracer gradient
 
    logical, parameter ::   &
         monotonicity_check = .true.       ,&! monotonicity check for remapping
         horiz_conservation_check = .true. ,&! bug check for horizontal remapping
         vert_conservation_check = .true.    ! bug check for vertical remapping
 
    !------------------------------------------------------------------
    ! Initialise
    !------------------------------------------------------------------
 
    ewn = model%general%ewn
    nsn = model%general%nsn
    upn = model%general%upn
    nlyr = upn - 1
    dsigma(:) = model%velowk%dups(:)
 
    worku(:,:) = c0
    workv(:,:) = c0
    workh(:,:) = c0
    workt(:,:,:,:) = c0
 
    if (model%geometry%empty) then
 
       ! surface ablation/accumulation
 
       model%geometry%thck(:,:) = dmax1(0.0d0,model%geometry%thck(:,:) &
                                 + model%climate%acab(:,:) * model%pcgdwk%fc2(2))
#ifdef DEBUG       
       print *, "* thck empty - net accumulation added", model%numerics%time
#endif
 
    else    ! not empty
 
    !------------------------------------------------------------------
    ! Calculate basal velocity
    !------------------------------------------------------------------
 
       !lipscomb - Set flag = 0 to compute basal velocity as linear function 
       !           of gravitational driving stress.  
       !lipscomb - Not sure why the flag is not simply set to
       !           model%glide_options%which_slip
 
       call slipvelo(model,                        &
                     0,                            &
                     model%velocity%btrc,          &
                     model%velocity%ubas,          &
                     model%velocity%vbas)
 
    !------------------------------------------------------------------
    ! Calculate horizontal velocity field
    !------------------------------------------------------------------
 
       !lipscomb - With flag = 0, this subroutine computes uvel and vvel,
       !           following Payne and Dongelmans eq. 8.  
 
       call zerovelo (model%velowk,             &
                      model%numerics%sigma,     &
                      0,                        &  ! Payne & Dongelmans
                      model%geomderv%stagthck,  &
                      model%geomderv%dusrfdew,  &
                      model%geomderv%dusrfdns,  &
                      model%temper%flwa,        &
                      model%velocity%ubas,      &
                      model%velocity%vbas,      &
                      model%velocity%uvel,      &  ! uvel, vvel are needed
                      model%velocity%vvel,      &
                      model%velocity%uflx,      &  ! uflx, vflx are not needed
                      model%velocity%vflx,      &  ! 
                      model%velocity%diffu)        ! diffu not needed
 
       if (predict_correct) then
 
    !------------------------------------------------------------------
    ! To reduce the time discretization error, there is the option
    ! of predicting the thickness field at time n + 1/2, then 
    ! correcting the velocity based on the mid-time thickness.
    !------------------------------------------------------------------
 
          workh(:,:) = model%geometry%thck(:,:)
          workt(:,:,1,1) = c1   ! dummy tracer field
 
          ! compute vertical average velocity
          ! Note: uvel and vvel have dimensions(ewn-1, nsn-1, upn)
          !       worku and workv have dimensions (ewn, nsn)
          !       worku and workv are initialized to zero above
 
          do k = 1, nlyr
             do j = 1, nsn-1
             do i = 1, ewn-1
                worku(i,j) = worku(i,j)   &
                           + model%velocity%uvel(k,i,j) * dsigma(k)
                workv(i,j) = workv(i,j)   &
                           + model%velocity%vvel(k,i,j) * dsigma(k)
             enddo
             enddo
          enddo
 
          ! predict thickness at time n + 1
 
          call remap_driver (ewn,                    &
                             nsn,                    &
                             1,                      &  ! no. of tracer fields
                             1,                      &  ! no. of ghost cells
                             model%gridwk,           &  ! grid quantities
                             model%numerics%dt,      &  ! time step (s)
                             worku,                  &  ! uvel
                             workv,                  &  ! vvel
                             workh,                  &  ! thck
                             workt(:,:,1,1),         &  ! dummy tracer
                             horiz_conservation_check, &
                             monotonicity_check)
 
          ! Given the predicted thickness at time n + 1, estimate the thickness
          ! at time n + 1/2
 
          workh(:,:) = p5 * (model%geometry%thck(:,:) + workh(:,:))
 
          ! Correct H and grad(S) at grid cell corners
 
          call stagvarb(workh,                         &
                        model%geomderv%stagthck,       &
                        ewn,                           &
                        nsn) 
 
          call geomders(model%numerics,                &
                        model%geometry%usrf,           &
                        model%geomderv%stagthck,       & 
                        model%geomderv%dusrfdew,       &
                        model%geomderv%dusrfdns)
 
          ! Correct the velocities
 
          call zerovelo (model%velowk,             &
                         model%numerics%sigma,     &
                         0,                        &
                         model%geomderv%stagthck,  &
                         model%geomderv%dusrfdew,  &
                         model%geomderv%dusrfdns,  &
                         model%temper%flwa,        &
                         model%velocity%ubas,      &
                         model%velocity%vbas,      &
                         model%velocity%uvel,      &
                         model%velocity%vvel,      &
                         model%velocity%uflx,      & ! uflx, vflx, and diffu
                         model%velocity%vflx,      & ! not needed for remapping
                         model%velocity%diffu)        
 
       endif   ! predict_correct
 
    !------------------------------------------------------------------
    ! Calculate flow factor A
    !------------------------------------------------------------------
 
       if (newtemps) then
          call velo_integrate_flwa (model%velowk,             &
                                    model%geomderv%stagthck,  &
                                    model%temper%flwa)
       endif
 
       if (l_advtemp) then
 
       !---------------------------------------------------------------
       ! Advance the thickness, temperature, and tracers in each ice layer.
       !---------------------------------------------------------------
 
          ! Increment ice age 
          ! Note: If l_advtemp is false, ice age is not updated or advected.
 
          model%geometry%age(:,:,:) = model%geometry%age(:,:,:)  &
                                    + model%numerics%dt
 
          do k = 1, nlyr
 
          ! Fill work arrays of the desired dimensions.
          ! Note: Work array dimensions are different from glimmer dimensions.
          !  model%velocity%uvel and vvel are (ewn-1, nsn-1, upn)
          !  model%temper%temp is (upn, 0:ewn+1, 0:nsn+1)
          !  worku and workv are (ewn, nsn),
          !  workt is (ewn, nwn, 0:upn)
 
             do j = 1, nsn
             do i = 1, ewn
                workl(i,j,k)   = model%geometry%thck(i,j) * dsigma(k)
                workt(i,j,1,k) = model%temper%temp(k,i,j)
                workt(i,j,2,k) = model%geometry%age(k,i,j)
             enddo
             enddo
 
             do j = 1, nsn-1
             do i = 1, ewn-1
                worku(i,j) = model%velocity%uvel(k,i,j)
                workv(i,j) = model%velocity%vvel(k,i,j)
             enddo
             enddo
 
          ! remap the thickness and tracer fields
 
!lipscomb - Pretend there is a layer of ghost cells so remapping loops 
!           are correct.
 
             call remap_driver(ewn,                    &
                               nsn,                    &
                               ntrace,                 &! no. of tracer fields
                               1,                      &! no. of ghost cells
                               model%gridwk,           &! grid quantities
                               model%numerics%dt,      &! time step (s)
                               worku(:,:),             &! uvel
                               workv(:,:),             &! vvel
                               workl(:,:,k),           &! layer thickness
                               workt(:,:,:,k),         &! tracers
                               horiz_conservation_check, &
                               monotonicity_check)
          enddo   ! k
 
          !------------------------------------------------------------
          ! Surface ablation/accumulation
          ! Compute thickness, temperature and age changes at surface.
          !------------------------------------------------------------
 
          workh(:,:) = model%climate%acab(:,:) * model%numerics%dt
 
          do j = 1, nsn
          do i = 1, ewn
             if (workh(i,j) > c0) then   ! surface accumulation
                k = 1
                hlyr = workl(i,j,k) + workh(i,j)
 
             ! layer 1 temperature 
                workt(i,j,1,k) = (workl(i,j,k)*workt(i,j,1,k)  &
                                + workh(i,j)*model%climate%artm(i,j)) / hlyr
 
             ! layer 1 age (sfc accumulation has mean age dt/2)
                workt(i,j,2,k) = (workl(i,j,k)*workt(i,j,2,k)  &
                                + workh(i,j)*p5*model%numerics%dt) / hlyr
 
                workl(i,j,k) = hlyr
 
             elseif (workh(i,j) < c0) then  ! surface ablation
                hmlt = -workh(i,j)
                do k = 1, nlyr
                   if (hmlt > workl(i,j,k)) then
                      hmlt = hmlt - workl(i,j,k)
                      workl(i,j,k) = c0  
                      workt(i,j,1,k) = c0  ! temperature
                      workt(i,j,2,k) = c0  ! age
                   elseif (hmlt > c0) then
                      workl(i,j,k) = workl(i,j,k) - hmlt
                      hmlt = c0
                      exit   ! k loop
                   endif
                enddo  ! k
             ! Note: If hmlt > 0 after k loop, energy is not conserved
             endif     ! workh > 0.
          enddo        ! i
          enddo        ! j
 
          !------------------------------------------------------------
          ! In each column of ice, remap the tracers to standard sigma layers.
          !------------------------------------------------------------
 
          ! Update ice thickness
          workh(:,:) = c0
          do k = 1, nlyr
             workh(:,:) = workh(:,:) + workl(:,:,k)
          enddo
       
          if (vert_conservation_check) then
             ! compute sum of h*tracer for each column 
             ht_init(:,:,:) = c0
             do k = 1, nlyr
                do nt = 1, ntrace
                   do j = 1, nsn
                   do i = 1, ewn
                      ht_init(i,j,nt) = ht_init(i,j,nt)   &
                                        + workl(i,j,k)*workt(i,j,nt,k)
                   enddo
                   enddo
                enddo
             enddo
          endif   ! vert_conservation_check
 
          !------------------------------------------------------------
          ! Compute depths at each level, before and after vert remapping.
          !------------------------------------------------------------
 
          zi1(:,:,0) = c0
          zi2(:,:,0) = c0
          
          do k = 1, nlyr-1
             zi1(:,:,k) = zi1(:,:,k-1) + workl(:,:,k)          ! before
             zi2(:,:,k) = zi2(:,:,k-1) + workh(:,:)*dsigma(k)  ! after
          enddo
          
          zi1(:,:,nlyr) = workh(:,:)
          zi2(:,:,nlyr) = workh(:,:)
 
          !------------------------------------------------------------
          ! Set tracer values at surface and base.
          !------------------------------------------------------------
 
          do j = 1, nsn
          do i = 1, ewn
 
             ! temperature
             workt(i,j,1,0)      = model%climate%artm(i,j)     ! sfc air temp
             workt(i,j,1,nlyr+1) = model%temper%temp(upn,i,j)  ! basal temp
             
             ! age
             workt(i,j,2,0) = c0    ! top sfc age = 0.
             dz = p5 * (zi1(i,j,nlyr) - zi1(i,j,nlyr-2))
             if (dz > eps) then     ! extrapolate gradient to base
                grad = (workt(i,j,2,nlyr) - workt(i,j,2,nlyr-1)) / dz
                workt(i,j,2,nlyr+1) = workt(i,j,2,nlyr)  &
                                    + p5*workl(i,j,nlyr)*grad
             else                   ! use bottom-layer value at base
                workt(i,j,2,nlyr+1) = workt(i,j,2,nlyr)
             endif
          enddo     ! i
          enddo     ! j
 
          !------------------------------------------------------------
          ! Conservative, 2nd-order accurate vertical remapping
          ! Reduces to 1st order where needed to preserve monotonicity
          !------------------------------------------------------------
 
          call vertical_remap(ewn,      nsn,       &
                              ntrace,   nlyr,      &
                              zi1,      zi2,       &
                              workt)
 
          if (vert_conservation_check) then
             ! compute sum of h*tracer for each column
             ht_final(:,:,:) = c0
             do k = 1, nlyr
                do nt = 1, ntrace
                   do j = 1, nsn
                   do i = 1, ewn
                      ht_final(i,j,nt) = ht_final(i,j,nt) &
                                       + workh(i,j)*dsigma(k)*workt(i,j,nt,k)
                   enddo
                   enddo
                enddo
             enddo
 
             ! compare to initial value
             do nt = 1, ntrace
                do j = 1, nsn
                do i = 1, ewn
                   if (abs(ht_init(i,j,nt)) >= eps) then 
                      if (abs( (ht_final(i,j,nt)-ht_init(i,j,nt))  &
                              / ht_init(i,j,nt) ) > eps) then
                         print*, 'Vertical remapping: tracer conservation error'
                         print*, 'i, j, nt, ht_init, ht_final =',  &
                                  i, j, nt, ht_init(i,j,nt), ht_final(i,j,nt)
                         stop
                      endif
                   endif
                enddo
                enddo
             enddo
 
          endif   ! vert_conservation_check
 
          !---------------------------------------------------------------
          ! update thickness and tracer arrays
          !---------------------------------------------------------------
 
          model%geometry%thck(:,:) = workh(:,:)
          
          do k = 1, nlyr
             do j = 1, nsn
                do i = 1, ewn
                   model%temper%temp(k,i,j) = workt(i,j,1,k)
                   model%geometry%age(k,i,j) = workt(i,j,2,k)
                enddo
             enddo
          enddo
          
          !------------------------------------------------------------
          ! Apply periodic ew BC to temperature field 
          !------------------------------------------------------------
          if (model%options%periodic_ew.eq.1) then 
             model%temper%temp(:,0,:)     = model%temper%temp(:,ewn-2,:)
             model%temper%temp(:,1,:)     = model%temper%temp(:,ewn-1,:)
             model%temper%temp(:,ewn,:)   = model%temper%temp(:,2,:)
             model%temper%temp(:,ewn+1,:) = model%temper%temp(:,3,:)
          end if
 
       else   ! advect thickness only
 
          workt(:,:,1,1) = c1   ! dummy tracer field
 
          ! compute vertical average velocity
          ! Note: uvel and vvel have dimensions(ewn-1, nsn-1, upn)
          !       worku and workv have dimensions (ewn, nsn)
 
          worku(:,:) = c0
          workv(:,:) = c0
          do k = 1, nlyr
             do j = 1, nsn-1
             do i = 1, ewn-1
                worku(i,j) = worku(i,j)   &
                           + model%velocity%uvel(k,i,j) * dsigma(k)
                workv(i,j) = workv(i,j)   &
                           + model%velocity%vvel(k,i,j) * dsigma(k)
             enddo
             enddo
          enddo
 
          !------------------------------------------------------------
          ! Advance the thickness.
          !------------------------------------------------------------
 
          call remap_driver (ewn,                    &
                             nsn,                    &
                             1,                      &  ! no. of tracer fields
                             1,                      &  ! no. of ghost cells
                             model%gridwk,           &  ! grid quantities
                             model%numerics%dt,      &  ! time step (s)
                             worku,                  &  ! vert average uvel
                             workv,                  &  ! vert average vvel
                             model%geometry%thck,    &  ! total thickness
                             workt(:,:,1,1),         &  ! dummy tracer
                             horiz_conservation_check, &!
                             monotonicity_check)
 
          !------------------------------------------------------------
          ! surface ablation/accumulation
          !------------------------------------------------------------
 
          workh(:,:) = model%climate%acab(:,:) * model%numerics%dt
 
          model%geometry%thck(:,:) = max(c0, model%geometry%thck(:,:) &
                                                         + workh(:,:))
 
       endif     ! l_advtemp
 
    end if   ! empty
 
  end subroutine thck_remap_evolve
 
!****************************************************************************
!lipscomb - Later, this subroutine will be moved to a lower level.

  subroutine vertical_remap(ewn,      nsn,       &
                            ntrace,   nlyr,      &
                            zi1,      zi2,       &
                            trcr)
 
    ! Conservative remapping of tracer fields from one set of vertical 
    ! coordinates to another.  Tracer fields are reconstructed linearly
    ! in each layer.  The remapping is second-order accurate except
    ! where tracer gradients are limited to preserve monotonicity.
 
    use glimmer_global, only : dp
    use glimmer_paramets
 
    implicit none
 
    ! in-out arguments
 
    integer, intent(in) ::  &
         ewn, nsn,   &! number of cells in EW and NS directions
         ntrace,     &! number of tracer fields
         nlyr         ! number of vertical layers
 
    real(dp), dimension (ewn, nsn, 0:nlyr), intent(in) ::  &
         zi1,        &! layer interfaces in old coordinate system
                      ! zi1(0) = 0. = value at top surface
                      ! zi1(k) = depth of lower interface of layer k
         zi2          ! layer interfaces in new coordinate system
                      ! Note: zi1(0) = zi2(0)
                      !       zi1(nlyr) = zi2(nlyr)
 
    real(dp), dimension (ewn, nsn, ntrace, 0:nlyr+1), intent(inout) ::   &
         trcr         ! tracer field to be remapped
                      ! trcr(0) = value at top surface
                      ! tracer(k) = value at midpoint of layer k
                      ! trcr(nlyr+1) = value at bottom surface
 
    ! local variables
 
    integer :: i, j, k, k1, k2, nt
 
    real(dp), dimension(ewn,nsn,ntrace,nlyr) ::       &
         gradt,      &! tracer gradient within a layer
         htsum        ! sum of thickness*tracer in a layer
         
    real(dp), dimension(ewn, nsn, 0:nlyr+1) ::        &
         zmid,       &! depths of layer midpoints for zi1
         lmask        ! = 1. if layer thickness > 0, else = 0.
 
    real(dp), dimension(ewn, nsn, nlyr) ::        &
         hlyr         ! layer thickness
 
    real(dp) ::   &
         dz,             &! difference of two depths
         zlo,            &! lower z value of overlap region
         zhi,            &! higher z value of overlap region
         zav,            &! average of two depths
         tup, tdn,       &! tracer value in layers above and below
         tmax, tmin,     &! max/min tracer value in a layer and its neighbors
         tzmax, tzmin,   &! max/min value of reconstructed tracer in layer
         wk1, wk2         ! work variables
 
       ! initialise
       gradt(:,:,:,:) = c0
       htsum(:,:,:,:) = c0
 
       !-----------------------------------------------------------------
       ! Compute midpoint and thickness of each layer.
       ! Tracer value is assumed to reside at zmid.
       !-----------------------------------------------------------------
 
       zmid(:,:,0) = c0
       do k = 1, nlyr
          zmid(:,:,k) = p5 * (zi1(:,:,k-1) + zi1(:,:,k))
          hlyr(:,:,k) = zi1(:,:,k) - zi1(:,:,k-1)
       enddo
       zmid(:,:,nlyr+1) = zi1(:,:,nlyr)
 
       !-----------------------------------------------------------------
       ! Compute mask.
       ! Layers with zero thickness are masked out so that the 
       !  tracer values in these layers do not affect the gradient. 
       ! Set lmask = 1 at top and bottom boundaries.
       !-----------------------------------------------------------------
 
       do k = 1, nlyr
          do j = 1, nsn
             do i = 1, ewn
                if (hlyr(i,j,k) > c0) then
                   lmask(i,j,k) = c1
                else
                   lmask(i,j,k) = c0
                endif
             enddo
          enddo
       enddo
 
       lmask(:,:,0) = c1
       lmask(:,:,nlyr+1) = c1
 
       !-----------------------------------------------------------------
       ! Compute tracer gradients within each layer
       ! Note: Skipping this loop gives a 1st-order accurate remapping.
       !-----------------------------------------------------------------
 
       do k = 1, nlyr
          do nt = 1, ntrace
             do j = 1, nsn
                do i = 1, ewn
                   if (hlyr(i,j,k) > c0) then
 
                ! Load values in neighbor cells
                ! For cells of zero thickness, substitute the value in cell k
 
                      tup =     lmask(i,j,k-1)  * trcr(i,j,nt,k-1)  &
                          + (c1-lmask(i,j,k-1)) * trcr(i,j,nt,k) 
                      tdn =     lmask(i,j,k+1)  * trcr(i,j,nt,k+1)  &
                          + (c1-lmask(i,j,k+1)) * trcr(i,j,nt,k) 
 
                      dz = zmid(i,j,k+1) - zmid(i,j,k-1)
                      gradt(i,j,nt,k) = (tdn - tup) / dz
             
                ! max and min deviations of tracer values in layer k-1, k, k+1
                ! from value in layer k
                      tmax = max(tup,trcr(i,j,nt,k),tdn) - trcr(i,j,nt,k)
                      tmin = min(tup,trcr(i,j,nt,k),tdn) - trcr(i,j,nt,k)
 
                ! max and min deviation of reconstructed values from mean value
                      wk1 = gradt(i,j,nt,k)*(zi1(i,j,k-1) - zmid(i,j,k))
                      wk2 = gradt(i,j,nt,k)*(zi1(i,j,k) - zmid(i,j,k))
                      tzmax = max(wk1, wk2)
                      tzmin = min(wk1, wk2)
 
                ! Limit tracer gradients
                
                      if (abs(tzmin) > c0) then
                         wk1 = max(c0, tmin/tzmin)
                      else
                         wk1 = c1
                      endif
                      
                      if (abs(tzmax) > c0) then
                         wk2 = max(c0, tmax/tzmax)
                      else
                         wk2 = c1
                      endif
 
                      gradt(i,j,nt,k) = gradt(i,j,nt,k) * min(c1, wk1, wk2)
                      
                   endif  ! hlyr > c0
                enddo     ! i
             enddo        ! j
          enddo           ! nt
       enddo              ! k
       
       ! new layer thicknesses
       do k = 1, nlyr
          hlyr(:,:,k) = zi2(:,:,k) - zi2(:,:,k-1)
       enddo
 
       !-----------------------------------------------------------------
       ! Compute sum of h*T for each new layer (k2) by integrating
       ! over the regions of overlap with old layers (k1).
       ! The basic formula is as follows:
       !
       ! int_zlo^zhi [T(z) dz] = int_zlo^zhi [trcr + gradt*(z - zmid)] dz
       !                       = dz * [trcr + gradt*(zav - zmid)] 
       ! where dz = zhi - zlo, zav = (zhi+zlo)/2
       !
       ! Note: It might be worth trying a more efficient
       !       search algorithm if the number of layers is large.
       !       This algorithm scales as nlyr^2.
       !-----------------------------------------------------------------
 
       do k2 = 1, nlyr
          do k1 = 1, nlyr
             do nt = 1, ntrace
                do j = 1, nsn
                   do i = 1, ewn
                      zhi = min (zi1(i,j,k1),   zi2(i,j,k2)) 
                      zlo = max (zi1(i,j,k1-1), zi2(i,j,k2-1))
                      dz = max(zhi - zlo, c0)
                      zav = p5 * (zlo + zhi)
                      htsum(i,j,nt,k2) = htsum(i,j,nt,k2)    &
                                       + dz * (trcr(i,j,nt,k1) & 
                                       + gradt(i,j,nt,k1)*(zav - zmid(i,j,k1)))
                   enddo   ! i
                enddo      ! j
             enddo         ! nt
          enddo            ! k1
       enddo               ! k2
 
       !-----------------------------------------------------------------
       ! Compute tracers in new layers (zi2)
       !-----------------------------------------------------------------
 
       trcr(:,:,:,:) = c0
       do k = 1, nlyr
          do nt = 1, ntrace
             do j = 1, nsn
                do i = 1, ewn
                   if (hlyr(i,j,k) > c0) then
                      trcr(i,j,nt,k) = htsum(i,j,nt,k) / hlyr(i,j,k)
                   endif
                enddo   ! i
             enddo      ! j
          enddo         ! nt
       enddo            ! k
 
  end subroutine vertical_remap

!*************************************************************************

  end module glissade

!*************************************************************************
