! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                           +
! +  glissade_thck.f90                                        + 
! +                                                           +
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 
! Copyright (C) 2004 GLIMMER contributors - see COPYRIGHT file 
! for list of contributors.
!
! This program is free software; you can redistribute it and/or 
! modify it under the terms of the GNU General Public License as 
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful, 
! but WITHOUT ANY WARRANTY; without even the implied warranty of 
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License 
! along with this program; if not, write to the Free Software 
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 
! 02111-1307 USA
!
! GLIMMER is maintained by:
!
! Ian Rutt
! School of Geographical Sciences
! University of Bristol
! University Road
! Bristol
! BS8 1SS
! UK
!
! email: <i.c.rutt@bristol.ac.uk> or <ian.rutt@physics.org>
!
! GLIMMER is hosted on NeSCForge:
!
! http://forge.nesc.ac.uk/projects/glimmer/
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!lipscomb - This module began as a copy of glide_thck.F90.  I will be introducing
!           some new algorithms that I can compare with standard GLIMMER.

#ifdef HAVE_CONFIG_H
#include <config.inc>
#endif

module glissade_thck

  use glide_types

!lipscomb - debug
  use glissade_remap, only: itest, jtest, debug

  private
  public :: glissade_init_thck, glissade_thck_nonlin_evolve, glissade_thck_lin_evolve, &
            glissade_stagvarb, glissade_geomders, glissade_timeders, glissade_stagleapthck

!lipscomb - remove these subroutines later
  public :: stagvar_cgrid, geomders_cgrid

#ifdef DEBUG_PICARD
  ! debugging Picard iteration
  integer, private, parameter :: picard_unit=101
  real, private, parameter    :: picard_interval=500.
  integer, private            :: picard_max=0
#endif

contains

  subroutine glissade_init_thck(model)
!lipscomb - At present this subroutine is not called; init_thck is called from glide.

    !*FD initialise work data for ice thickness evolution
    use glimmer_log
    implicit none
    type(glide_global_type) :: model

    
    model%pcgdwk%fc2 = (/ model%numerics%alpha * model%numerics%dt / (2.0d0 * model%numerics%dew * model%numerics%dew), &
         model%numerics%dt, (1.0d0-model%numerics%alpha) / model%numerics%alpha, &
         1.0d0 / model%numerics%alpha, model%numerics%alpha * model%numerics%dt / &
         (2.0d0 * model%numerics%dns * model%numerics%dns), 0.0d0 /) 

#ifdef DEBUG_PICARD
    call write_log('Logging Picard iterations')
    open(picard_unit,name='picard_info.data',status='unknown')
    write(picard_unit,*) '#time    max_iter'
#endif

    ! allocate memory for ADI scheme
!lipscomb - added an analogous glissade option
!!!   if (model%options%whichevol.eq.1) then
  if (model%options%whichevol.eq.1 .or. model%options%whichevol.eq.11) then
!lipscomb - print*, 'allocate arrays'
       allocate(model%thckwk%alpha(max(model%general%ewn, model%general%nsn)))
       allocate(model%thckwk%beta (max(model%general%ewn, model%general%nsn)))
       allocate(model%thckwk%gamma(max(model%general%ewn, model%general%nsn)))
       allocate(model%thckwk%delta(max(model%general%ewn, model%general%nsn)))
    end if
  end subroutine glissade_init_thck

!****************************************************************************************

  subroutine glissade_thck_explicit_evolve(model,newtemps,logunit)

!lipscomb - This is a new explicit difference scheme.
! Diffusivities are computed in each of four quadrants and averaged to cell corners.
! Later, change the interface so that arguments are passed in explicitly.
! (Verifying, of course, that answers do not change.)

    use glissade_velo
    implicit none

    ! subroutine arguments

    type(glide_global_type) :: model
    logical, intent(in) :: newtemps             !*FD true when we should recalculate Glen's A
    integer,intent(in) :: logunit               !*FD unit for logging

!   Here is the basic sequence of calculations:
!
!   Part 1: Update the surface elevation
!   (1)  Compute vertically integrated flow factor in each grid cell.
!   (2)  Compute |del(s)^2| in each quadrant of each grid cell.
!   (3)  Given (1) and (2), compute the interior diffusivity for each quadrant
!        and average to cell corners.
!   (4)  Compute the effective diffusivity from basal sliding for each grid cell.
!   (5)  Combine (3) and (4) to get total diffusivity D in each cell corner.
!   (6)  Check CFL condition, dt <= dx^2 / (2*D_max)
!   (7)  Average D to cell edges and compute ice volume flux at each edge.
!   (8)  Advance the surface elevation.
!   (9)  Check for negative thickness.  (May need to limit the volume fluxes?)
! 
!   Part 2: Compute the strain heating term (might move to temperature module?)
!   (1)  Evaluate the interior velocity (at layer midpts) in each quadrant of each grid cell.
!   (2)  Evaluate the stress tau (at layer interfaces) in each quadrant of each grid cell.
!   (3)  Given (1) and (2), evaluate heating rate in each quadrant.
!   (4)  Sum over quadrants to get heating rate in each layer.
!   (5)  Compute basal heating rate.
!   (6)  Compare the total column heating in each grid cell to the change in potential energy.
!        Renormalize the heating as needed to conserv energy.
!
!   Part 3: Advect temperature and other tracers (switch with part 4?)
!   (1)  Given cell corner velocities in each layer, compute the area flux across each
!        cell edge in each layer.
!   (2)  Given the total volume flux in each column, compute the volume flux across each
!        cell edge in each layer.
!   (3)  Given (1) and (2), do a horizontal remapping of temperature and other tracers.
!   (4)  Verify that the new column thickness computed by the remapping is consistent
!        with the value computed in Part 1.
!   (5)  Do a vertical remapping of tracers back to sigma layers.
!
!   Part 4: Vertical heating
!   (1)  Solve a tridiagonal system of equations in each cell column for the new vertical
!        temperature profile, with heating rates appearing as source terms on the RHS.
!        This is done as in standard glimmer, but with advection turned off.
 
    if (model%geometry%empty) then

       model%geometry%thck = dmax1(0.0d0,model%geometry%thck + model%climate%acab * model%pcgdwk%fc2(2))
#ifdef DEBUG       
       print *, "* thck empty - net accumulation added", model%numerics%time
#endif
    else

       ! calculate basal velos

       if (newtemps) then
          call glissade_slipvelo(model,                &
                                 1,                             &
                                 model%velocity% btrc,          &
                                 model%velocity% ubas,          &
                                 model%velocity% vbas)

          ! calculate Glen's A if necessary
          !lipscomb - do not know why slipvelo is called before flwa is computed
          call glissade_velo_integrate_flwa(model%velowk,model%geomderv%stagthck,model%temper%flwa)

       end if

       call glissade_slipvelo(model,                         &
                              2,                             &
                              model%velocity% btrc,          &
                              model%velocity% ubas,          &
                              model%velocity% vbas)

       ! calculate diffusivity
       call glissade_velo_calc_diffu(model%velowk,             &
                                     model%geomderv%stagthck,  &
                                     model%geomderv%dusrfdew,  &
                                     model%geomderv%dusrfdns,  &
                                     model%velocity%diffu)

       ! get new thicknesses

  endif

  end subroutine glissade_thck_explicit_evolve

!****************************************************************************************

  subroutine glissade_thck_lin_evolve(model,newtemps,logunit)

    !*FD this subroutine solves the linearised ice thickness equation by computing the
    !*FD diffusivity from quantities of the previous time step

!lipscomb
!!!    use glide_velo
    use glissade_velo
    implicit none

    ! subroutine arguments

    type(glide_global_type) :: model
    logical, intent(in) :: newtemps                     !*FD true when we should recalculate Glen's A
    integer,intent(in) :: logunit                       !*FD unit for logging


!lipscomb - Here is a new flag that controls how we compute the diffusivity:
!           diffu_flag = 1 => Average A and H to cell corners, compute D at corners.
!           diffu_flag = 2 => Average del(usrf) to cell centers, compute D at centers,
!                             then average D to corners.
!           diffu_flag = 3 => Compute D in each of four cell quadrants, then average D
!                             to corners.
!           The default is diffu_flag = 1, the option used in standard glimmer.

    integer, parameter :: diffu_flag = 1
    integer :: i, j, ewn, nsn

    ewn = model%general%ewn
    nsn = model%general%nsn

    if (model%geometry%empty) then

       model%geometry%thck = dmax1(0.0d0,model%geometry%thck + model%climate%acab * model%pcgdwk%fc2(2))
#ifdef DEBUG       
       print *, "* thck empty - net accumulation added", model%numerics%time
#endif
    else

!lipscomb - reorganized the calls below.
!           There is just one call to glissade_basal_velo instead of three calls to slipvelo.
!           The call to glissade_shallow_velo replaces separate calls to 
!            velo_integrate_flwa, velo_calc_diffu, and velo_calc_velo.
!           Defined a new basal diffusivity variable, model%velocity%diffu_bas

       ! calculate basal velos
       if (newtemps) then

!lipscomb - This calculation moved to velocity calculations below.
!           Might want to modify so that this is done only if flwa has been updated.

          ! calculate Glen's A
!          call glissade_velo_integrate_flwa(model%velowk,             &
!                                            model%geomderv%stagthck,  &
!                                            model%temper%flwa)

       end if  ! newtemps

       call glissade_basal_velo(model,                         &
                                1,                             &
                                model%velocity% btrc,          &
                                model%velocity% ubas,          &
                                model%velocity% vbas)

       call glissade_shallow_velo(diffu_flag,                &
                                  model%numerics%dew,        &
                                  model%numerics%dns,        &
                                  model%velowk,              &
                                  model%geometry%usrf,       &
                                  model%geomderv%dusrfdew,   &
                                  model%geomderv%dusrfdns,   &
                                  model%geometry%thck,       &
                                  model%geomderv%stagthck,   &
                                  model%velocity%ubas,       &
                                  model%velocity%vbas,       &
                                  model%temper%flwa,         &
                                  model%velocity%diffu,      &
                                  model%velocity%uvel,       &
                                  model%velocity%vvel,       &
                                  model%velocity%uflx,       &
                                  model%velocity%vflx)

!lipscomb - debug

       if (debug) then
	print*, ''
	print*, 'itest, jtest =', itest, jtest

        print*, ''
        print*, 'stagthck:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%geomderv%stagthck(itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'dsdx:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%geomderv%dusrfdew(itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'dsdy:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%geomderv%dusrfdns(itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'diffu:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%velocity%diffu(itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'uvel(1):'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%velocity%uvel(1,itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'vvel(1):'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%velocity%vvel(1,itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'ubas:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%velocity%ubas(itest-1:itest+1,j)
        enddo

        print*, ''
        print*, 'vbas:'
        do j = jtest+1,jtest-1, -1
           write(6, '(3e14.6)') model%velocity%vbas(itest-1:itest+1,j)
        enddo

      endif

       ! get new thicknesses
       call glissade_thck_evolve(model, logunit, .true.,  &
                                 model%geometry%thck,     &
                                 model%geometry%thck)

    end if

  end subroutine glissade_thck_lin_evolve

!===========================================================================

  subroutine glissade_thck_nonlin_evolve(model,newtemps,logunit)

    !*FD this subroutine solves the ice thickness equation by doing an outer, 
    !*FD non-linear iteration to update the diffusivities and in inner, linear
    !*FD iteration to calculate the new ice thickness distrib

    use glimmer_global, only : dp
!lipscomb
!    use glide_velo
    use glissade_velo
    use glide_setup
    implicit none
    ! subroutine arguments
    type(glide_global_type) :: model
    logical, intent(in) :: newtemps                     !*FD true when we should recalculate Glen's A
    integer,intent(in) :: logunit                       !*FD unit for logging

    ! local variables
    integer, parameter :: pmax=50                       !*FD maximum Picard iterations
    real(kind=dp), parameter :: tol=1.0d-6
    real(kind=dp) :: residual
    integer p
    logical first_p


    if (model%geometry%empty) then

       model%geometry%thck = dmax1(0.0d0,model%geometry%thck + model%climate%acab * model%pcgdwk%fc2(2))
#ifdef DEBUG
       print *, "* thck empty - net accumulation added", model%numerics%time
#endif
    else

       ! calculate basal velos
       if (newtemps) then
          call glissade_slipvelo(model,                &
               1,                             &
               model%velocity% btrc,          &
               model%velocity% ubas,          &
               model%velocity% vbas)
          ! calculate Glen's A if necessary
          call glissade_velo_integrate_flwa(model%velowk,model%geomderv%stagthck,model%temper%flwa)
       end if

       first_p = .true.
       model%thckwk%oldthck = model%geometry%thck
       ! do Picard iteration
       do p=1,pmax
          model%thckwk%oldthck2 = model%geometry%thck

          call glissade_stagvarb(model%geometry% thck, &
               model%geomderv% stagthck,&
               model%general%  ewn, &
               model%general%  nsn)

          call glissade_geomders(model%numerics, &
               model%geometry% usrf, &
               model%geomderv% stagthck,&
               model%geomderv% dusrfdew, &
               model%geomderv% dusrfdns)

          call glissade_geomders(model%numerics, &
               model%geometry% thck, &
               model%geomderv% stagthck,&
               model%geomderv% dthckdew, &
               model%geomderv% dthckdns)

          call glissade_slipvelo(model,                &
               2,                             &
               model%velocity% btrc,          &
               model%velocity% ubas,          &
               model%velocity% vbas)

          ! calculate diffusivity
          call glissade_velo_calc_diffu(model%velowk,model%geomderv%stagthck,model%geomderv%dusrfdew, &
               model%geomderv%dusrfdns,model%velocity%diffu)

          ! get new thicknesses
          call glissade_thck_evolve(model,logunit,first_p,model%thckwk%oldthck,model%geometry%thck)

          first_p = .false.
          residual = maxval(abs(model%geometry%thck-model%thckwk%oldthck2))
          if (residual.le.tol) then
             exit
          end if
          
       end do
#ifdef DEBUG_PICARD
       picard_max=max(picard_max,p)
       if (model%numerics%tinc > mod(model%numerics%time,picard_interval)) then
          write(picard_unit,*) model%numerics%time,p
          picard_max = 0
       end if
#endif

       ! calculate horizontal velocity field
       call glissade_slipvelo(model,                &
                              3,                             &
                              model%velocity%btrc,           &
                              model%velocity%ubas,           &
                              model%velocity%vbas)
       call glissade_velo_calc_velo(model%velowk,    &
                                    model%geomderv%stagthck, &
                                    model%geomderv%dusrfdew, &
                                    model%geomderv%dusrfdns, &
                                    model%temper%flwa,       &
                                    model%velocity%diffu,    &
                                    model%velocity%ubas,     &
                                    model%velocity%vbas,     &
                                    model%velocity%uvel,     &
                                    model%velocity%vvel,     &
                                    model%velocity%uflx,     &
                                    model%velocity%vflx)
    end if
  end subroutine glissade_thck_nonlin_evolve

!==========================================================================

  subroutine glissade_thck_evolve(model,logunit,calc_rhs,old_thck,new_thck)

    !*FD set up sparse matrix and solve matrix equation to find new ice thickness distribution
    !*FD this routine does not override the old thickness distribution
    use glide_setup, only: glide_calclsrf
    use glimmer_global, only : dp
    implicit none
    ! subroutine arguments
    type(glide_global_type) :: model
    integer,intent(in) :: logunit                       !*FD unit for logging
    logical,intent(in) :: calc_rhs                      !*FD set to true when rhs should be calculated 
                                                        !*FD i.e. when doing lin solution or first picard iteration
    real(dp), intent(in), dimension(:,:) :: old_thck    !*FD contains ice thicknesses from previous time step
    real(dp), intent(inout), dimension(:,:) :: new_thck !*FD on entry contains first guess for new ice thicknesses
                                                        !*FD on exit contains ice thicknesses of new time step

    ! local variables
    real(dp), dimension(5) :: sumd 
    real(dp) :: err
    real(dp), parameter :: tolbnd = 1.0d-6

    integer :: linit
    integer, parameter :: mxtbnd = 10, ewbc = 1, nsbc = 1

    ! ewbc/nsbc set the type of boundary condition aplied at the end of
    ! the domain. a value of 0 implies zero gradient.

    integer :: ew,ns

       !* the number of grid points and the number of nonzero 
       !* matrix elements (including bounary points)
       model%pcgdwk%pcgsize = (/ model%geometry%totpts, model%geometry%totpts * 5 /)

       model%pcgdwk%ct = 1

       ! Boundary Conditions
       ! lower and upper BC
       do ew = 1,model%general%ewn
          ns=1
          if (model%geometry%mask(ew,ns) /= 0) then
             call glissade_putpcgc(model%pcgdwk,1.0d0,  &
                                   model%geometry%mask(ew,ns),  &
                                   model%geometry%mask(ew,ns))
             if (calc_rhs) then
                model%pcgdwk%rhsd(model%geometry%mask(ew,ns)) = old_thck(ew,ns) 
             end if
             model%pcgdwk%answ(model%geometry%mask(ew,ns)) = new_thck(ew,ns)
          end if
          ns=model%general%nsn
          if (model%geometry%mask(ew,ns) /= 0) then
             call glissade_putpcgc(model%pcgdwk,1.0d0,   &
                                   model%geometry%mask(ew,ns), &
                                   model%geometry%mask(ew,ns))
             if (calc_rhs) then
                model%pcgdwk%rhsd(model%geometry%mask(ew,ns)) = old_thck(ew,ns) 
             end if
             model%pcgdwk%answ(model%geometry%mask(ew,ns)) = new_thck(ew,ns)
          end if
       end do          

       !left and right BC
       if (model%options%periodic_ew.eq.1) then
          do ns=2,model%general%nsn-1
             ew = 1
             if (model%geometry%mask(ew,ns) /= 0) then
                call glissade_findsums(model%general%ewn-2,model%general%ewn-1,ns-1,ns)
                call glissade_generate_row(model%general%ewn-2,ew,ew+1,ns-1,ns,ns+1)
             end if
             ew=model%general%ewn
             if (model%geometry%mask(ew,ns) /= 0) then
                call glissade_findsums(1,2,ns-1,ns)
                call glissade_generate_row(ew-1,ew,3,ns-1,ns,ns+1)
             end if
          end do
       else
          do ns=2,model%general%nsn-1
             ew=1
             if (model%geometry%mask(ew,ns) /= 0) then
                call glissade_putpcgc(model%pcgdwk,1.0d0,   &
                                      model%geometry%mask(ew,ns),  &
                                      model%geometry%mask(ew,ns))
                if (calc_rhs) then
                   model%pcgdwk%rhsd(model%geometry%mask(ew,ns)) = old_thck(ew,ns) 
                end if
                model%pcgdwk%answ(model%geometry%mask(ew,ns)) = new_thck(ew,ns)
             end if
             ew=model%general%ewn
             if (model%geometry%mask(ew,ns) /= 0) then
                call glissade_putpcgc(model%pcgdwk,1.0d0,  &
                                      model%geometry%mask(ew,ns), &
                                      model%geometry%mask(ew,ns))
                if (calc_rhs) then
                   model%pcgdwk%rhsd(model%geometry%mask(ew,ns)) = old_thck(ew,ns) 
                end if
                model%pcgdwk%answ(model%geometry%mask(ew,ns)) = new_thck(ew,ns)
             end if
          end do
       end if

       ! ice body
       do ns = 2,model%general%nsn-1
          do ew = 2,model%general%ewn-1

             if (model%geometry%mask(ew,ns) /= 0) then
                
                call glissade_findsums(ew-1,ew,ns-1,ns)
                call glissade_generate_row(ew-1,ew,ew+1,ns-1,ns,ns+1)

             end if
          end do
       end do

       model%pcgdwk%pcgsize(2) = model%pcgdwk%ct - 1 

       call glissade_slapsolv(model,.true.,linit,err,logunit)   

       do ns = 1,model%general%nsn
          do ew = 1,model%general%ewn 

             if (model%geometry%mask(ew,ns) /= 0) then
                new_thck(ew,ns) = model%pcgdwk%answ(model%geometry%mask(ew,ns))
             end if

          end do
       end do

       ! *tp+* implement bcs

       model%pcgdwk%tlinit = model%pcgdwk%tlinit + linit
       model%pcgdwk%mlinit = max(linit,model%pcgdwk%mlinit)

       new_thck = max(0.0d0, new_thck)
#ifdef DEBUG
       print *, "* thck ", model%numerics%time, linit, model%pcgdwk%mlinit, model%pcgdwk%tlinit, model%geometry%totpts, &
            real(thk0*new_thck(model%general%ewn/2+1,model%general%nsn/2+1)), &
            real(vel0*maxval(abs(model%velocity%ubas))), real(vel0*maxval(abs(model%velocity%vbas))) 
#endif
       
       !------------------------------------------------------------
       ! calculate upper and lower surface
       !------------------------------------------------------------
       call glide_calclsrf(model%geometry%thck, model%geometry%topg, model%climate%eus, model%geometry%lsrf)
       model%geometry%usrf = max(0.d0,model%geometry%thck + model%geometry%lsrf)

  contains
    subroutine glissade_generate_row(ewm,ew,ewp,nsm,ns,nsp)
      ! calculate row of sparse matrix equation
      implicit none
      integer, intent(in) :: ewm,ew,ewp  ! ew index to left, central, right node
      integer, intent(in) :: nsm,ns,nsp  ! ns index to lower, central, upper node

      ! fill sparse matrix
      call glissade_putpcgc(model%pcgdwk,sumd(1), model%geometry%mask(ewm,ns), model%geometry%mask(ew,ns))       ! point (ew-1,ns)
      call glissade_putpcgc(model%pcgdwk,sumd(2), model%geometry%mask(ewp,ns), model%geometry%mask(ew,ns))       ! point (ew+1,ns)
      call glissade_putpcgc(model%pcgdwk,sumd(3), model%geometry%mask(ew,nsm), model%geometry%mask(ew,ns))       ! point (ew,ns-1)
      call glissade_putpcgc(model%pcgdwk,sumd(4), model%geometry%mask(ew,nsp), model%geometry%mask(ew,ns))       ! point (ew,ns+1)
      call glissade_putpcgc(model%pcgdwk,1.0d0 + sumd(5), model%geometry%mask(ew,ns), model%geometry%mask(ew,ns))! point (ew,ns)

      ! calculate RHS
      if (calc_rhs) then
         model%pcgdwk%rhsd(model%geometry%mask(ew,ns)) = old_thck(ew,ns) * &
              (1.0d0 - model%pcgdwk%fc2(3) * sumd(5)) - model%pcgdwk%fc2(3) * &
              (old_thck(ewm,ns) * sumd(1) + old_thck(ewp,ns) * sumd(2) + &
              old_thck(ew,nsm) * sumd(3) + old_thck(ew,nsp) * sumd(4)) - &
              model%pcgdwk%fc2(4) * (model%geometry%lsrf(ew,ns) * sumd(5) + &
              model%geometry%lsrf(ewm,ns) * sumd(1) + model%geometry%lsrf(ewp,ns) * sumd(2) + &
              model%geometry%lsrf(ew,nsm) * sumd(3) + model%geometry%lsrf(ew,nsp) * sumd(4)) +  &
              model%climate%acab(ew,ns) * model%pcgdwk%fc2(2)
      end if
      model%pcgdwk%answ(model%geometry%mask(ew,ns)) = new_thck(ew,ns)      
    end subroutine glissade_generate_row

    subroutine glissade_findsums(ewm,ew,nsm,ns)
      ! calculate diffusivities
      implicit none
      integer, intent(in) :: ewm,ew  ! ew index to left, right
      integer, intent(in) :: nsm,ns  ! ns index to lower, upper

!lipscomb - Below, ubas is not the basal velocity but the basal diffusivity.
!           Created a new variable corresponding to the basal diffusivity.
!           This makes it possible to compute ubas, the actual basal velocity,
!            before calling thck_evolve.

      ! calculate sparse matrix elements
      sumd(1) = model%pcgdwk%fc2(1) * (&
           (model%velocity%diffu(ewm,nsm) + model%velocity%diffu(ewm,ns)) + &
!!!           (model%velocity%ubas (ewm,nsm) + model%velocity%ubas (ewm,ns)))
           (model%velocity%diffu_bas (ewm,nsm) + model%velocity%diffu_bas (ewm,ns)))
      sumd(2) = model%pcgdwk%fc2(1) * (&
           (model%velocity%diffu(ew,nsm) + model%velocity%diffu(ew,ns)) + &
!!!           (model%velocity%ubas (ew,nsm) + model%velocity%ubas (ew,ns)))
           (model%velocity%diffu_bas (ew,nsm) + model%velocity%diffu_bas (ew,ns)))
      sumd(3) = model%pcgdwk%fc2(5) * (&
           (model%velocity%diffu(ewm,nsm) + model%velocity%diffu(ew,nsm)) + &
!!!           (model%velocity%ubas (ewm,nsm) + model%velocity%ubas (ew,nsm)))
           (model%velocity%diffu_bas (ewm,nsm) + model%velocity%diffu_bas (ew,nsm)))
      sumd(4) = model%pcgdwk%fc2(5) * (&
           (model%velocity%diffu(ewm,ns) + model%velocity%diffu(ew,ns)) + &
!!!           (model%velocity%ubas (ewm,ns) + model%velocity%ubas (ew,ns)))
           (model%velocity%diffu_bas (ewm,ns) + model%velocity%diffu_bas (ew,ns)))
      sumd(5) = - (sumd(1) + sumd(2) + sumd(3) + sumd(4))
    end subroutine glissade_findsums
  end subroutine glissade_thck_evolve

!---------------------------------------------------------------------------------

  subroutine glissade_slapsolv(model,first,iter,err,lunit)

    use glimmer_global, only : dp 
    use glide_stop
    use glimmer_log
  !use pcgdwk 
  !use funits, only : ulog

    implicit none

    type(glide_global_type) :: model
    logical, intent(in) :: first
    integer, intent(out) :: iter
    real(dp), intent(out) :: err
    integer,intent(in) :: lunit

    real(dp), dimension(:), allocatable :: rwork
    integer, dimension(:), allocatable :: iwork

    real(dp), parameter :: tol = 1.0d-12

    integer, parameter :: isym = 0, itol = 2, itmax = 101
    integer :: ierr, mxnelt

    if (first) then
      call ds2y(model%pcgdwk%pcgsize(1),  &
                model%pcgdwk%pcgsize(2),  &
                model%pcgdwk%pcgrow,      &
                model%pcgdwk%pcgcol,      &
                model%pcgdwk%pcgval,isym)
    end if

    mxnelt = 20 * model%pcgdwk%pcgsize(1)

    allocate(rwork(mxnelt),iwork(mxnelt))

!**     solve the problem using the SLAP package routines     
!**     -------------------------------------------------
!**     n ... order of matrix a (in)
!**     b ... right hand side vector (in)                        
!**     x ... initial quess/final solution vector (in/out)                        
!**     nelt ... number of non-zeroes in A (in)
!**     ia, ja ... sparse matrix format of A (in)
!**     a ... matrix helt in SLAT column format (in)
!**     isym ... storage method (0 is complete) (in)
!**     itol ... convergence criteria (2 recommended) (in)                     
!**     tol ... criteria for convergence (in)
!**     itmax ... maximum number of iterations (in)
!**     iter ... returned number of iterations (out)
!**     err ... error estimate of solution (out)
!**     ierr ... returned error message (0 is ok) (out)
!**     iunit ... unit for error writes during iteration (0 no write) (in)
!**     rwork ... workspace for SLAP routines (in)
!**     mxnelt ... maximum array and vector sizes (in)
!**     iwork ... workspace for SLAP routines (in)

    call dslucs(model%pcgdwk%pcgsize(1),   &
                model%pcgdwk%rhsd,         &
                model%pcgdwk%answ,         &
                model%pcgdwk%pcgsize(2),   &
                model%pcgdwk%pcgrow,       &
                model%pcgdwk%pcgcol,       &
                model%pcgdwk%pcgval, &
                isym,itol,tol,itmax,iter,err,ierr,0,rwork,mxnelt,iwork,mxnelt)

    if (ierr /= 0) then
      print *, 'pcg error ', ierr, itmax, iter
      call dcpplt(model%pcgdwk%pcgsize(1),  &
                  model%pcgdwk%pcgsize(2),  &
                  model%pcgdwk%pcgrow,      &
                  model%pcgdwk%pcgcol,      &
                  model%pcgdwk%pcgval,isym,lunit)
      print *, model%pcgdwk%pcgval
      write(*,*) model%numerics%time
      call glide_finalise(model,.true.)
      call close_log
      stop
    end if

    deallocate(rwork,iwork)

  end subroutine glissade_slapsolv 

!---------------------------------------------------------------------------------

  subroutine glissade_putpcgc(pcgdwk,value,col,row)

    use glimmer_global, only : dp
 ! use pcgdwk, only : pcgval, pcgcol, pcgrow, ct

    implicit none

    type(glide_pcgdwk) :: pcgdwk
    integer, intent(in) :: row, col
    real(dp), intent(in) :: value

    if (value /= 0.0d0) then
      pcgdwk%pcgval(pcgdwk%ct) = value
      pcgdwk%pcgcol(pcgdwk%ct) = col
      pcgdwk%pcgrow(pcgdwk%ct) = row
      pcgdwk%ct = pcgdwk%ct + 1
    end if

  end subroutine glissade_putpcgc
  
!---------------------------------------------------------------------------------

  subroutine glissade_geomders(numerics,ipvr,stagthck,opvrew,opvrns)

    use glimmer_global, only : dp

    implicit none 

    type(glide_numerics) :: numerics
    real(dp), intent(out), dimension(:,:) :: opvrew, opvrns
    real(dp), intent(in), dimension(:,:) :: ipvr, stagthck

    real(dp) :: dew2, dns2 
    integer :: ew,ns,ewn,nsn

    ! Obviously we don't need to do this every time,
    ! but will do so for the moment.
    dew2 = 1.d0/(2.0d0 * numerics%dew)
    dns2 = 1.d0/(2.0d0 * numerics%dns)
    ewn=size(ipvr,1)
    nsn=size(ipvr,2)

    do ns=1,nsn-1
       do ew = 1,ewn-1
          if (stagthck(ew,ns) /= 0.0d0) then
             opvrew(ew,ns) = (ipvr(ew+1,ns+1)+ipvr(ew+1,ns)-ipvr(ew,ns)-ipvr(ew,ns+1)) * dew2
             opvrns(ew,ns) = (ipvr(ew+1,ns+1)+ipvr(ew,ns+1)-ipvr(ew,ns)-ipvr(ew+1,ns)) * dns2
          else
             opvrew(ew,ns) = 0.
             opvrns(ew,ns) = 0.
          end if
       end do
    end do

    !opvrew = (cshift(cshift(ipvr,1,2),1,1) + cshift(ipvr,1,1) - ipvr - cshift(ipvr,1,2)) * dew2
    !opvrns = (cshift(cshift(ipvr,1,2),1,1) + cshift(ipvr,1,2) - ipvr - cshift(ipvr,1,1)) * dns2
    
  end subroutine glissade_geomders

!---------------------------------------------------------------------------------
!lipscomb - later, move this to glissade

  subroutine glissade_geomders_cgrid(numerics,ipvr,stagthcku,stagthckv,opvrew,opvrns)

    use glimmer_global, only : dp

    implicit none 

    type(glide_numerics) :: numerics
    real(dp), intent(out), dimension(:,:) :: opvrew, opvrns
    real(dp), intent(in), dimension(:,:) :: ipvr, stagthcku, stagthckv

    real(dp) :: dew, dns 
    integer :: ew,ns,ewn,nsn

    ! Obviously we don't need to do this every time,
    ! but will do so for the moment.
!!    dew2 = 1.d0/(2.0d0 * numerics%dew)
!!    dns2 = 1.d0/(2.0d0 * numerics%dns)
    dew = 1.d0 / numerics%dew
    dns = 1.d0 / numerics%dns
    ewn=size(ipvr,1)
    nsn=size(ipvr,2)

    do ns=1,nsn-1
       do ew = 1,ewn-1
          if (stagthcku(ew,ns) /= 0.0d0) then
!!             opvrew(ew,ns) = (ipvr(ew+1,ns+1)+ipvr(ew+1,ns)-ipvr(ew,ns)-ipvr(ew,ns+1)) * dew2
             opvrew(ew,ns) = (ipvr(ew+1,ns)-ipvr(ew,ns)) * dew
          else
             opvrew(ew,ns) = 0.d0
          endif

          if (stagthckv(ew,ns) /= 0.0d0) then
!!             opvrns(ew,ns) = (ipvr(ew+1,ns+1)+ipvr(ew,ns+1)-ipvr(ew,ns)-ipvr(ew+1,ns)) * dns2
             opvrns(ew,ns) = (ipvr(ew,ns+1)-ipvr(ew,ns)) * dns
          else
             opvrns(ew,ns) = 0.d0
          end if
       end do
    end do

    !opvrew = (cshift(cshift(ipvr,1,2),1,1) + cshift(ipvr,1,1) - ipvr - cshift(ipvr,1,2)) * dew2
    !opvrns = (cshift(cshift(ipvr,1,2),1,1) + cshift(ipvr,1,2) - ipvr - cshift(ipvr,1,1)) * dns2
    
  end subroutine glissade_geomders_cgrid

!---------------------------------------------------------------------------------

  subroutine glissade_geom2ders(model,ipvr,stagthck,opvrew,opvrns)

    use glimmer_global, only : dp ! ew, ewn, ns, nsn
!    use numerics, only : dew, dns

    implicit none 

    type(glide_global_type) :: model
    real(dp), intent(out), dimension(:,:) :: opvrew, opvrns
    real(dp), intent(in), dimension(:,:) :: ipvr, stagthck

    real(dp) :: dewsq4, dnssq4
    integer :: ew,ns

    integer :: pt(2)

    dewsq4 = 4.0d0 * model%numerics%dew * model%numerics%dew
    dnssq4 = 4.0d0 * model%numerics%dns * model%numerics%dns
 
    do ns = 2, model%general%nsn-2
      do ew = 2, model%general%ewn-2
        if (stagthck(ew,ns) .gt. 0.0d0) then
          opvrew(ew,ns) = centerew(ew,ns)
          opvrns(ew,ns) = centerns(ew,ns)
        else
          opvrew(ew,ns) = 0.0d0
          opvrns(ew,ns) = 0.0d0
        end if
      end do
    end do

! *** 2nd order boundaries using upwinding

    do ew = 1, model%general%ewn-1, model%general%ewn-2

      pt = whichway(ew)

      do ns = 2, model%general%nsn-2 
        if (stagthck(ew,ns) .gt. 0.0d0) then
          opvrew(ew,ns) = boundyew(pt,ns)
          opvrns(ew,ns) = centerns(ew,ns)
        else
          opvrew(ew,ns) = 0.0d0
          opvrns(ew,ns) = 0.0d0
        end if
      end do

    end do

    do ns = 1, model%general%nsn-1, model%general%nsn-2

      pt = whichway(ns)

      do ew = 2, model%general%ewn-2  
        if (stagthck(ew,ns) .gt. 0.0d0) then
          opvrew(ew,ns) = centerew(ew,ns)
          opvrns(ew,ns) = boundyns(pt,ew)
        else
          opvrew(ew,ns) = 0.0d0
          opvrns(ew,ns) = 0.0d0
        end if
      end do

    end do

    do ns = 1, model%general%nsn-1, model%general%nsn-2
      do ew = 1, model%general%ewn-1, model%general%ewn-2
        if (stagthck(ew,ns) .gt. 0.0d0) then
          pt = whichway(ew)
          opvrew(ew,ns) = boundyew(pt,ns)
          pt = whichway(ns)
          opvrns(ew,ns) = boundyns(pt,ew)
        else
          opvrew(ew,ns) = 0.0d0
          opvrns(ew,ns) = 0.0d0
        end if
      end do
    end do

  contains

    function centerew(ew,ns)

      implicit none

      real(dp) :: centerew
      integer ns,ew

      centerew = (sum(ipvr(ew+2,ns:ns+1)) + sum(ipvr(ew-1,ns:ns+1)) - &
                  sum(ipvr(ew+1,ns:ns+1)) - sum(ipvr(ew,ns:ns+1))) / dewsq4
    
    end function centerew 

    function centerns(ew,ns)

      implicit none

      real(dp) :: centerns
      integer ns,ew

      centerns = (sum(ipvr(ew:ew+1,ns+2)) + sum(ipvr(ew:ew+1,ns-1)) - &
                  sum(ipvr(ew:ew+1,ns+1)) - sum(ipvr(ew:ew+1,ns))) / dnssq4
  
    end function centerns 

    function boundyew(pt,ns)

      implicit none

      integer, intent(in) :: pt(2)
      real(dp) :: boundyew
      integer ns

      boundyew = pt(1) * (3.0d0 * sum(ipvr(pt(2),ns:ns+1)) - 7.0d0 * sum(ipvr(pt(2)+pt(1),ns:ns+1)) + &
                 5.0d0 * sum(ipvr(pt(2)+2*pt(1),ns:ns+1)) - sum(ipvr(pt(2)+3*pt(1),ns:ns+1))) / dewsq4

    end function boundyew

    function boundyns(pt,ew)

      implicit none

      integer, intent(in) :: pt(2)
      real(dp) :: boundyns
      integer ew

      boundyns = pt(1) * (3.0d0 * sum(ipvr(ew:ew+1,pt(2))) - 7.0d0 * sum(ipvr(ew:ew+1,pt(2)+pt(1))) + &
                 5.0d0 * sum(ipvr(ew:ew+1,pt(2)+2*pt(1))) - sum(ipvr(ew:ew+1,pt(2)+3*pt(1)))) / dnssq4

    end function boundyns

    function whichway(i)

      implicit none

      integer, intent(in) :: i
      integer :: whichway(2) 

      if (i == 1) then 
        whichway = (/1,1/)
      else
        whichway = (/-1,i+1/)
      end if

    end function whichway

  end subroutine glissade_geom2ders

!---------------------------------------------------------------------------------

  subroutine glissade_stagvarb(ipvr,opvr,ewn,nsn)

    use glimmer_global, only : dp ! ewn, nsn
 
    implicit none 

    real(dp), intent(out), dimension(:,:) :: opvr 
    real(dp), intent(in), dimension(:,:) :: ipvr
    integer :: ewn,nsn

    opvr(1:ewn-1,1:nsn-1) = (ipvr(2:ewn,1:nsn-1) + ipvr(1:ewn-1,2:nsn) + &
                             ipvr(2:ewn,2:nsn)   + ipvr(1:ewn-1,1:nsn-1)) / 4.0d0

  end subroutine glissade_stagvarb

!---------------------------------------------------------------------------------
!lipscomb - later, move this to glissade

  subroutine glissade_stagvar_cgrid(ipvr,opvru,opvrv,ewn,nsn)

    use glimmer_global, only : dp ! ewn, nsn
 
    implicit none 

    real(dp), intent(in), dimension(:,:) :: ipvr   ! input field, T pts (cell centers)
    real(dp), intent(out), dimension(:,:) :: opvru ! output field, U pts (east edges)
    real(dp), intent(out), dimension(:,:) :: opvrv ! output field, V pts (north edges)
    integer :: ewn,nsn

!lipscomb - B-grid stuff commented out
!!    opvr(1:ewn-1,1:nsn-1) = (ipvr(2:ewn,1:nsn-1) + ipvr(1:ewn-1,2:nsn) + &
!!                             ipvr(2:ewn,2:nsn)   + ipvr(1:ewn-1,1:nsn-1)) / 4.0d0

    opvru(1:ewn-1,1:nsn-1) = (ipvr(2:ewn,1:nsn-1) + ipvr(1:ewn-1,1:nsn-1)) / 2.0d0
    opvrv(1:ewn-1,1:nsn-1) = (ipvr(1:ewn-1,2:nsn) + ipvr(1:ewn-1,1:nsn-1)) / 2.0d0

  end subroutine glissade_stagvar_cgrid

!---------------------------------------------------------------------------------

  subroutine glissade_timeders(thckwk,ipvr,opvr,mask,time,which)

    use glimmer_global, only : dp, sp
    use glimmer_paramets, only : conv

    implicit none 

    type(glide_thckwk) :: thckwk
    real(dp), intent(out), dimension(:,:) :: opvr 
    real(dp), intent(in), dimension(:,:) :: ipvr
    real(sp), intent(in) :: time 
    integer, intent(in), dimension(:,:) :: mask
    integer, intent(in) :: which

    real(sp) :: factor

    factor = (time - thckwk%oldtime)
    if (factor .eq.0) then
       opvr = 0.0d0
    else
       factor = 1./factor
       where (mask /= 0)
          opvr = conv * (ipvr - thckwk%olds(:,:,which)) * factor
       elsewhere
          opvr = 0.0d0
       end where
    end if

    thckwk%olds(:,:,which) = ipvr

    if (which == thckwk%nwhich) then
      thckwk%oldtime = time
    end if

  end subroutine glissade_timeders

  subroutine glissade_filterthck(thck,ewn,nsn)

    use glimmer_global, only : dp ! ew, ewn, ns, nsn

    implicit none

    real(dp), dimension(:,:), intent(inout) :: thck
    real(dp), dimension(:,:), allocatable :: smth
    integer :: ewn,nsn

    real(dp), parameter :: f = 0.1d0 / 16.0d0
    integer :: count
    integer :: ns,ew

    allocate(smth(ewn,nsn))
    count = 1

    do ns = 3,nsn-2
      do ew = 3,ewn-2

        if (all((thck(ew-2:ew+2,ns) > 0.0d0)) .and. all((thck(ew,ns-2:ns+2) > 0.0d0))) then
          smth(ew,ns) =  thck(ew,ns) + f * &
                        (thck(ew-2,ns) - 4.0d0 * thck(ew-1,ns) + 12.0d0 * thck(ew,ns) - &
                         4.0d0 * thck(ew+1,ns) + thck(ew+2,ns) + &
                         thck(ew,ns-2) - 4.0d0 * thck(ew,ns-1) - &
                         4.0d0 * thck(ew,ns+1) + thck(ew,ns+2))
          count = count + 1
        else
          smth(ew,ns) = thck(ew,ns)
        end if

      end do
    end do

    thck(3:ewn-2,3:nsn-2) = smth(3:ewn-2,3:nsn-2)
    print *, count

    deallocate(smth)            

  end subroutine glissade_filterthck

!----------------------------------------------------------------------

  subroutine glissade_swapbndh(bc,a,b,c,d)

    use glimmer_global, only : dp

    implicit none

    real(dp), intent(out), dimension(:) :: a, c
    real(dp), intent(in), dimension(:) :: b, d
    integer, intent(in) :: bc

    if (bc == 0) then
      a = b
      c = d
    end if

  end subroutine glissade_swapbndh

  !-----------------------------------------------------------------------------
  ! ADI routines
  !-----------------------------------------------------------------------------

  subroutine glissade_stagleapthck(model,newtemps,logunit)
    
    !*FD this subroutine solves the ice sheet thickness equation using the ADI scheme
    !*FD diffusivities are updated for each half time step

    use glide_setup, only: glide_calclsrf
!lipscomb
!!    use glide_velo
    use glissade_velo
    use glimmer_utils
    implicit none
    ! subroutine arguments
    type(glide_global_type) :: model
    logical, intent(in) :: newtemps                     !*FD true when we should recalculate Glen's A
    integer,intent(in) :: logunit                       !*FD unit for logging

    ! local variables
    integer ew,ns, n

    if (model%geometry%empty) then

       model%geometry%thck = dmax1(0.0d0,model%geometry%thck + model%climate%acab * model%pcgdwk%fc2(2))
#ifdef DEBUG       
       print *, "* thck empty - net accumulation added", model%numerics%time
#endif
    else

       ! calculate basal velos
       if (newtemps) then
          call glissade_slipvelo(model,                &
               1,                             &
               model%velocity% btrc,          &
               model%velocity% ubas,          &
               model%velocity% vbas)
          ! calculate Glen's A if necessary
          call glissade_velo_integrate_flwa(model%velowk,model%geomderv%stagthck,model%temper%flwa)
       end if
       call glissade_slipvelo(model,                &
            2,                             &
            model%velocity% btrc,          &
            model%velocity% ubas,          &
            model%velocity% vbas)

       ! calculate diffusivity
       call glissade_velo_calc_diffu(model%velowk,model%geomderv%stagthck,model%geomderv%dusrfdew, &
            model%geomderv%dusrfdns,model%velocity%diffu)

       model%velocity%total_diffu(:,:) = model%velocity%diffu(:,:) + model%velocity%ubas(:,:)

       ! first ADI step, solve thickness equation along rows j
       n = model%general%ewn
       do ns=2,model%general%nsn-1
          call glissade_adi_tri ( model%thckwk%alpha, model%thckwk%beta, model%thckwk%gamma, model%thckwk%delta, &
               model%geometry%thck(:,ns), model%geometry%lsrf(:,ns), model%climate%acab(:,ns), &
               model%velocity%vflx(:,ns), model%velocity%vflx(:,ns-1), &
               model%velocity%total_diffu(:,ns),  model%velocity%total_diffu(:,ns-1), &
               model%numerics%dt, model%numerics%dew, model%numerics%dns )

          call tridag( model%thckwk%alpha(2:n), model%thckwk%beta(1:n), model%thckwk%gamma(1:n-1), model%thckwk%delta(1:n), &
               model%thckwk%oldthck(:,ns), n )
       end do

       model%thckwk%oldthck(:,:) = max(model%thckwk%oldthck(:,:), 0.d0)

       ! second ADI step, solve thickness equation along columns i
       n = model%general%nsn
       do ew=2,model%general%ewn-1
          call glissade_adi_tri ( model%thckwk%alpha, model%thckwk%beta, model%thckwk%gamma, model%thckwk%delta, &
               model%thckwk%oldthck(ew,:), model%geometry%lsrf(ew, :), model%climate%acab(ew, :), &
               model%velocity%uflx(ew,:), model%velocity%uflx(ew-1,:), &
               model%velocity%total_diffu(ew,:), model%velocity%total_diffu(ew-1,:), &
               model%numerics%dt, model%numerics%dns, model%numerics%dew )

          call tridag( model%thckwk%alpha(2:n), model%thckwk%beta(1:n), model%thckwk%gamma(1:n-1), model%thckwk%delta(1:n), &
               model%geometry%thck(ew, :), n)
       end do

       model%geometry%thck(:,:) = max(model%geometry%thck(:,:), 0.d0)

       ! calculate horizontal velocity field
       call glissade_slipvelo(model,                &
            3,                             &
            model%velocity%btrc,           &
            model%velocity%ubas,           &
            model%velocity%vbas)
       call glissade_velo_calc_velo(model%velowk,model%geomderv%stagthck,model%geomderv%dusrfdew, &
            model%geomderv%dusrfdns,model%temper%flwa,model%velocity%diffu,model%velocity%ubas, &
            model%velocity%vbas,model%velocity%uvel,model%velocity%vvel,model%velocity%uflx,model%velocity%vflx)
    end if

    !------------------------------------------------------------
    ! calculate upper and lower surface
    !------------------------------------------------------------
    call glide_calclsrf(model%geometry%thck, model%geometry%topg, model%climate%eus, model%geometry%lsrf)
    model%geometry%usrf = max(0.d0,model%geometry%thck + model%geometry%lsrf)

  end subroutine glissade_stagleapthck


  subroutine glissade_adi_tri(a,b,c,d,thk,tpg,mb,flx_p,flx_m,dif_p,dif_m,dt,ds1, ds2)
    !*FD construct tri-diagonal matrix system for a column/row
    use glimmer_global, only : dp, sp
    implicit none
    
    real(dp), dimension(:), intent(out) :: a !*FD alpha (subdiagonal)
    real(dp), dimension(:), intent(out) :: b !*FD alpha (diagonal)
    real(dp), dimension(:), intent(out) :: c !*FD alpha (superdiagonal)
    real(dp), dimension(:), intent(out) :: d !*FD right-hand side
    
    real(dp), dimension(:), intent(in) :: thk   !*FD ice thickness
    real(dp), dimension(:), intent(in) :: tpg   !*FD lower surface of ice
    real(sp), dimension(:), intent(in) :: mb    !*FD mass balance
    real(dp), dimension(:), intent(in) :: flx_p !*FD flux +1/2
    real(dp), dimension(:), intent(in) :: flx_m !*FD flux -1/2
    real(dp), dimension(:), intent(in) :: dif_p !*FD diffusivity +1/2
    real(dp), dimension(:), intent(in) :: dif_m !*FD diffusivity -1/2
    
    real(dp), intent(in) :: dt !*FD time step
    real(dp), intent(in) :: ds1, ds2 !*FD spatial steps inline and transversal

    ! local variables
    real(dp) :: f1, f2, f3
    integer :: i,n
    
    n = size(thk)

    f1 = dt/(4*ds1*ds1)
    f2 = dt/(4*ds2)
    f3 = dt/2.

    a(:) = 0.
    b(:) = 0.
    c(:) = 0.
    d(:) = 0.

    a(1) = 0.
    do i=2,n
       a(i) = f1*(dif_m(i-1)+dif_p(i-1))
    end do
    do i=1,n-1
       c(i) = f1*(dif_m(i)+dif_p(i))
    end do
    c(n) = 0.
    b(:) = -(a(:)+c(:))

    ! calculate RHS
    do i=2,n-1
       d(i) = thk(i) - &
            f2 * (flx_p(i-1) + flx_p(i) - flx_m(i-1) - flx_m(i)) + &
            f3 * mb(i) - &
            a(i)*tpg(i-1) - b(i)*tpg(i) - c(i)*tpg(i+1)
    end do

    b(:) = 1.+b(:)

  end subroutine glissade_adi_tri

end module glissade_thck

