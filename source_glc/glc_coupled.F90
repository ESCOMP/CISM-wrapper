!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_coupled

!BOP
!MODULE: glc_coupled

! !DESCRIPTION:
!  This module contains the routines necessary for coupling glc to
!  other model components using the CCSM flux coupler.  To
!  enable the routines in this module, the coupled ifdef option must
!  be specified during the make process.
!
!  In the POP version, fields are packed and unpacked locally for each block.
!  Here, fields are packed and unpacked on a global 2d grid which is
!   identical to the clm grid.  Upscaling and downscaling between the local
!   ice sheet grid and global clm/glc grid is done by GLINT.
!
! !REVISION HISTORY:
!  SVN:$Id: forcing_coupled.F90 901 2006-05-08 20:47:20Z njn01 $
!  WHL, Aug 2007: Adapted from forcing_coupled.F90 in POP 2.0

! !USES:
 
   use glc_kinds_mod
   use glc_communicate
   use glc_constants
   use glc_time_management
   use glc_exit_mod
   use glc_timers
   use glc_global_grid, only: glc_grid, region_mask
   use glc_global_fields, only: nec                          ! no. elev classes
   use glc_global_fields, only: tsfc, topo, qice                    ! from coupler
   use glc_global_fields, only: gfrac, gthck, gtopo, ghflx, groff   ! to coupler

   !*** ccsm
   use cpl_contract_mod
   use cpl_interface_mod
   use cpl_fields_mod
   use glc_registry
   use shr_sys_mod
      
   implicit none
   save

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  module variables
!
!-----------------------------------------------------------------------

!lipscomb - just for now
   logical (log_kind), parameter ::   &
      l_coupglc = .true.    ! true for coupling to CCSM

   logical (log_kind) ::   &
      lcoupled,            &! flag for coupled forcing
      ldiag_cpl = .false.

   integer (int_kind) ::   &
      coupled_freq_iopt,   &! coupler frequency option
      coupled_freq          ! frequency of coupling

!lipscomb - comment out the ifdef for now
!!!#if COUPGLC

   integer (int_kind) ::   &
      timer_send_to_cpl,   &
      timer_recv_from_cpl, &
      timer_recv_to_send,  &
      timer_send_to_recv 
 
!lipscomb - are all these diagnostics needed?
   integer (int_kind), private ::   &
      cpl_stop_now,        &! flag id for stop_now flag
      cpl_ts,              &! flag id for coupled_ts flag
      cpl_write_restart,   &! flag id for write restart
      cpl_write_history,   &! flag id for write history
      cpl_write_tavg,      &! flag id for write tavg      
      cpl_diag_global,     &! flag id for computing diagnostics
      cpl_diag_transp       ! flag id for computing diagnostics

   integer (int_kind), dimension(cpl_fields_ibuf_total) ::  &
      isbuf,               &! integer control buffer for sends
      irbuf                 ! integer control buffer for receives
 
   type(cpl_contract) ::  &
      contractS,           &! contract for sends to coupler
      contractR             ! contract for receives from coupler
 
   real (r8), dimension(:,:), allocatable ::  &
      sbuf                 ! temporary send/recv buffer

!-----------------------------------------------------------------------
!  The following variables are used in the exchange of 
!  information between cpl6 and the glc code.
!
!  glc --> cpl6
!  ============
!    cpl_fields_ibuf_total -- length of integer ocean "send buffer" vector (isbuf)
!    nsend --  total number of 2D fields sent to cpl6 from glc
!
!    integer send buffer indices (isbuf in subroutine init_coupled):  
!
!     o  cpl_fields_ibuf_cdate   -- glc's character date string (yyyymmdd)
!     o  cpl_fields_ibuf_sec     -- glc's character time string (seconds)
!     o  cpl_fields_ibuf_gsize   -- nx_global*ny_global
!     o  cpl_fields_ibuf_gisize  -- nx_global
!     o  cpl_fields_ibuf_gjsize  -- ny_global
!lipscomb - Should we assume lsize = gsize?  Do we pass this info?
!!!!     o  cpl_fields_ibuf_lsize   -- (iphys_e-iphys_b+1)*(jphys_e-jphys_b+1)
!!!!     o  cpl_fields_ibuf_lisize  -- (iphys_e-iphys_b+1)
!!!!     o  cpl_fields_ibuf_ljsize  -- (jphys_e-jphys_b+1)
!     o  cpl_fields_ibuf_lsize   -- nx_global*ny_global
!     o  cpl_fields_ibuf_lisize  -- nx_global
!     o  cpl_fields_ibuf_ljsize  -- ny_global
!     o  cpl_fields_ibuf_ncpl    -- ncouple_per_day
!     o  cpl_fields_ibuf_nfields -- cpl_fields_grid_total
!     o  cpl_fields_ibuf_dead    --  0 ==>  not a "dead" model
!
!    real send buffer indices (sbuf in subroutine init_coupled):
!
!lipscomb - modify if TLON and TLAT are in radians
!     o  cpl_fields_grid_lon   -- radian*TLON(i,j)
!     o  cpl_fields_grid_lat   -- radian*TLAT(i,j)
!     o  cpl_fields_grid_area  -- TAREA(i,j)/(radius*radius)
!     o  cpl_fields_grid_mask  -- float(REGION_MASK(i,j))
!     o  cpl_fields_grid_index -- (j_global(j)-1)*(nx_global)+i_global(i)
!
!    real send buffer indices (sbuf in subroutine send_to_coupler):
!
!      o  index_gg2c_Sg_gfrac     -- glacier ice fraction [0,1]
!      o  index_gg2c_Sg_gthck     -- ice thickness (m)
!      o  index_gg2c_Sg_gtopo     -- surface elevation (m)
!      o  index_gg2c_Flgg_ghflx   -- heat flux at surface, positive down (W/m^2)
!      o  index_gg2c_Flgg_groff   -- glacier runoff (kg/m^2/s = mm H2O/s)
!         if CLM compute sfc mass balance, runoff includes calving and basal melt 
!
!    cpl6 --> glc  
!    ============
!
!    cpl_fields_ibuf_total -- length of integer glc "receive buffer" vector (irbuf)
!
!    integer receive buffer indices (irbuf in subroutine recv_from_coupler):
!
!     o  cpl_fields_ibuf_stopnow  -- stop glc integration now
!     o  cpl_fields_ibuf_infobug  -- write glc/coupler diagnostics now  
!     o  cpl_fields_ibuf_resteod  -- write glc restart files at end of day
!     o  cpl_fields_ibuf_histeod  -- write glc history files at end of day
!     o  cpl_fields_ibuf_histtavg -- write glc "tavg"  files at end of day
!     o  cpl_fields_ibuf_diageod  -- write glc diagnostics   at end of day
!
!    real receive buffer indices (sbuf in subroutine recv_from_coupler):
!
!     o  index_c2gg_Sl_tsfc       -- surface temperature (Kelvin) 
!                                 -- (2m ref temp for PDD scheme)
!     o  index_c2gg_Sl_topo       -- surface elevation (m) 
!     o  index_c2gg_Flgl_qice     -- flux of new glacier ice (kg/m2/s)
!                                 -- (precip for PDD scheme)
!
!-----------------------------------------------------------------------

   ! g2c states and fluxes
   integer(kind=int_kind) :: index_gg2c_Sg_gfrac    ! glacier ice fraction [0,1]
   integer(kind=int_kind) :: index_gg2c_Sg_gthck    ! ice thickness (m)
   integer(kind=int_kind) :: index_gg2c_Sg_gtopo    ! surface elevation (m)
   integer(kind=int_kind) :: index_gg2c_Flgg_ghflx  ! heat flux (W/m^2, positive down)
   integer(kind=int_kind) :: index_gg2c_Flgg_groff  ! runoff (kg/m^2/s = mm H2O/s)
 
   ! c2g states and fluxes
   integer(kind=int_kind) :: index_c2gg_Sl_tsfc     ! surface temperature (Kelvin)
   integer(kind=int_kind) :: index_c2gg_Sl_topo     ! surface elevation (m)
   integer(kind=int_kind) :: index_c2gg_Flgl_qice   ! flux of new glacier ice (kg/m^2/s)

!lipscomb - commented out the ifdef for now
!!!#endif

!EOC
!***********************************************************************

 contains

!***********************************************************************

!BOP
! !IROUTINE: init_coupled
! !INTERFACE:

 subroutine init_coupled

! !DESCRIPTION:
!  This routine sets up everything necessary for coupling with
!  the CCSM coupler, cpl6
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   character (char_len) ::  &
      coupled_freq_opt

!lipscomb - Do we keep all these options?
   namelist /coupled_nml/ coupled_freq_opt, coupled_freq

   integer (i4) ::   &
      nxg, nyg,            &! global grid dimensions
      nsend,               &! num of fields to send
      numg,                &! number of global grid cells
      ncouple_per_day,     &! num of coupler comms per day
      nml_error             ! namelist i/o error flag

   real(r8), dimension(:), pointer ::   &
      lat, lon

   real(r8), dimension(:,:), pointer ::   &
      area                  ! grid cell area (m^2)

   integer (i4) ::  &
      i,j,g                 ! indices


!lipscomb - debug
      write(6,*) 'Beginning init_coupled'
      call shr_sys_flush(6)

!-----------------------------------------------------------------------
!
!  read coupled_nml namelist to start coupling and determine
!  coupling frequency
!
!-----------------------------------------------------------------------
      
      lcoupled          = .false.
      coupled_freq_opt  = 'never'
      coupled_freq_iopt = freq_opt_never
      coupled_freq      = 100000
      
      if (my_task == master_task) then
         open (nml_in, file=nml_filename, status='old',iostat=nml_error)
         if (nml_error /= 0) then
            nml_error = -1
         else
            nml_error =  1
         endif
         do while (nml_error > 0)
            read(nml_in, nml=coupled_nml,iostat=nml_error)
         end do
         if (nml_error == 0) close(nml_in)
      endif

      call broadcast_scalar(nml_error, master_task)
      if (nml_error /= 0) then
         call exit_glc(sigAbort,'ERROR reading coupled_nml')
      endif

      if (my_task == master_task) then
          write(stdout,blank_fmt)
          write(stdout,ndelim_fmt)
          write(stdout,blank_fmt)
          write(stdout,*) ' Coupling:'
          write(stdout,blank_fmt)
          write(stdout,*) ' coupled_nml namelist settings:'
          write(stdout,blank_fmt)
          write(stdout, coupled_nml)
          write(stdout,blank_fmt)
      endif

!lipscomb - Coupling should normally be as infrequent as possible, i.e. once per day.

      if (my_task == master_task) then
        select case (coupled_freq_opt)

        case ('nyear')
          coupled_freq_iopt = -1000

        case ('nmonth')
          coupled_freq_iopt = -1000

        case ('nday')
          if (coupled_freq == 1) then
            lcoupled = .true.
            coupled_freq_iopt = freq_opt_nday
            ncouple_per_day = 1
          else
            coupled_freq_iopt = -1000
          endif

        case ('nhour')
          if (coupled_freq <= 24) then
            lcoupled = .true.
            coupled_freq_iopt = freq_opt_nhour
            ncouple_per_day = 24/coupled_freq
          else
            coupled_freq_iopt = -1000
          endif

        case ('nsecond')
          if (coupled_freq <= seconds_in_day) then
            lcoupled = .true.
            coupled_freq_iopt = freq_opt_nsecond
            ncouple_per_day = seconds_in_day/coupled_freq
          else
            coupled_freq_iopt = -1000
          endif

        case ('nstep')
          if (coupled_freq <= nsteps_per_day) then
            lcoupled = .true.
            coupled_freq_iopt = freq_opt_nstep
            ncouple_per_day = nsteps_per_day/coupled_freq
          else
            coupled_freq_iopt = -1000
          endif

        case ('never')
          lcoupled = .false.

        case default
          coupled_freq_iopt = -2000
        end select

      endif
            
      call broadcast_scalar(lcoupled,          master_task)
      call broadcast_scalar(coupled_freq_iopt, master_task)
      call broadcast_scalar(coupled_freq     , master_task)

      if (coupled_freq_iopt == -1000) then
        call exit_glc(sigAbort,  &
                 'ERROR: Coupling frequency must be at least once per day')
      else if (coupled_freq_iopt == -2000) then
        call exit_glc(sigAbort,  &
                 'ERROR: Unknown option for coupling frequency')
      endif

!-----------------------------------------------------------------------
!
!     register lcoupled if running with the flux coupler
!
!-----------------------------------------------------------------------

      if (lcoupled) call register_string('lcoupled')
      call register_string('init_coupled')

!lipscomb - commented out the ifdef for now
!!!#ifdef COUPGLC
      if (.not. lcoupled) then
        call exit_glc(sigAbort,   &
             'ERROR: Coupled ifdef option enabled but lcoupled=false')
      endif

!-----------------------------------------------------------------------
!
!  Initialize flags
!  Note that the cpl_write_xxx flags have _no_ default value;
!  therefore, they must be explicitly set .true. and .false.
!  at the appropriate times
!
!-----------------------------------------------------------------------

      cpl_stop_now      = init_time_flag('stop_now',default=.false.)
      cpl_ts            = init_time_flag('coupled_ts',                  &
                                         freq_opt = coupled_freq_iopt,  &
                                         freq     = coupled_freq)
      cpl_write_restart = init_time_flag('cpl_write_restart')
      cpl_write_history = init_time_flag('cpl_write_history')
      cpl_write_tavg    = init_time_flag('cpl_write_tavg'   )
      cpl_diag_global   = init_time_flag('cpl_diag_global')
      cpl_diag_transp   = init_time_flag('cpl_diag_transp')

!-----------------------------------------------------------------------
!
!   initialize grid info
!-----------------------------------------------------------------------

      nxg  =  glc_grid%nx
      nyg  =  glc_grid%ny
      lat  => glc_grid%lats
      lon  => glc_grid%lons
      area => glc_grid%box_areas

!-----------------------------------------------------------------------
!
!   initialize and send buffer
!
!-----------------------------------------------------------------------

      isbuf = 0

      isbuf(cpl_fields_ibuf_cdate  ) = iyear*10000 + imonth*100 + iday
      isbuf(cpl_fields_ibuf_sec    ) =   &
         ihour*seconds_in_hour + iminute*seconds_in_minute + isecond

      isbuf(cpl_fields_ibuf_gsize  ) = nxg*nyg
      isbuf(cpl_fields_ibuf_gisize ) = nxg
      isbuf(cpl_fields_ibuf_gjsize ) = nyg
!lipscomb - Do these need to be sent?
!           For now, assume lsize = gsize, etc.
      isbuf(cpl_fields_ibuf_lsize  ) = nxg*nyg
      isbuf(cpl_fields_ibuf_lisize ) = nxg
      isbuf(cpl_fields_ibuf_ljsize ) = nyg
      isbuf(cpl_fields_ibuf_ncpl   ) = ncouple_per_day
      isbuf(cpl_fields_ibuf_nfields) = cpl_fields_grid_total
      isbuf(cpl_fields_ibuf_dead   ) = 0           ! not a dead model

      numg = nxg*nyg
      allocate(sbuf(numg, cpl_fields_grid_total))
      sbuf = -888.0

      ! Pack the send buffer
      ! NOTE: The send buffer assumes global indexing from W to E, S to N.

      do j = 1, nyg
      do i = 1, nxg
         g = (j-1)*nxg + i   ! global index (W to E, S to N)
         sbuf(g,cpl_fields_grid_lon  ) = radian*lon(i)
         sbuf(g,cpl_fields_grid_lat  ) = radian*lat(j)
         sbuf(g,cpl_fields_grid_area ) = area(i,j)/(radius*radius)
         sbuf(g,cpl_fields_grid_mask ) = float(region_mask(i,j))
!lipscomb - This is trivial, yes?
         sbuf(g,cpl_fields_grid_index) = g
      enddo
      enddo

!-----------------------------------------------------------------------
!  initialize the contracts
!-----------------------------------------------------------------------

!lipscomb - debug
     write(6,*) 'Init glc contractS'
     call shr_sys_flush(6)

      call cpl_interface_contractInit(contractS, cpl_fields_glcname,  &
         cpl_fields_cplname, cpl_fields_gg2c_fields, isbuf, sbuf)

!lipscomb - debug
     write(6,*) 'Init glc contractR'
     call shr_sys_flush(6)

      call cpl_interface_contractInit(contractR, cpl_fields_glcname,  &
         cpl_fields_cplname, cpl_fields_c2gg_fields, isbuf, sbuf)

      write(stdout,*) '(glc) Initialized contracts with coupler'
      call shr_sys_flush(stdout)

      deallocate(sbuf)

!lipscomb - debug
     write(6,*) 'Determine send and receive indices'
     call shr_sys_flush(6)

      !--- Determine send indices 
      index_gg2c_Sg_gfrac    = cpl_interface_contractIndex(contractS,'Sg_gfrac') 
      index_gg2c_Sg_gthck    = cpl_interface_contractIndex(contractS,'Sg_gthck') 
      index_gg2c_Sg_gtopo    = cpl_interface_contractIndex(contractS,'Sg_gtopo') 
      index_gg2c_Flgg_ghflx  = cpl_interface_contractIndex(contractS,'Flgg_ghflx') 
      index_gg2c_Flgg_groff  = cpl_interface_contractIndex(contractS,'Flgg_groff') 
 
      !--- Determine receive indices
      index_c2gg_Sl_tsfc   = cpl_interface_contractIndex(contractR,'Sl_tsfc') 
      index_c2gg_Sl_topo   = cpl_interface_contractIndex(contractR,'Sl_topo') 
      index_c2gg_Flgl_qice = cpl_interface_contractIndex(contractR,'Flgl_qice') 
 
!lipscomb - debug
     write(6,*) 'Receive initial message from coupler'
     call shr_sys_flush(6)

      !--- receive initial message from coupler
      call cpl_interface_ibufRecv(cpl_fields_cplname,irbuf)

!-----------------------------------------------------------------------
!
!  send initial state info to coupler
!
!-----------------------------------------------------------------------

!lipscomb - debug
     write(6,*) 'Send initial message to coupler'
     call shr_sys_flush(6)

!lipscomb - Is this send needed?
      call send_to_coupler

!-----------------------------------------------------------------------
!
!  initialize timers for coupled model
!
!-----------------------------------------------------------------------

!lipscomb - commented out for now      
!!!      call get_glc_timer (timer_send_to_cpl)
!!!      call get_glc_timer (timer_recv_from_cpl)
!!!      call get_glc_timer (timer_recv_to_send)
!!!      call get_glc_timer (timer_send_to_recv)

      call flushm (stdout)

!lipscomb - commented out the ifdef for now
!!!#endif

!lipscomb - debug
      write(6,*) 'Leaving init_coupled'
      call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 end subroutine init_coupled

!***********************************************************************

!lipscomb - commented out the ifdef for now 
!!!#ifdef COUPGLC
!BOP
! !IROUTINE: recv_from_coupler
! !INTERFACE:

 subroutine recv_from_coupler

! !DESCRIPTION:
!  This routine receives messages from the coupler.
!
! !REVISION HISTORY:
!  same as module
!
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   character (char_len) :: label
 
   integer (int_kind) ::  &
      nxg, nyg,           &! global grid dimensions
      nrecv,              &! number of fields to receive
      numg,               &! number of global grid cells
      i,j,jj,k,g,n         ! indices

   real (r8) :: gsum       ! global sum (on single processor)

   real(r8), dimension(:,:), pointer ::    &
      area                 ! grid cell area (m^2)

!-----------------------------------------------------------------------
!
!  initialize grid info
!
!-----------------------------------------------------------------------

   nxg = glc_grid%nx
   nyg = glc_grid%ny
   area => glc_grid%box_areas

!lipscomb - debug
   write(6,*) 'In recv_from_coupler'
   write(6,*) 'nxg, nyg =', nxg, nyg
   call shr_sys_flush(6)

!-----------------------------------------------------------------------
!
!  receive message from coupler
!
!-----------------------------------------------------------------------

   nrecv = cpl_interface_contractNumatt(contractR)
   numg = nxg*nyg
   allocate(sbuf(numg, nrecv))

!lipscomb - debug
   write(6,*) 'cpl_interface_contractRecv'
   call shr_sys_flush(6)

   call cpl_interface_contractRecv(cpl_fields_cplname,contractR,irbuf,sbuf)

!-----------------------------------------------------------------------
!
!  check all coupler flags and respond appropriately
!lipscomb - This code is from POP.  Use for glc also?
!
!-----------------------------------------------------------------------

   if (irbuf(cpl_fields_ibuf_stopnow) == 1) then
     call set_time_flag(cpl_stop_now,.true.)
     if (my_task == master_task) then
       call int_to_char (4,iyear   , cyear  )
       call int_to_char (2,imonth  , cmonth )
       call int_to_char (2,iday    , cday   )
       call int_to_char (2,ihour   , chour  )
       call int_to_char (2,iminute , cminute)
       call int_to_char (2,isecond , csecond)
       write(stdout,*) '(recv_from_coupler) ',  &
        'cpl requests termination now: ', &
        cyear,'/',cmonth,'/',cday,' ', chour,':',cminute,':',csecond
     endif
     RETURN
   endif

   if (irbuf(cpl_fields_ibuf_infobug) >= 2) then
     ldiag_cpl = .true. 
   else
     ldiag_cpl = .false.
   endif

   if (irbuf(cpl_fields_ibuf_resteod) == 1) then
     call set_time_flag(cpl_write_restart,.true.)
     if (my_task == master_task) then
       write(stdout,*) '(recv_from_coupler) ', &
         'cpl requests restart file at eod  ',cyear,'/',cmonth,'/',cday
     endif
   endif
  
   if (irbuf(cpl_fields_ibuf_histeod) == 1) then
     call set_time_flag(cpl_write_history,.true.)
     call int_to_char (4,iyear   , cyear )
     call int_to_char (2,imonth  ,cmonth )
     call int_to_char (2,iday    ,cday   )
     call int_to_char (2,ihour   ,chour  )
     call int_to_char (2,iminute ,cminute)
     call int_to_char (2,isecond ,csecond)
     if (my_task == master_task) then
       write(stdout,*) ' cpl requests history file at eod ',  &
                       ' ', cyear,'/',cmonth,'/',cday, '  '
     endif
   endif

   if (irbuf(cpl_fields_ibuf_histavg) == 1) then
     call set_time_flag(cpl_write_tavg, .true.)
     call int_to_char (4,iyear   , cyear )
     call int_to_char (2,imonth  ,cmonth )
     call int_to_char (2,iday    ,cday   )
     call int_to_char (2,ihour   ,chour  )
     call int_to_char (2,iminute ,cminute)
     call int_to_char (2,isecond ,csecond)
     if (my_task == master_task) then
       write(stdout,*) ' cpl requests tavg file at eod ',  &
                       ' ', cyear,'/',cmonth,'/',cday, '  '
     endif
   endif
  
   if (irbuf(cpl_fields_ibuf_diageod) == 1) then
     call set_time_flag(cpl_diag_global,.true.)
     call set_time_flag(cpl_diag_transp,.true.)
     call int_to_char (4,iyear   ,cyear  )
     call int_to_char (2,imonth  ,cmonth )
     call int_to_char (2,iday    ,cday   )
     call int_to_char (2,ihour   ,chour  )
     call int_to_char (2,iminute ,cminute)
     call int_to_char (2,isecond ,csecond)
     if (my_task == master_task) then
       write(stdout,*) ' cpl requests diagnostics at eod ' , &
                       ' ', cyear,'/',cmonth,'/',cday, '  '
     endif
   endif

!-----------------------------------------------------------------------
!
!  unpack and distribute recv buffer
!  Note: The recv fields (tsfc, topo, qice) are located on the global
!        clm/glc grid.
!
!-----------------------------------------------------------------------

!lipscomb - modify for elevation classes and ice sheet mask

      n = 1                   ! elevation class; only one for now
      do j = 1, nyg           ! S to N
         jj = nyg - j + 1     ! reversed j index for glint grid (N to S)
         do i = 1, nxg
            g = (j-1)*nxg + i   ! global index (W to E, S to N)
            tsfc(i,jj,n) = sbuf(g,index_c2gg_Sl_tsfc)
            topo(i,jj,n) = sbuf(g,index_c2gg_Sl_topo)
            qice(i,jj,n) = sbuf(g,index_c2gg_Flgl_qice)
         enddo
      enddo

      ! unit conversions
      tsfc(:,:,:) = tsfc(:,:,:) - tkfrz    ! Kelvin to Celsius

!-----------------------------------------------------------------------
!
!  diagnostics
!
!-----------------------------------------------------------------------

   if (ldiag_cpl) then

      if  (my_task == master_task) then
         call int_to_char (4,iyear   ,cyear  )
         call int_to_char (2,imonth  ,cmonth )
         call int_to_char (2,iday    ,cday   )
         call int_to_char (2,ihour   ,chour  )
         call int_to_char (2,iminute ,cminute)
         call int_to_char (2,isecond ,csecond)
         write(stdout,*)' Global averages of fluxes received from cpl',  &
                         ' at ', cyear,'/',cmonth ,'/',cday,             &
                            ' ', chour,':',cminute,':',csecond
         call shr_sys_flush(stdout)
      endif
 
      do k = 1,nrecv

         gsum = c0
         do j = 1, nyg
         do i = 1, nxg
            g = (j-1)*nxg + i   ! global index (W to E, S to N)
            gsum = gsum + sbuf(g,k)*area(i,j)     !lipscomb - multiply by a mask?
         enddo
         enddo

         if (my_task == master_task) then
            call cpl_fields_getField(label,k,cpl_fields_c2gg_fields)
            write(stdout,1100) 'glc', 'recv', label , gsum
         endif

      enddo   ! nrecv

      if (my_task == master_task) call shr_sys_flush(stdout)
   endif      ! ldiag_cpl

   deallocate(sbuf)

1100  format ('comm_diag ', a3, 1x, a4, 1x, a8, 1x, es26.19:, 1x, a6)

!lipscomb - debug
   write(6,*) 'Done in recv_from_coupler'
   call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 end subroutine recv_from_coupler

!***********************************************************************

!BOP
! !IROUTINE: send_to_coupler
! !INTERFACE:

 subroutine send_to_coupler

! !DESCRIPTION:
!  This routine packs fields into a message buffer and sends the
!  message to the coupler
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   character (char_len)    :: label
 
   integer (int_kind) ::  &
      i,j,jj,k,g,n,       &! indices
      nxg, nyg,           &! global grid dimensions
      numg,               &! number of global gridcells 
      nsend                ! number of fields to send

   real (r8) :: gsum    ! global sum (on single processor)

   real(r8), dimension(:,:), allocatable ::    &
      area                 ! grid cell area (m^2)

!lipscomb - debug
    write(6,*) 'In send_to_coupler'
    call shr_sys_flush(6)

!-----------------------------------------------------------------------
!
!  initialize grid info
!
!-----------------------------------------------------------------------

      nxg = glc_grid%nx
      nyg = glc_grid%ny

!lipscomb - debug
    write(6,*) 'allocate'
    call shr_sys_flush(6)

      allocate (area(nxg,nyg))
      area(:,:) = glc_grid%box_areas(:,:)

!lipscomb - debug
    write(6,*) 'Init buffers'
    call shr_sys_flush(6)

!-----------------------------------------------------------------------
!
!  initialize buffers
!
!-----------------------------------------------------------------------

      isbuf = 0

      if (check_time_flag(cpl_stop_now)) then
        isbuf(cpl_fields_ibuf_stopnow) = 1
      endif

      isbuf(cpl_fields_ibuf_cdate) = iyear*10000 + imonth*100 + iday
      isbuf(cpl_fields_ibuf_sec) =   &
         ihour*seconds_in_hour + iminute*seconds_in_minute + isecond

      nsend = cpl_interface_contractNumatt(contractS)
      numg = nxg*nyg
      allocate(sbuf(numg, nsend))

!-----------------------------------------------------------------------
!
!     pack the send buffer
!
!-----------------------------------------------------------------------

!lipscomb - debug
    write(6,*) 'Pack send buffer'
    call shr_sys_flush(6)

!lipscomb - Modify for elevation classes and ice sheet mask.
!           Switch sign of ghflx?

      n = 1                 ! assume one elev class for now
      do j = 1, nyg         ! S to N
         jj = nyg - j + 1   ! reverse for glint grid (N to S)
         do i = 1, nxg
            g = (j-1)*nxg + i   ! global index
            sbuf(g,index_gg2c_Sg_gfrac)   = gfrac(i,jj,n)
            sbuf(g,index_gg2c_Sg_gthck)   = gthck(i,jj,n)
            sbuf(g,index_gg2c_Sg_gtopo)   = gtopo(i,jj,n)
            sbuf(g,index_gg2c_Flgg_ghflx) = ghflx(i,jj,n)
            sbuf(g,index_gg2c_Flgg_groff) = groff(i,jj)
         enddo
      enddo

!-----------------------------------------------------------------------
!
!  send fields to coupler
!
!-----------------------------------------------------------------------

!lipscomb - debug
    write(6,*) 'Send fields'
    call shr_sys_flush(6)

      call cpl_interface_contractSend(cpl_fields_cplname,contractS,isbuf,sbuf)

!lipscomb - debug
    write(6,*) 'Fields have been sent'
    call shr_sys_flush(6)
 
!-----------------------------------------------------------------------
!
!     diagnostics
!
!-----------------------------------------------------------------------

      if (ldiag_cpl) then

        if (my_task == master_task) then
          call int_to_char (4,iyear   ,cyear  )
          call int_to_char (2,imonth  ,cmonth )
          call int_to_char (2,iday    ,cday   )
          call int_to_char (2,ihour   ,chour  )
          call int_to_char (2,iminute ,cminute)
          call int_to_char (2,isecond ,csecond)
          write(stdout,*) ' Global averages of fluxes sent to cpl at '  &
      ,                   ' ', cyear,'/',cmonth, '/',cday    &
      ,                   ' ', chour,':',cminute,':',csecond
          call shr_sys_flush(stdout)
        endif
 
        do k = 1, nsend

           gsum = c0
           do j = 1, nyg
           do i = 1, nxg
              g = (j-1)*nxg
              gsum = gsum + sbuf(g,k)*area(i,j)     !lipscomb - multiply by a mask?
           enddo
           enddo

           if (my_task == master_task) then
              call cpl_fields_getField(label,k,cpl_fields_o2c_fields)
              write(stdout,1100)'ocn','send', label ,gsum
           endif
        enddo  ! k

        if (my_task == master_task) call shr_sys_flush(stdout)

      endif   ! ldiag_cpl

1100  format ('comm_diag ', a3, 1x, a4, 1x, a8, 1x, es26.19:, 1x, a6)

      deallocate(sbuf)

!lipscomb - debug
    write(6,*) 'Done in send_to_coupler'
    call shr_sys_flush(6)

!-----------------------------------------------------------------------
!EOC

 end subroutine send_to_coupler

!***********************************************************************

!-----------------------------------------------------------------------
!EOC
 
!lipscomb - comment out the ifdef for now
!!!#endif
 
!***********************************************************************

 end module glc_coupled

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

