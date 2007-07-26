!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING: this file was automatically generated on
! Tue, 08 May 2007 21:10:22 +0000
! from ncdf_template.F90.in
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                           +
! +  ncdf_template.f90 - part of the GLIMMER ice model        + 
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

#define NCO outfile%nc
#define NCI infile%nc


module glint_mbal_io
  !*FD template for creating subsystem specific I/O routines
  !*FD written by Magnus Hagdorn, 2004

  character(len=*),private,parameter :: hotvars = ''

contains

  !*****************************************************************************
  ! netCDF output
  !*****************************************************************************
  subroutine glint_mbal_io_createall(model,data)
    !*FD open all netCDF files for output
    use glint_mbal_coupling
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glide_global_type) :: model
    type(glint_mbc), optional :: data
    
    ! local variables
    type(glimmer_nc_output), pointer :: oc

    oc=>model%funits%out_first
    do while(associated(oc))
       if (present(data)) then
          call glint_mbal_io_create(oc,model,data)
       else
          call glint_mbal_io_create(oc,model)
       end if
       oc=>oc%next
    end do
  end subroutine glint_mbal_io_createall

  subroutine glint_mbal_io_writeall(data,model,atend)
    !*FD if necessary write to netCDF files
    use glint_mbal_coupling
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glint_mbc) :: data
    type(glide_global_type) :: model
    logical, optional :: atend

    ! local variables
    type(glimmer_nc_output), pointer :: oc
    logical :: forcewrite=.false.

    if (present(atend)) then
       forcewrite = atend
    end if

    oc=>model%funits%out_first
    do while(associated(oc))
#ifdef HAVE_AVG
       if (oc%do_averages) then
          call glint_mbal_avg_accumulate(oc,data,model)
       end if
#endif
       call glimmer_nc_checkwrite(oc,model,forcewrite)
       if (oc%nc%just_processed) then
          ! write standard variables
          call glint_mbal_io_write(oc,data)
#ifdef HAVE_AVG
          if (oc%do_averages) then
             call glint_mbal_avg_reset(oc,data)
          end if
#endif
       end if
       oc=>oc%next
    end do
  end subroutine glint_mbal_io_writeall
  
  subroutine glint_mbal_io_create(outfile,model,data)
    use glide_types
    use glint_mbal_coupling
    use glimmer_ncdf
    use glimmer_map_types
    use glimmer_log
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    type(glide_global_type) :: model
    type(glint_mbc), optional :: data

    integer status,varid,pos

    integer :: time_dimid
    integer :: x1_dimid
    integer :: y1_dimid

    ! defining dimensions
    status = nf90_inq_dimid(NCO%id,'time',time_dimid)
    call nc_errorhandle(__FILE__,__LINE__,status)
    status = nf90_inq_dimid(NCO%id,'x1',x1_dimid)
    call nc_errorhandle(__FILE__,__LINE__,status)
    status = nf90_inq_dimid(NCO%id,'y1',y1_dimid)
    call nc_errorhandle(__FILE__,__LINE__,status)

    NCO%vars = ' '//trim(NCO%vars)//' '
    ! expanding hotstart variables
    pos = index(NCO%vars,' hot ') 
    if (pos.ne.0) then
       NCO%vars = NCO%vars(:pos)//NCO%vars(pos+4:)
       NCO%hotstart = .true.
    end if
    if (NCO%hotstart) then
       NCO%vars = trim(NCO%vars)//hotvars
    end if
    ! checking if we need to handle time averages
    pos = index(NCO%vars,"_tavg")
    if (pos.ne.0) then
       outfile%do_averages = .True.
    end if    

    !     instant_ablt -- instantaneous ablation
    pos = index(NCO%vars,' instant_ablt ')
    status = nf90_inq_varid(NCO%id,'instant_ablt',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+12) = '            '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_ablt')
       status = nf90_def_var(NCO%id,'instant_ablt',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous ablation')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_acab -- instantaneous mass-balance
    pos = index(NCO%vars,' instant_acab ')
    status = nf90_inq_varid(NCO%id,'instant_acab',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+12) = '            '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_acab')
       status = nf90_def_var(NCO%id,'instant_acab',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous mass-balance')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_artm -- instantaneous air temperature
    pos = index(NCO%vars,' instant_artm ')
    status = nf90_inq_varid(NCO%id,'instant_artm',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+12) = '            '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_artm')
       status = nf90_def_var(NCO%id,'instant_artm',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous air temperature')
       status = nf90_put_att(NCO%id, varid, 'units', 'degC')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_humidity -- instantaneous humidity
    pos = index(NCO%vars,' instant_humidity ')
    status = nf90_inq_varid(NCO%id,'instant_humidity',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+16) = '                '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_humidity')
       status = nf90_def_var(NCO%id,'instant_humidity',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous humidity')
       status = nf90_put_att(NCO%id, varid, 'units', '1')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_lwdown -- instantaneous lw down
    pos = index(NCO%vars,' instant_lwdown ')
    status = nf90_inq_varid(NCO%id,'instant_lwdown',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+14) = '              '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_lwdown')
       status = nf90_def_var(NCO%id,'instant_lwdown',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous lw down')
       status = nf90_put_att(NCO%id, varid, 'units', 'W/m^2')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_prcp -- instantaneous precip
    pos = index(NCO%vars,' instant_prcp ')
    status = nf90_inq_varid(NCO%id,'instant_prcp',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+12) = '            '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_prcp')
       status = nf90_def_var(NCO%id,'instant_prcp',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous precip')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_psurf -- instantaneous surface pressure
    pos = index(NCO%vars,' instant_psurf ')
    status = nf90_inq_varid(NCO%id,'instant_psurf',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+13) = '             '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_psurf')
       status = nf90_def_var(NCO%id,'instant_psurf',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous surface pressure')
       status = nf90_put_att(NCO%id, varid, 'units', 'Pa')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_swdown -- instantaneous sw down
    pos = index(NCO%vars,' instant_swdown ')
    status = nf90_inq_varid(NCO%id,'instant_swdown',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+14) = '              '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_swdown')
       status = nf90_def_var(NCO%id,'instant_swdown',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous sw down')
       status = nf90_put_att(NCO%id, varid, 'units', 'W/m^2')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_xwind -- instantaneous x-wind
    pos = index(NCO%vars,' instant_xwind ')
    status = nf90_inq_varid(NCO%id,'instant_xwind',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+13) = '             '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_xwind')
       status = nf90_def_var(NCO%id,'instant_xwind',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous x-wind')
       status = nf90_put_att(NCO%id, varid, 'units', 'm/s')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     instant_ywind -- instantaneous y-wind
    pos = index(NCO%vars,' instant_ywind ')
    status = nf90_inq_varid(NCO%id,'instant_ywind',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+13) = '             '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable instant_ywind')
       status = nf90_def_var(NCO%id,'instant_ywind',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'instantaneous y-wind')
       status = nf90_put_att(NCO%id, varid, 'units', 'm/s')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     siced -- superimposed ice depth
    pos = index(NCO%vars,' siced ')
    status = nf90_inq_varid(NCO%id,'siced',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable siced')
       status = nf90_def_var(NCO%id,'siced',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'superimposed ice depth')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     snowd -- snow depth
    pos = index(NCO%vars,' snowd ')
    status = nf90_inq_varid(NCO%id,'snowd',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable snowd')
       status = nf90_def_var(NCO%id,'snowd',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'snow depth')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

  end subroutine glint_mbal_io_create

  subroutine glint_mbal_io_write(outfile,data)
    use glint_mbal_coupling
    use glimmer_ncdf
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glint_mbc) :: data
    !*FD the model instance

    ! local variables
    real tavgf
    integer status, varid
    integer up
     
    tavgf = outfile%total_time
    if (tavgf.ne.0.) then
       tavgf = 1./tavgf
    end if

    ! write variables
    status = nf90_inq_varid(NCO%id,'instant_ablt',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%ablt, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_acab',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%acab, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_artm',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%artm, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_humidity',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%humidity, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_lwdown',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%lwdown, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_prcp',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%prcp, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_psurf',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%psurf, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_swdown',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%swdown, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_xwind',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%xwind, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'instant_ywind',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%ywind, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'siced',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%siced, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'snowd',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%snowd, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

  end subroutine glint_mbal_io_write

  !*****************************************************************************
  ! netCDF input
  !*****************************************************************************  
  subroutine glint_mbal_io_readall(data,model)
    !*FD read from netCDF file
    use glint_mbal_coupling
    use glide_types
    use glimmer_ncio
    use glimmer_ncdf
    implicit none
    type(glint_mbc) :: data
    type(glide_global_type) :: model

    ! local variables
    type(glimmer_nc_input), pointer :: ic    

    ic=>model%funits%in_first
    do while(associated(ic))
       call glimmer_nc_checkread(ic,model)
       if (ic%nc%just_processed) then
          call glint_mbal_io_read(ic,data)
       end if
       ic=>ic%next
    end do
  end subroutine glint_mbal_io_readall

  subroutine glint_mbal_io_read(infile,data,scale_vars)
    !*FD read variables from a netCDF file
    use glimmer_log
    use glimmer_ncdf
    use glint_mbal_coupling
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glint_mbc) :: data
    !*FD the model instance
    logical,optional :: scale_vars
    !*FD Specifies whether fields should be scaled by factors when read in.

    ! local variables
    integer status,varid
    integer up
    logical :: scale=.true.

    ! Deal with optional argument
    if (present(scale_vars)) scale=scale_vars
   
    ! read variables
  end subroutine glint_mbal_io_read

  subroutine glint_mbal_io_checkdim(infile,model,data)
    !*FD check if dimension sizes in file match dims of model
    use glimmer_log
    use glimmer_ncdf
    use glide_types
    use glint_mbal_coupling
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glint_mbc), optional :: data

    integer status,dimid,dimsize
    character(len=150) message

    ! check dimensions
  end subroutine glint_mbal_io_checkdim

  !*****************************************************************************
  ! calculating time averages
  !*****************************************************************************  
#ifdef HAVE_AVG
  subroutine glint_mbal_avg_accumulate(outfile,data,model)
    use glide_types
    use glint_mbal_coupling
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glint_mbc) :: data

    ! local variables
    real :: factor
    integer status, varid

    ! increase total time
    outfile%total_time = outfile%total_time + model%numerics%tinc
    factor = model%numerics%tinc

  end subroutine glint_mbal_avg_accumulate

  subroutine glint_mbal_avg_reset(outfile,data)
    use glint_mbal_coupling
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glint_mbc) :: data

    ! local variables
    integer status, varid

    ! reset total time
    outfile%total_time = 0.

  end subroutine glint_mbal_avg_reset
#endif

  !*********************************************************************
  ! lots of accessor subroutines follow
  !*********************************************************************
  subroutine glint_mbal_get_instant_ablt(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%ablt
  end subroutine glint_mbal_get_instant_ablt

  subroutine glint_mbal_set_instant_ablt(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%ablt = inarray
  end subroutine glint_mbal_set_instant_ablt

  subroutine glint_mbal_get_instant_acab(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%acab
  end subroutine glint_mbal_get_instant_acab

  subroutine glint_mbal_set_instant_acab(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%acab = inarray
  end subroutine glint_mbal_set_instant_acab

  subroutine glint_mbal_get_instant_artm(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%artm
  end subroutine glint_mbal_get_instant_artm

  subroutine glint_mbal_set_instant_artm(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%artm = inarray
  end subroutine glint_mbal_set_instant_artm

  subroutine glint_mbal_get_instant_humidity(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%humidity
  end subroutine glint_mbal_get_instant_humidity

  subroutine glint_mbal_set_instant_humidity(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%humidity = inarray
  end subroutine glint_mbal_set_instant_humidity

  subroutine glint_mbal_get_instant_lwdown(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%lwdown
  end subroutine glint_mbal_get_instant_lwdown

  subroutine glint_mbal_set_instant_lwdown(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%lwdown = inarray
  end subroutine glint_mbal_set_instant_lwdown

  subroutine glint_mbal_get_instant_prcp(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%prcp
  end subroutine glint_mbal_get_instant_prcp

  subroutine glint_mbal_set_instant_prcp(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%prcp = inarray
  end subroutine glint_mbal_set_instant_prcp

  subroutine glint_mbal_get_instant_psurf(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%psurf
  end subroutine glint_mbal_get_instant_psurf

  subroutine glint_mbal_set_instant_psurf(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%psurf = inarray
  end subroutine glint_mbal_set_instant_psurf

  subroutine glint_mbal_get_instant_swdown(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%swdown
  end subroutine glint_mbal_get_instant_swdown

  subroutine glint_mbal_set_instant_swdown(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%swdown = inarray
  end subroutine glint_mbal_set_instant_swdown

  subroutine glint_mbal_get_instant_xwind(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%xwind
  end subroutine glint_mbal_get_instant_xwind

  subroutine glint_mbal_set_instant_xwind(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%xwind = inarray
  end subroutine glint_mbal_set_instant_xwind

  subroutine glint_mbal_get_instant_ywind(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%ywind
  end subroutine glint_mbal_get_instant_ywind

  subroutine glint_mbal_set_instant_ywind(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%ywind = inarray
  end subroutine glint_mbal_set_instant_ywind

  subroutine glint_mbal_get_siced(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%siced
  end subroutine glint_mbal_get_siced

  subroutine glint_mbal_set_siced(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%siced = inarray
  end subroutine glint_mbal_set_siced

  subroutine glint_mbal_get_snowd(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%snowd
  end subroutine glint_mbal_get_snowd

  subroutine glint_mbal_set_snowd(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_mbal_coupling
    implicit none
    type(glint_mbc) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%snowd = inarray
  end subroutine glint_mbal_set_snowd


end module glint_mbal_io
