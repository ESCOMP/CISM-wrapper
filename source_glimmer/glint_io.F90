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


module glint_io
  !*FD template for creating subsystem specific I/O routines
  !*FD written by Magnus Hagdorn, 2004

  character(len=*),private,parameter :: hotvars = ' mask  glint_siced  glint_snowd '

contains

  !*****************************************************************************
  ! netCDF output
  !*****************************************************************************
  subroutine glint_io_createall(model,data)
    !*FD open all netCDF files for output
    use glint_type
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glide_global_type) :: model
    type(glint_instance), optional :: data
    
    ! local variables
    type(glimmer_nc_output), pointer :: oc

    oc=>model%funits%out_first
    do while(associated(oc))
       if (present(data)) then
          call glint_io_create(oc,model,data)
       else
          call glint_io_create(oc,model)
       end if
       oc=>oc%next
    end do
  end subroutine glint_io_createall

  subroutine glint_io_writeall(data,model,atend)
    !*FD if necessary write to netCDF files
    use glint_type
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glint_instance) :: data
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
          call glint_avg_accumulate(oc,data,model)
       end if
#endif
       call glimmer_nc_checkwrite(oc,model,forcewrite)
       if (oc%nc%just_processed) then
          ! write standard variables
          call glint_io_write(oc,data)
#ifdef HAVE_AVG
          if (oc%do_averages) then
             call glint_avg_reset(oc,data)
          end if
#endif
       end if
       oc=>oc%next
    end do
  end subroutine glint_io_writeall
  
  subroutine glint_io_create(outfile,model,data)
    use glide_types
    use glint_type
    use glimmer_ncdf
    use glimmer_map_types
    use glimmer_log
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    type(glide_global_type) :: model
    type(glint_instance), optional :: data

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

    !     glint_ablt -- ablation
    pos = index(NCO%vars,' glint_ablt ')
    status = nf90_inq_varid(NCO%id,'glint_ablt',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_ablt')
       status = nf90_def_var(NCO%id,'glint_ablt',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ablation')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter (water)/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_acab -- mass-balance
    pos = index(NCO%vars,' glint_acab ')
    status = nf90_inq_varid(NCO%id,'glint_acab',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_acab')
       status = nf90_def_var(NCO%id,'glint_acab',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'mass-balance')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_surface_specific_mass_balance')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter (water)/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_arng -- air temperature half-range
    pos = index(NCO%vars,' glint_arng ')
    status = nf90_inq_varid(NCO%id,'glint_arng',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_arng')
       status = nf90_def_var(NCO%id,'glint_arng',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'air temperature half-range')
       status = nf90_put_att(NCO%id, varid, 'units', 'degreeC')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_artm -- air temperature
    pos = index(NCO%vars,' glint_artm ')
    status = nf90_inq_varid(NCO%id,'glint_artm',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_artm')
       status = nf90_def_var(NCO%id,'glint_artm',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'air temperature')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_temperature')
       status = nf90_put_att(NCO%id, varid, 'units', 'degreeC')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_prcp -- precipitation
    pos = index(NCO%vars,' glint_prcp ')
    status = nf90_inq_varid(NCO%id,'glint_prcp',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_prcp')
       status = nf90_def_var(NCO%id,'glint_prcp',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'precipitation')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'lwe_precipitation_rate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter (water)/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_siced -- superimposed ice depth
    pos = index(NCO%vars,' glint_siced ')
    status = nf90_inq_varid(NCO%id,'glint_siced',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+11) = '           '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_siced')
       status = nf90_def_var(NCO%id,'glint_siced',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'superimposed ice depth')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     glint_snowd -- snow depth
    pos = index(NCO%vars,' glint_snowd ')
    status = nf90_inq_varid(NCO%id,'glint_snowd',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+11) = '           '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable glint_snowd')
       status = nf90_def_var(NCO%id,'glint_snowd',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'snow depth')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_snow_thickness')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     global_orog -- orographic elevation provided by global model
    pos = index(NCO%vars,' global_orog ')
    status = nf90_inq_varid(NCO%id,'global_orog',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+11) = '           '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable global_orog')
       status = nf90_def_var(NCO%id,'global_orog',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'orographic elevation provided by global model')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_altitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     local_orog -- orographic elevation provided by local model
    pos = index(NCO%vars,' local_orog ')
    status = nf90_inq_varid(NCO%id,'local_orog',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable local_orog')
       status = nf90_def_var(NCO%id,'local_orog',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'orographic elevation provided by local model')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_altitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     mask -- upscaling and downscaling mask
    pos = index(NCO%vars,' mask ')
    status = nf90_inq_varid(NCO%id,'mask',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable mask')
       status = nf90_def_var(NCO%id,'mask',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'upscaling and downscaling mask')
       status = nf90_put_att(NCO%id, varid, 'units', '1')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

  end subroutine glint_io_create

  subroutine glint_io_write(outfile,data)
    use glint_type
    use glimmer_ncdf
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glint_instance) :: data
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
    status = nf90_inq_varid(NCO%id,'glint_ablt',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%ablt, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_acab',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%acab, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_arng',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%arng, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_artm',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%artm, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_prcp',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%prcp, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_siced',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%siced, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'glint_snowd',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%snowd, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'global_orog',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%global_orog, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'local_orog',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%local_orog, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'mask',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%out_mask, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

  end subroutine glint_io_write

  !*****************************************************************************
  ! netCDF input
  !*****************************************************************************  
  subroutine glint_io_readall(data,model)
    !*FD read from netCDF file
    use glint_type
    use glide_types
    use glimmer_ncio
    use glimmer_ncdf
    implicit none
    type(glint_instance) :: data
    type(glide_global_type) :: model

    ! local variables
    type(glimmer_nc_input), pointer :: ic    

    ic=>model%funits%in_first
    do while(associated(ic))
       call glimmer_nc_checkread(ic,model)
       if (ic%nc%just_processed) then
          call glint_io_read(ic,data)
       end if
       ic=>ic%next
    end do
  end subroutine glint_io_readall

  subroutine glint_io_read(infile,data,scale_vars)
    !*FD read variables from a netCDF file
    use glimmer_log
    use glimmer_ncdf
    use glint_type
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glint_instance) :: data
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
    status = nf90_inq_varid(NCI%id,'glint_siced',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading glint_siced')
       status = nf90_get_var(NCI%id, varid, &
            data%siced, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'glint_snowd',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading glint_snowd')
       status = nf90_get_var(NCI%id, varid, &
            data%snowd, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'mask',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading mask')
       status = nf90_get_var(NCI%id, varid, &
            data%out_mask, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

  end subroutine glint_io_read

  subroutine glint_io_checkdim(infile,model,data)
    !*FD check if dimension sizes in file match dims of model
    use glimmer_log
    use glimmer_ncdf
    use glide_types
    use glint_type
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glint_instance), optional :: data

    integer status,dimid,dimsize
    character(len=150) message

    ! check dimensions
  end subroutine glint_io_checkdim

  !*****************************************************************************
  ! calculating time averages
  !*****************************************************************************  
#ifdef HAVE_AVG
  subroutine glint_avg_accumulate(outfile,data,model)
    use glide_types
    use glint_type
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glint_instance) :: data

    ! local variables
    real :: factor
    integer status, varid

    ! increase total time
    outfile%total_time = outfile%total_time + model%numerics%tinc
    factor = model%numerics%tinc

  end subroutine glint_avg_accumulate

  subroutine glint_avg_reset(outfile,data)
    use glint_type
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glint_instance) :: data

    ! local variables
    integer status, varid

    ! reset total time
    outfile%total_time = 0.

  end subroutine glint_avg_reset
#endif

  !*********************************************************************
  ! lots of accessor subroutines follow
  !*********************************************************************
  subroutine glint_get_glint_ablt(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%ablt
  end subroutine glint_get_glint_ablt

  subroutine glint_set_glint_ablt(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%ablt = inarray
  end subroutine glint_set_glint_ablt

  subroutine glint_get_glint_acab(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%acab
  end subroutine glint_get_glint_acab

  subroutine glint_set_glint_acab(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%acab = inarray
  end subroutine glint_set_glint_acab

  subroutine glint_get_glint_arng(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%arng
  end subroutine glint_get_glint_arng

  subroutine glint_set_glint_arng(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%arng = inarray
  end subroutine glint_set_glint_arng

  subroutine glint_get_glint_artm(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%artm
  end subroutine glint_get_glint_artm

  subroutine glint_set_glint_artm(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%artm = inarray
  end subroutine glint_set_glint_artm

  subroutine glint_get_glint_prcp(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%prcp
  end subroutine glint_get_glint_prcp

  subroutine glint_set_glint_prcp(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%prcp = inarray
  end subroutine glint_set_glint_prcp

  subroutine glint_get_glint_siced(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%siced
  end subroutine glint_get_glint_siced

  subroutine glint_set_glint_siced(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%siced = inarray
  end subroutine glint_set_glint_siced

  subroutine glint_get_glint_snowd(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%snowd
  end subroutine glint_get_glint_snowd

  subroutine glint_set_glint_snowd(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%snowd = inarray
  end subroutine glint_set_glint_snowd

  subroutine glint_get_global_orog(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%global_orog
  end subroutine glint_get_global_orog

  subroutine glint_set_global_orog(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%global_orog = inarray
  end subroutine glint_set_global_orog

  subroutine glint_get_local_orog(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%local_orog
  end subroutine glint_get_local_orog

  subroutine glint_set_local_orog(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%local_orog = inarray
  end subroutine glint_set_local_orog

  subroutine glint_get_mask(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%out_mask
  end subroutine glint_get_mask

  subroutine glint_set_mask(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glint_type
    implicit none
    type(glint_instance) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%out_mask = inarray
  end subroutine glint_set_mask


end module glint_io
