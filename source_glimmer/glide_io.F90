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

#define HAVE_AVG 1

module glide_io
  !*FD template for creating subsystem specific I/O routines
  !*FD written by Magnus Hagdorn, 2004

  character(len=*),private,parameter :: hotvars = ' bwat  bmlt  topg  relx  litho_temp  lat  bheatflx  flwa  temp  age  thk '

contains

  !*****************************************************************************
  ! netCDF output
  !*****************************************************************************
  subroutine glide_io_createall(model,data)
    !*FD open all netCDF files for output
    use glide_types
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glide_global_type) :: model
    type(glide_global_type), optional :: data
    
    ! local variables
    type(glimmer_nc_output), pointer :: oc

    oc=>model%funits%out_first
    do while(associated(oc))
       if (present(data)) then
          call glide_io_create(oc,model,data)
       else
          call glide_io_create(oc,model)
       end if
       oc=>oc%next
    end do
  end subroutine glide_io_createall

  subroutine glide_io_writeall(data,model,atend)
    !*FD if necessary write to netCDF files
    use glide_types
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none
    type(glide_global_type) :: data
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
          call glide_avg_accumulate(oc,data,model)
       end if
#endif
       call glimmer_nc_checkwrite(oc,model,forcewrite)
       if (oc%nc%just_processed) then
          ! write standard variables
          call glide_io_write(oc,data)
#ifdef HAVE_AVG
          if (oc%do_averages) then
             call glide_avg_reset(oc,data)
          end if
#endif
       end if
       oc=>oc%next
    end do
  end subroutine glide_io_writeall
  
  subroutine glide_io_create(outfile,model,data)
    use glide_types
    use glide_types
    use glimmer_ncdf
    use glimmer_map_types
    use glimmer_log
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    type(glide_global_type) :: model
    type(glide_global_type), optional :: data

    integer status,varid,pos

    integer :: level_dimid
    integer :: lithoz_dimid
    integer :: time_dimid
    integer :: x0_dimid
    integer :: x1_dimid
    integer :: y0_dimid
    integer :: y1_dimid

    ! defining dimensions
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'level',model%general%upn,level_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'level',level_dimid)
    endif
    call nc_errorhandle(__FILE__,__LINE__,status)
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'lithoz',model%lithot%nlayer,lithoz_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'lithoz',lithoz_dimid)
    endif
    call nc_errorhandle(__FILE__,__LINE__,status)
    status = nf90_inq_dimid(NCO%id,'time',time_dimid)
    call nc_errorhandle(__FILE__,__LINE__,status)
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'x0',model%general%ewn-1,x0_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'x0',x0_dimid)
    endif
    call nc_errorhandle(__FILE__,__LINE__,status)
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'x1',model%general%ewn,x1_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'x1',x1_dimid)
    endif
    call nc_errorhandle(__FILE__,__LINE__,status)
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'y0',model%general%nsn-1,y0_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'y0',y0_dimid)
    endif
    call nc_errorhandle(__FILE__,__LINE__,status)
    if (.not.outfile%append) then
       status = nf90_def_dim(NCO%id,'y1',model%general%nsn,y1_dimid)
    else
       status = nf90_inq_dimid(NCO%id,'y1',y1_dimid)
    endif
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

    !     level -- sigma layers
    if (.not.outfile%append) then
       call write_log('Creating variable level')
       status = nf90_def_var(NCO%id,'level',NF90_FLOAT,(/level_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'formula_terms', 'sigma: level topo: topg thick: thk')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'sigma layers')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_sigma_coordinate')
       status = nf90_put_att(NCO%id, varid, 'units', '1')
     end if

    !     lithoz -- vertical coordinate of lithosphere layer
    if (.not.outfile%append) then
       call write_log('Creating variable lithoz')
       status = nf90_def_var(NCO%id,'lithoz',NF90_FLOAT,(/lithoz_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'vertical coordinate of lithosphere layer')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
     end if

    !     x0 -- Cartisian x-coordinate, velocity grid
    if (.not.outfile%append) then
       call write_log('Creating variable x0')
       status = nf90_def_var(NCO%id,'x0',NF90_FLOAT,(/x0_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Cartisian x-coordinate, velocity grid')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'projection_x_coordinate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
     end if

    !     x1 -- Cartisian x-coordinate
    if (.not.outfile%append) then
       call write_log('Creating variable x1')
       status = nf90_def_var(NCO%id,'x1',NF90_FLOAT,(/x1_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Cartisian x-coordinate')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'projection_x_coordinate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
     end if

    !     y0 -- Cartisian y-coordinate, velocity grid
    if (.not.outfile%append) then
       call write_log('Creating variable y0')
       status = nf90_def_var(NCO%id,'y0',NF90_FLOAT,(/y0_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Cartisian y-coordinate, velocity grid')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'projection_y_coordinate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
     end if

    !     y1 -- Cartisian y-coordinate
    if (.not.outfile%append) then
       call write_log('Creating variable y1')
       status = nf90_def_var(NCO%id,'y1',NF90_FLOAT,(/y1_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Cartisian y-coordinate')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'projection_y_coordinate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
     end if

    !     acab -- accumulation, ablation rate
    pos = index(NCO%vars,' acab ')
    status = nf90_inq_varid(NCO%id,'acab',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable acab')
       status = nf90_def_var(NCO%id,'acab',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'accumulation, ablation rate')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_surface_specific_mass_balance')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     acab_tavg -- accumulation, ablation rate (time average)
    pos = index(NCO%vars,' acab_tavg ')
    status = nf90_inq_varid(NCO%id,'acab_tavg',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+9) = '         '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable acab_tavg')
       status = nf90_def_var(NCO%id,'acab_tavg',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'accumulation, ablation rate (time average)')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_surface_specific_mass_balance')
       status = nf90_put_att(NCO%id, varid, 'cell_methods', 'time: mean over years')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     age -- ice age
    pos = index(NCO%vars,' age ')
    status = nf90_inq_varid(NCO%id,'age',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable age')
       status = nf90_def_var(NCO%id,'age',NF90_FLOAT,(/x1_dimid, y1_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice age')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_age')
       status = nf90_put_att(NCO%id, varid, 'units', 'year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     artm -- annual mean air temperature
    pos = index(NCO%vars,' artm ')
    status = nf90_inq_varid(NCO%id,'artm',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable artm')
       status = nf90_def_var(NCO%id,'artm',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'annual mean air temperature')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_temperature')
       status = nf90_put_att(NCO%id, varid, 'cell_methods', 'time: mean')
       status = nf90_put_att(NCO%id, varid, 'units', 'degree_Celsius')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     bheatflx -- basal heat flux
    pos = index(NCO%vars,' bheatflx ')
    status = nf90_inq_varid(NCO%id,'bheatflx',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+8) = '        '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable bheatflx')
       status = nf90_def_var(NCO%id,'bheatflx',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal heat flux')
       status = nf90_put_att(NCO%id, varid, 'units', 'watt/meter2')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     bmlt -- basal melt rate
    pos = index(NCO%vars,' bmlt ')
    status = nf90_inq_varid(NCO%id,'bmlt',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable bmlt')
       status = nf90_def_var(NCO%id,'bmlt',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal melt rate')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_melt_rate')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     bmlt_tavg -- basal melt rate (time average)
    pos = index(NCO%vars,' bmlt_tavg ')
    status = nf90_inq_varid(NCO%id,'bmlt_tavg',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+9) = '         '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable bmlt_tavg')
       status = nf90_def_var(NCO%id,'bmlt_tavg',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_melt_rate')
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal melt rate (time average)')
       status = nf90_put_att(NCO%id, varid, 'cell_methods', 'time: mean over years')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     btemp -- basal ice temperature
    pos = index(NCO%vars,' btemp ')
    status = nf90_inq_varid(NCO%id,'btemp',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable btemp')
       status = nf90_def_var(NCO%id,'btemp',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal ice temperature')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_temperature')
       status = nf90_put_att(NCO%id, varid, 'units', 'degree_Celsius')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     btrc -- basal slip coefficient
    pos = index(NCO%vars,' btrc ')
    status = nf90_inq_varid(NCO%id,'btrc',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable btrc')
       status = nf90_def_var(NCO%id,'btrc',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal slip coefficient')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/pascal/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     bwat -- basal water depth
    pos = index(NCO%vars,' bwat ')
    status = nf90_inq_varid(NCO%id,'bwat',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable bwat')
       status = nf90_def_var(NCO%id,'bwat',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal water depth')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     calving -- ice margin calving
    pos = index(NCO%vars,' calving ')
    status = nf90_inq_varid(NCO%id,'calving',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+7) = '       '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable calving')
       status = nf90_def_var(NCO%id,'calving',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice margin calving')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     diffu -- apparent diffusivity
    pos = index(NCO%vars,' diffu ')
    status = nf90_inq_varid(NCO%id,'diffu',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable diffu')
       status = nf90_def_var(NCO%id,'diffu',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'apparent diffusivity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter2/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     dusrfdtm -- rate of upper ice surface elevation change
    pos = index(NCO%vars,' dusrfdtm ')
    status = nf90_inq_varid(NCO%id,'dusrfdtm',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+8) = '        '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable dusrfdtm')
       status = nf90_def_var(NCO%id,'dusrfdtm',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'rate of upper ice surface elevation change')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     eus -- global average sea level
    pos = index(NCO%vars,' eus ')
    status = nf90_inq_varid(NCO%id,'eus',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable eus')
       status = nf90_def_var(NCO%id,'eus',NF90_FLOAT,(/time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'global average sea level')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'global_average_sea_level_change')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     flwa -- Pre-exponential flow law parameter
    pos = index(NCO%vars,' flwa ')
    status = nf90_inq_varid(NCO%id,'flwa',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable flwa')
       status = nf90_def_var(NCO%id,'flwa',NF90_FLOAT,(/x1_dimid, y1_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Pre-exponential flow law parameter')
       status = nf90_put_att(NCO%id, varid, 'units', 'pascal/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     iarea -- area covered by ice
    pos = index(NCO%vars,' iarea ')
    status = nf90_inq_varid(NCO%id,'iarea',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable iarea')
       status = nf90_def_var(NCO%id,'iarea',NF90_FLOAT,(/time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'area covered by ice')
       status = nf90_put_att(NCO%id, varid, 'units', 'km2')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     ivol -- ice volume
    pos = index(NCO%vars,' ivol ')
    status = nf90_inq_varid(NCO%id,'ivol',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable ivol')
       status = nf90_def_var(NCO%id,'ivol',NF90_FLOAT,(/time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice volume')
       status = nf90_put_att(NCO%id, varid, 'units', 'km3')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     lat -- Latitude
    pos = index(NCO%vars,' lat ')
    status = nf90_inq_varid(NCO%id,'lat',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable lat')
       status = nf90_def_var(NCO%id,'lat',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'latitude')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Latitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'degreeN')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     litho_temp -- lithosphere temperature
    pos = index(NCO%vars,' litho_temp ')
    status = nf90_inq_varid(NCO%id,'litho_temp',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+10) = '          '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable litho_temp')
       status = nf90_def_var(NCO%id,'litho_temp',NF90_FLOAT,(/x1_dimid, y1_dimid, lithoz_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'lithosphere temperature')
       status = nf90_put_att(NCO%id, varid, 'units', 'degree_Celsius')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     lon -- Longitude
    pos = index(NCO%vars,' lon ')
    status = nf90_inq_varid(NCO%id,'lon',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable lon')
       status = nf90_def_var(NCO%id,'lon',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Longitude')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'longitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'degreeE')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     lsurf -- ice lower surface elevation
    pos = index(NCO%vars,' lsurf ')
    status = nf90_inq_varid(NCO%id,'lsurf',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable lsurf')
       status = nf90_def_var(NCO%id,'lsurf',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice lower surface elevation')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     relx -- relaxed bedrock topography
    pos = index(NCO%vars,' relx ')
    status = nf90_inq_varid(NCO%id,'relx',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable relx')
       status = nf90_def_var(NCO%id,'relx',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'relaxed bedrock topography')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     slc -- isostatic adjustment
    pos = index(NCO%vars,' slc ')
    status = nf90_inq_varid(NCO%id,'slc',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable slc')
       status = nf90_def_var(NCO%id,'slc',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'isostatic adjustment')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'bedrock_altitude_change_due_to_isostatic_adjustment')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     soft -- bed softness parameter
    pos = index(NCO%vars,' soft ')
    status = nf90_inq_varid(NCO%id,'soft',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable soft')
       status = nf90_def_var(NCO%id,'soft',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'bed softness parameter')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/pascal/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     taux -- basal shear stress in x direction
    pos = index(NCO%vars,' taux ')
    status = nf90_inq_varid(NCO%id,'taux',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable taux')
       status = nf90_def_var(NCO%id,'taux',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal shear stress in x direction')
       status = nf90_put_att(NCO%id, varid, 'units', 'kilopascal')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     tauy -- basal shear stress in y direction
    pos = index(NCO%vars,' tauy ')
    status = nf90_inq_varid(NCO%id,'tauy',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable tauy')
       status = nf90_def_var(NCO%id,'tauy',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal shear stress in y direction')
       status = nf90_put_att(NCO%id, varid, 'units', 'kilopascal')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     temp -- ice temperature
    pos = index(NCO%vars,' temp ')
    status = nf90_inq_varid(NCO%id,'temp',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable temp')
       status = nf90_def_var(NCO%id,'temp',NF90_FLOAT,(/x1_dimid, y1_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice temperature')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_temperature')
       status = nf90_put_att(NCO%id, varid, 'units', 'degree_Celsius')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     thk -- ice thickness
    pos = index(NCO%vars,' thk ')
    status = nf90_inq_varid(NCO%id,'thk',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+3) = '   '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable thk')
       status = nf90_def_var(NCO%id,'thk',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice thickness')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_thickness')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     thkmask -- mask
    pos = index(NCO%vars,' thkmask ')
    status = nf90_inq_varid(NCO%id,'thkmask',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+7) = '       '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable thkmask')
       status = nf90_def_var(NCO%id,'thkmask',NF90_INT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'mask')
       status = nf90_put_att(NCO%id, varid, 'units', '1')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     topg -- bedrock topography
    pos = index(NCO%vars,' topg ')
    status = nf90_inq_varid(NCO%id,'topg',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable topg')
       status = nf90_def_var(NCO%id,'topg',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'bedrock topography')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'bedrock_altitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     ubas -- basal slip velocity in x direction
    pos = index(NCO%vars,' ubas ')
    status = nf90_inq_varid(NCO%id,'ubas',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable ubas')
       status = nf90_def_var(NCO%id,'ubas',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal slip velocity in x direction')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_x_velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     ubas_tavg -- basal slip velocity in x direction (time average)
    pos = index(NCO%vars,' ubas_tavg ')
    status = nf90_inq_varid(NCO%id,'ubas_tavg',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+9) = '         '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable ubas_tavg')
       status = nf90_def_var(NCO%id,'ubas_tavg',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal slip velocity in x direction (time average)')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_x_velocity')
       status = nf90_put_att(NCO%id, varid, 'cell_methods', 'time: mean over years')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     uflx -- flux in x direction
    pos = index(NCO%vars,' uflx ')
    status = nf90_inq_varid(NCO%id,'uflx',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable uflx')
       status = nf90_def_var(NCO%id,'uflx',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'flux in x direction')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter2/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     usurf -- ice upper surface elevation
    pos = index(NCO%vars,' usurf ')
    status = nf90_inq_varid(NCO%id,'usurf',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+5) = '     '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable usurf')
       status = nf90_def_var(NCO%id,'usurf',NF90_FLOAT,(/x1_dimid, y1_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice upper surface elevation')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'surface_altitude')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     uvel -- ice velocity in x direction
    pos = index(NCO%vars,' uvel ')
    status = nf90_inq_varid(NCO%id,'uvel',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable uvel')
       status = nf90_def_var(NCO%id,'uvel',NF90_FLOAT,(/x0_dimid, y0_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice velocity in x direction')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_x_velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     vbas -- basal slip velocity in y direction
    pos = index(NCO%vars,' vbas ')
    status = nf90_inq_varid(NCO%id,'vbas',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable vbas')
       status = nf90_def_var(NCO%id,'vbas',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal slip velocity in y direction')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_y_velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     vbas_tavg -- basal slip velocity in y direction (time average)
    pos = index(NCO%vars,' vbas_tavg ')
    status = nf90_inq_varid(NCO%id,'vbas_tavg',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+9) = '         '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable vbas_tavg')
       status = nf90_def_var(NCO%id,'vbas_tavg',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'basal slip velocity in y direction (time average)')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_basal_y_velocity')
       status = nf90_put_att(NCO%id, varid, 'cell_methods', 'time: mean over years')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     vflx -- flux in x direction
    pos = index(NCO%vars,' vflx ')
    status = nf90_inq_varid(NCO%id,'vflx',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable vflx')
       status = nf90_def_var(NCO%id,'vflx',NF90_FLOAT,(/x0_dimid, y0_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'flux in x direction')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter2/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     vvel -- ice velocity in y direction
    pos = index(NCO%vars,' vvel ')
    status = nf90_inq_varid(NCO%id,'vvel',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable vvel')
       status = nf90_def_var(NCO%id,'vvel',NF90_FLOAT,(/x0_dimid, y0_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'long_name', 'ice velocity in y direction')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_y_velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     wgrd -- Vertical grid velocity
    pos = index(NCO%vars,' wgrd ')
    status = nf90_inq_varid(NCO%id,'wgrd',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable wgrd')
       status = nf90_def_var(NCO%id,'wgrd',NF90_FLOAT,(/x1_dimid, y1_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'Vertical grid velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

    !     wvel -- vertical ice velocity
    pos = index(NCO%vars,' wvel ')
    status = nf90_inq_varid(NCO%id,'wvel',varid)
    if (pos.ne.0) then
      NCO%vars(pos+1:pos+4) = '    '
    end if
    if (pos.ne.0 .and. status.eq.nf90_enotvar) then
       call write_log('Creating variable wvel')
       status = nf90_def_var(NCO%id,'wvel',NF90_FLOAT,(/x1_dimid, y1_dimid, level_dimid, time_dimid/),varid)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(NCO%id, varid, 'coordinates', 'lon lat')
       status = nf90_put_att(NCO%id, varid, 'long_name', 'vertical ice velocity')
       status = nf90_put_att(NCO%id, varid, 'standard_name', 'land_ice_z_velocity')
       status = nf90_put_att(NCO%id, varid, 'units', 'meter/year')
       if (glimmap_allocated(model%projection)) then
          status = nf90_put_att(NCO%id, varid, 'grid_mapping',glimmer_nc_mapvarname)
       end if
     end if

  end subroutine glide_io_create

  subroutine glide_io_write(outfile,data)
    use glide_types
    use glimmer_ncdf
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: data
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
    status = nf90_inq_varid(NCO%id,'acab',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f1)*(data%climate%acab), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'acab_tavg',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            ((scale2d_f1)*tavgf)*(data%climate%acab_tavg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'age',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (tim0/scyr)*(data%geometry%age(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'artm',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%climate%artm, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'bheatflx',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%temper%bheatflx, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'bmlt',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f1)*(data%temper%bmlt), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'bmlt_tavg',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            ((scale2d_f1)*tavgf)*(data%temper%bmlt_tavg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'btemp',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%temper%temp(data%general%upn,1:data%general%ewn,1:data%general%nsn), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'btrc',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f6)*(data%velocity%btrc), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'bwat',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%temper%bwat), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'calving',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%climate%calving), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'diffu',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f4)*(data%velocity%diffu), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'dusrfdtm',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f1)*(data%geomderv%dusrfdtm), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'eus',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%climate%eus), (/outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'flwa',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (scale3d_f8)*(data%temper%flwa(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'iarea',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (len0*len0*1.e-6)*(data%geometry%iarea), (/outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'ivol',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0*len0*len0*1.e-9)*(data%geometry%ivol), (/outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'lat',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%climate%lati, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'litho_temp',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%lithot%temp, (/1,1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'lon',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%climate%loni, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'lsurf',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%geometry%lsrf), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'relx',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%isos%relx), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'slc',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%isos%relx-data%geometry%topg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'soft',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f6)*(data%velocity%bed_softness), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'taux',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (1e-3*thk0*thk0/len0)*(data%velocity%tau_x), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'tauy',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (1e-3*thk0*thk0/len0)*(data%velocity%tau_y), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'temp',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               data%temper%temp(up,1:data%general%ewn,1:data%general%nsn), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'thk',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%geometry%thck), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'thkmask',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            data%geometry%thkmask, (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'topg',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%geometry%topg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'ubas',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f5)*(data%velocity%ubas), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'ubas_tavg',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            ((scale2d_f5)*tavgf)*(data%velocity%ubas_tavg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'uflx',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f2)*(data%velocity%uflx), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'usurf',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (thk0)*(data%geometry%usrf), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'uvel',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (scale3d_f1)*(data%velocity%uvel(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'vbas',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f5)*(data%velocity%vbas), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'vbas_tavg',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            ((scale2d_f5)*tavgf)*(data%velocity%vbas_tavg), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'vflx',varid)
    if (status .eq. nf90_noerr) then
       status = nf90_put_var(NCO%id, varid, &
            (scale2d_f2)*(data%velocity%vflx), (/1,1,outfile%timecounter/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCO%id,'vvel',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (scale3d_f1)*(data%velocity%vvel(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'wgrd',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (scale3d_f7)*(data%velocity%wgrd(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCO%id,'wvel',varid)
    if (status .eq. nf90_noerr) then
       do up=1,NCO%nlevel
          status = nf90_put_var(NCO%id, varid, &
               (scale3d_f7)*(data%velocity%wvel(up,:,:)), (/1,1,up,outfile%timecounter/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

  end subroutine glide_io_write

  !*****************************************************************************
  ! netCDF input
  !*****************************************************************************  
  subroutine glide_io_readall(data,model)
    !*FD read from netCDF file
    use glide_types
    use glide_types
    use glimmer_ncio
    use glimmer_ncdf
    implicit none
    type(glide_global_type) :: data
    type(glide_global_type) :: model

    ! local variables
    type(glimmer_nc_input), pointer :: ic    

    ic=>model%funits%in_first
    do while(associated(ic))
       call glimmer_nc_checkread(ic,model)
       if (ic%nc%just_processed) then
          call glide_io_read(ic,data)
       end if
       ic=>ic%next
    end do
  end subroutine glide_io_readall

  subroutine glide_io_read(infile,data,scale_vars)
    !*FD read variables from a netCDF file
    use glimmer_log
    use glimmer_ncdf
    use glide_types
    use glimmer_paramets
    use glimmer_scales
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: data
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
    status = nf90_inq_varid(NCI%id,'age',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading age')
       do up=1,NCI%nlevel
          status = nf90_get_var(NCI%id, varid, &
               data%geometry%age(up,:,:), (/1,1,up,infile%current_time/))
          call nc_errorhandle(__FILE__,__LINE__,status)
          if (scale) then
          data%geometry%age(up,:,:) = data%geometry%age(up,:,:)/(tim0/scyr)
          end if
       end do
    end if

    status = nf90_inq_varid(NCI%id,'bheatflx',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading bheatflx')
       status = nf90_get_var(NCI%id, varid, &
            data%temper%bheatflx, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'bmlt',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading bmlt')
       status = nf90_get_var(NCI%id, varid, &
            data%temper%bmlt, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%temper%bmlt = data%temper%bmlt/(scale2d_f1)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'bwat',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading bwat')
       status = nf90_get_var(NCI%id, varid, &
            data%temper%bwat, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%temper%bwat = data%temper%bwat/(thk0)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'flwa',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading flwa')
       do up=1,NCI%nlevel
          status = nf90_get_var(NCI%id, varid, &
               data%temper%flwa(up,:,:), (/1,1,up,infile%current_time/))
          call nc_errorhandle(__FILE__,__LINE__,status)
          if (scale) then
          data%temper%flwa(up,:,:) = data%temper%flwa(up,:,:)/(scale3d_f8)
          end if
       end do
    end if

    status = nf90_inq_varid(NCI%id,'lat',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading lat')
       status = nf90_get_var(NCI%id, varid, &
            data%climate%lati, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'litho_temp',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading litho_temp')
       status = nf90_get_var(NCI%id, varid, &
            data%lithot%temp, (/1,1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'lon',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading lon')
       status = nf90_get_var(NCI%id, varid, &
            data%climate%loni, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    status = nf90_inq_varid(NCI%id,'relx',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading relx')
       status = nf90_get_var(NCI%id, varid, &
            data%isos%relx, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%isos%relx = data%isos%relx/(thk0)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'soft',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading soft')
       status = nf90_get_var(NCI%id, varid, &
            data%velocity%bed_softness, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%velocity%bed_softness = data%velocity%bed_softness/(scale2d_f6)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'temp',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading temp')
       do up=1,NCI%nlevel
          status = nf90_get_var(NCI%id, varid, &
               data%temper%temp(up,1:data%general%ewn,1:data%general%nsn), (/1,1,up,infile%current_time/))
          call nc_errorhandle(__FILE__,__LINE__,status)
       end do
    end if

    status = nf90_inq_varid(NCI%id,'thk',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading thk')
       status = nf90_get_var(NCI%id, varid, &
            data%geometry%thck, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%geometry%thck = data%geometry%thck/(thk0)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'topg',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading topg')
       status = nf90_get_var(NCI%id, varid, &
            data%geometry%topg, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%geometry%topg = data%geometry%topg/(thk0)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'ubas',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading ubas')
       status = nf90_get_var(NCI%id, varid, &
            data%velocity%ubas, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%velocity%ubas = data%velocity%ubas/(scale2d_f5)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'usurf',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading usurf')
       status = nf90_get_var(NCI%id, varid, &
            data%geometry%usrf, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%geometry%usrf = data%geometry%usrf/(thk0)
       end if
    end if

    status = nf90_inq_varid(NCI%id,'vbas',varid)
    if (status .eq. nf90_noerr) then
       call write_log('  Loading vbas')
       status = nf90_get_var(NCI%id, varid, &
            data%velocity%vbas, (/1,1,infile%current_time/))
       call nc_errorhandle(__FILE__,__LINE__,status)
       if (scale) then
       data%velocity%vbas = data%velocity%vbas/(scale2d_f5)
       end if
    end if

  end subroutine glide_io_read

  subroutine glide_io_checkdim(infile,model,data)
    !*FD check if dimension sizes in file match dims of model
    use glimmer_log
    use glimmer_ncdf
    use glide_types
    use glide_types
    implicit none
    type(glimmer_nc_input), pointer :: infile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glide_global_type), optional :: data

    integer status,dimid,dimsize
    character(len=150) message

    ! check dimensions
    status = nf90_inq_dimid(NCI%id,'level',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%general%upn) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size level does not match: ', &
               model%general%upn
          call write_log(message,GM_FATAL)
       end if
    end if
    status = nf90_inq_dimid(NCI%id,'lithoz',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%lithot%nlayer) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size lithoz does not match: ', &
               model%lithot%nlayer
          call write_log(message,GM_FATAL)
       end if
    end if
    status = nf90_inq_dimid(NCI%id,'x0',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%general%ewn-1) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size x0 does not match: ', &
               model%general%ewn-1
          call write_log(message,GM_FATAL)
       end if
    end if
    status = nf90_inq_dimid(NCI%id,'x1',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%general%ewn) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size x1 does not match: ', &
               model%general%ewn
          call write_log(message,GM_FATAL)
       end if
    end if
    status = nf90_inq_dimid(NCI%id,'y0',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%general%nsn-1) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size y0 does not match: ', &
               model%general%nsn-1
          call write_log(message,GM_FATAL)
       end if
    end if
    status = nf90_inq_dimid(NCI%id,'y1',dimid)
    if (dimid.gt.0) then
       status = nf90_inquire_dimension(NCI%id,dimid,len=dimsize)
       if (dimsize.ne.model%general%nsn) then
          write(message,*) 'Error, reading file ',trim(NCI%filename),' size y1 does not match: ', &
               model%general%nsn
          call write_log(message,GM_FATAL)
       end if
    end if
  end subroutine glide_io_checkdim

  !*****************************************************************************
  ! calculating time averages
  !*****************************************************************************  
#ifdef HAVE_AVG
  subroutine glide_avg_accumulate(outfile,data,model)
    use glide_types
    use glide_types
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: model
    type(glide_global_type) :: data

    ! local variables
    real :: factor
    integer status, varid

    ! increase total time
    outfile%total_time = outfile%total_time + model%numerics%tinc
    factor = model%numerics%tinc

    ! accumulate acab
    status = nf90_inq_varid(NCO%id,'acab_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%climate%acab_tavg = data%climate%acab_tavg + factor * data%climate%acab
    end if

    ! accumulate bmlt
    status = nf90_inq_varid(NCO%id,'bmlt_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%temper%bmlt_tavg = data%temper%bmlt_tavg + factor * data%temper%bmlt
    end if

    ! accumulate ubas
    status = nf90_inq_varid(NCO%id,'ubas_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%velocity%ubas_tavg = data%velocity%ubas_tavg + factor * data%velocity%ubas
    end if

    ! accumulate vbas
    status = nf90_inq_varid(NCO%id,'vbas_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%velocity%vbas_tavg = data%velocity%vbas_tavg + factor * data%velocity%vbas
    end if

  end subroutine glide_avg_accumulate

  subroutine glide_avg_reset(outfile,data)
    use glide_types
    use glimmer_ncdf
    implicit none
    type(glimmer_nc_output), pointer :: outfile
    !*FD structure containg output netCDF descriptor
    type(glide_global_type) :: data

    ! local variables
    integer status, varid

    ! reset total time
    outfile%total_time = 0.

    ! reset acab
    status = nf90_inq_varid(NCO%id,'acab_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%climate%acab_tavg = 0.
    end if

    ! reset bmlt
    status = nf90_inq_varid(NCO%id,'bmlt_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%temper%bmlt_tavg = 0.
    end if

    ! reset ubas
    status = nf90_inq_varid(NCO%id,'ubas_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%velocity%ubas_tavg = 0.
    end if

    ! reset vbas
    status = nf90_inq_varid(NCO%id,'vbas_tavg',varid)
    if (status .eq. nf90_noerr) then
       data%velocity%vbas_tavg = 0.
    end if

  end subroutine glide_avg_reset
#endif

  !*********************************************************************
  ! lots of accessor subroutines follow
  !*********************************************************************
  subroutine glide_get_acab(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f1)*(data%climate%acab)
  end subroutine glide_get_acab

  subroutine glide_set_acab(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%climate%acab = inarray/(scale2d_f1)
  end subroutine glide_set_acab

  subroutine glide_get_artm(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%climate%artm
  end subroutine glide_get_artm

  subroutine glide_set_artm(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%climate%artm = inarray
  end subroutine glide_set_artm

  subroutine glide_get_bheatflx(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%temper%bheatflx
  end subroutine glide_get_bheatflx

  subroutine glide_set_bheatflx(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%temper%bheatflx = inarray
  end subroutine glide_set_bheatflx

  subroutine glide_get_bmlt(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f1)*(data%temper%bmlt)
  end subroutine glide_get_bmlt

  subroutine glide_set_bmlt(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%temper%bmlt = inarray/(scale2d_f1)
  end subroutine glide_set_bmlt

  subroutine glide_get_btemp(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%temper%temp(data%general%upn,1:data%general%ewn,1:data%general%nsn)
  end subroutine glide_get_btemp

  subroutine glide_get_btrc(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f6)*(data%velocity%btrc)
  end subroutine glide_get_btrc

  subroutine glide_set_btrc(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%btrc = inarray/(scale2d_f6)
  end subroutine glide_set_btrc

  subroutine glide_get_bwat(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%temper%bwat)
  end subroutine glide_get_bwat

  subroutine glide_set_bwat(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%temper%bwat = inarray/(thk0)
  end subroutine glide_set_bwat

  subroutine glide_get_calving(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%climate%calving)
  end subroutine glide_get_calving

  subroutine glide_set_calving(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%climate%calving = inarray/(thk0)
  end subroutine glide_set_calving

  subroutine glide_get_diffu(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f4)*(data%velocity%diffu)
  end subroutine glide_get_diffu

  subroutine glide_set_diffu(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%diffu = inarray/(scale2d_f4)
  end subroutine glide_set_diffu

  subroutine glide_get_dusrfdtm(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f1)*(data%geomderv%dusrfdtm)
  end subroutine glide_get_dusrfdtm

  subroutine glide_set_dusrfdtm(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%geomderv%dusrfdtm = inarray/(scale2d_f1)
  end subroutine glide_set_dusrfdtm

  subroutine glide_get_eus(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(out) :: outarray

    outarray = (thk0)*(data%climate%eus)
  end subroutine glide_get_eus

  subroutine glide_set_eus(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(in) :: inarray

    data%climate%eus = inarray/(thk0)
  end subroutine glide_set_eus

  subroutine glide_get_iarea(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(out) :: outarray

    outarray = (len0*len0*1.e-6)*(data%geometry%iarea)
  end subroutine glide_get_iarea

  subroutine glide_set_iarea(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(in) :: inarray

    data%geometry%iarea = inarray/(len0*len0*1.e-6)
  end subroutine glide_set_iarea

  subroutine glide_get_ivol(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(out) :: outarray

    outarray = (thk0*len0*len0*1.e-9)*(data%geometry%ivol)
  end subroutine glide_get_ivol

  subroutine glide_set_ivol(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, intent(in) :: inarray

    data%geometry%ivol = inarray/(thk0*len0*len0*1.e-9)
  end subroutine glide_set_ivol

  subroutine glide_get_lat(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%climate%lati
  end subroutine glide_get_lat

  subroutine glide_set_lat(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%climate%lati = inarray
  end subroutine glide_set_lat

  subroutine glide_get_lon(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = data%climate%loni
  end subroutine glide_get_lon

  subroutine glide_set_lon(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%climate%loni = inarray
  end subroutine glide_set_lon

  subroutine glide_get_lsurf(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%geometry%lsrf)
  end subroutine glide_get_lsurf

  subroutine glide_set_lsurf(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%geometry%lsrf = inarray/(thk0)
  end subroutine glide_set_lsurf

  subroutine glide_get_relx(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%isos%relx)
  end subroutine glide_get_relx

  subroutine glide_set_relx(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%isos%relx = inarray/(thk0)
  end subroutine glide_set_relx

  subroutine glide_get_slc(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%isos%relx-data%geometry%topg)
  end subroutine glide_get_slc

  subroutine glide_get_soft(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f6)*(data%velocity%bed_softness)
  end subroutine glide_get_soft

  subroutine glide_set_soft(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%bed_softness = inarray/(scale2d_f6)
  end subroutine glide_set_soft

  subroutine glide_get_taux(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (1e-3*thk0*thk0/len0)*(data%velocity%tau_x)
  end subroutine glide_get_taux

  subroutine glide_set_taux(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%tau_x = inarray/(1e-3*thk0*thk0/len0)
  end subroutine glide_set_taux

  subroutine glide_get_tauy(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (1e-3*thk0*thk0/len0)*(data%velocity%tau_y)
  end subroutine glide_get_tauy

  subroutine glide_set_tauy(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%tau_y = inarray/(1e-3*thk0*thk0/len0)
  end subroutine glide_set_tauy

  subroutine glide_get_thk(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%geometry%thck)
  end subroutine glide_get_thk

  subroutine glide_set_thk(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%geometry%thck = inarray/(thk0)
  end subroutine glide_set_thk

  subroutine glide_get_thkmask(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    integer, dimension(:,:), intent(out) :: outarray

    outarray = data%geometry%thkmask
  end subroutine glide_get_thkmask

  subroutine glide_set_thkmask(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    integer, dimension(:,:), intent(in) :: inarray

    data%geometry%thkmask = inarray
  end subroutine glide_set_thkmask

  subroutine glide_get_topg(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%geometry%topg)
  end subroutine glide_get_topg

  subroutine glide_set_topg(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%geometry%topg = inarray/(thk0)
  end subroutine glide_set_topg

  subroutine glide_get_ubas(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f5)*(data%velocity%ubas)
  end subroutine glide_get_ubas

  subroutine glide_set_ubas(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%ubas = inarray/(scale2d_f5)
  end subroutine glide_set_ubas

  subroutine glide_get_uflx(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f2)*(data%velocity%uflx)
  end subroutine glide_get_uflx

  subroutine glide_set_uflx(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%uflx = inarray/(scale2d_f2)
  end subroutine glide_set_uflx

  subroutine glide_get_usurf(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (thk0)*(data%geometry%usrf)
  end subroutine glide_get_usurf

  subroutine glide_set_usurf(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%geometry%usrf = inarray/(thk0)
  end subroutine glide_set_usurf

  subroutine glide_get_vbas(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f5)*(data%velocity%vbas)
  end subroutine glide_get_vbas

  subroutine glide_set_vbas(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%vbas = inarray/(scale2d_f5)
  end subroutine glide_set_vbas

  subroutine glide_get_vflx(data,outarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(out) :: outarray

    outarray = (scale2d_f2)*(data%velocity%vflx)
  end subroutine glide_get_vflx

  subroutine glide_set_vflx(data,inarray)
    use glimmer_scales
    use glimmer_paramets
    use glide_types
    implicit none
    type(glide_global_type) :: data
    real, dimension(:,:), intent(in) :: inarray

    data%velocity%vflx = inarray/(scale2d_f2)
  end subroutine glide_set_vflx


end module glide_io
