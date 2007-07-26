! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                               +
! +  glimmer_restart_pointarr.f90 - part of the GLIMMER ice model + 
! +                                                               +
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#ifdef HAVE_CONFIG_H
#include <config.inc>
#endif

module glimmer_restart_pointarr

  use glimmer_restart_common

  implicit none

  interface write_pointarr
     module procedure write_pointarr_int_1d, write_pointarr_realsp_1d, write_pointarr_realdp_1d, write_pointarr_logical_1d
     module procedure write_pointarr_int_2d, write_pointarr_realsp_2d, write_pointarr_realdp_2d, write_pointarr_logical_2d
     module procedure write_pointarr_int_3d, write_pointarr_realsp_3d, write_pointarr_realdp_3d, write_pointarr_logical_3d
  end interface

  interface read_pointarr
     module procedure read_pointarr_int_1d, read_pointarr_realsp_1d, read_pointarr_realdp_1d, read_pointarr_logical_1d
     module procedure read_pointarr_int_2d, read_pointarr_realsp_2d, read_pointarr_realdp_2d, read_pointarr_logical_2d
     module procedure read_pointarr_int_3d, read_pointarr_realsp_3d, read_pointarr_realdp_3d, read_pointarr_logical_3d
  end interface

contains

  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! WRITE code - 1D
  !------------------------------------------------------------------

  subroutine write_pointarr_int_1d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:),pointer     :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(1) :: shp
    integer,dimension(1) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_int_1d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_realsp_1d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:),pointer    :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(1) :: shp
    integer,dimension(1) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_FLOAT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realsp_1d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_realdp_1d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:),pointer    :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(1) :: shp
    integer,dimension(1) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_DOUBLE,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realdp_1d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_logical_1d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:),pointer    :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer :: nx
    integer,allocatable,dimension(:) :: tmp
    integer,dimension(1) :: shp
    integer,dimension(1) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       allocate(tmp(size(values)))
       where (values)
          tmp=1
       elsewhere
          tmp=0
       end where
       status=nf90_put_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_logical_1d

  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! WRITE code - 2D
  !------------------------------------------------------------------
    
  subroutine write_pointarr_int_2d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:,:),pointer   :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(2) :: shp
    integer,dimension(2) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_int_2d

  !------------------------------------------------------------------
  
  subroutine write_pointarr_realsp_2d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:,:),pointer  :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(2) :: shp
    integer,dimension(2) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_FLOAT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realsp_2d

  !------------------------------------------------------------------
 
  subroutine write_pointarr_realdp_2d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:,:),pointer  :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(2) :: shp
    integer,dimension(2) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_DOUBLE,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realdp_2d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_logical_2d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:,:),pointer    :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,allocatable,dimension(:,:) :: tmp
    integer,dimension(2) :: shp
    integer,dimension(2) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       allocate(tmp(size(values,1),size(values,2)))
       where (values)
          tmp=1
       elsewhere
          tmp=0
       end where
       status=nf90_put_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_logical_2d
    
  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! WRITE code - 3D
  !------------------------------------------------------------------

  subroutine write_pointarr_int_3d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:,:,:),pointer :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(3) :: shp
    integer,dimension(3) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_int_3d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_realsp_3d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:,:,:),pointer :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(3) :: shp
    integer,dimension(3) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_FLOAT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realsp_3d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_realdp_3d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:,:,:),pointer :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,dimension(3) :: shp
    integer,dimension(3) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_DOUBLE,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       status=nf90_put_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_realdp_3d

  !------------------------------------------------------------------
    
  subroutine write_pointarr_logical_3d(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:,:,:),pointer    :: values

    integer :: status,varid
    character(varnamelen) :: varname
    integer,allocatable,dimension(:,:,:) :: tmp
    integer,dimension(3) :: shp
    integer,dimension(3) :: lbounds

    if (associated(values)) then
       shp=shape(values)
       lbounds=lbound(values)
    end if

    call write_array_common(file,prefix,name,NF90_INT,associated(values),shp,lbounds,varid)

    if (associated(values)) then
       allocate(tmp(size(values,1),size(values,2),size(values,3)))
       where (values)
          tmp=1
       elsewhere
          tmp=0
       end where
       status=nf90_put_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

  end subroutine write_pointarr_logical_3d

  !------------------------------------------------------------------

  subroutine write_array_common(file,prefix,name,typecode,assoc,lens,lbs,varid)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(in)    :: typecode
    logical,           intent(in)    :: assoc
    integer,dimension(:),intent(in)  :: lens
    integer,dimension(:),intent(in)  :: lbs
    integer,           intent(out)   :: varid

    character(varnamelen) :: varname
    integer :: status
    integer,dimension(size(lens)) :: dimids
    integer :: ndims,i

    ndims=size(lens)

    call new_varname(varname,prefix,file%count)

    if (assoc) then
       do i=1,ndims
          call new_dimension(file,com_dims(i),lens(i),dimids(i))
       end do
       call set_define(file)
       status=nf90_def_var(file%ncid,varname,typecode,dimids,varid)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
       call write_lbounds(file%ncid,varid,lbs)
    else
       call set_define(file)
       status=nf90_def_var(file%ncid,varname,typecode,varid=varid)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
       status=nf90_put_att(file%ncid,varid,name='NULL',values='NULL')
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    endif

    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    call end_define(file)
    file%count=file%count+1

  end subroutine write_array_common

  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! READ code - 1D
  !------------------------------------------------------------------

  subroutine read_pointarr_int_1d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:),pointer     :: values

    integer :: varid,status
    integer,dimension(1) :: dimlens
    integer,dimension(1) :: lbounds
    integer,dimension(1) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_int_1d

  !------------------------------------------------------------------

  subroutine read_pointarr_realsp_1d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:),pointer    :: values

    integer :: varid,status
    integer,dimension(1) :: dimlens
    integer,dimension(1) :: lbounds
    integer,dimension(1) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realsp_1d

  !------------------------------------------------------------------

  subroutine read_pointarr_realdp_1d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:),pointer    :: values

    integer :: varid,status
    integer,dimension(1) :: dimlens
    integer,dimension(1) :: lbounds
    integer,dimension(1) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realdp_1d
 
  !------------------------------------------------------------------

  subroutine read_pointarr_logical_1d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:),pointer     :: values

    integer :: varid,status
    integer,dimension(1) :: dimlens
    integer,dimension(1) :: lbounds
    integer,dimension(1) :: ubounds
    logical :: nullt
    integer,dimension(:),allocatable :: tmp

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1)))
       allocate(tmp(lbounds(1):ubounds(1)))
        status=nf90_get_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
       where (tmp==1)
          values=.true.
       elsewhere
          values=.false.
       end where
    end if

  end subroutine read_pointarr_logical_1d
 
  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! READ code - 2D
  !------------------------------------------------------------------

  subroutine read_pointarr_int_2d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:,:),pointer  :: values

    integer :: varid,status
    integer,dimension(2) :: dimlens
    integer,dimension(2) :: lbounds
    integer,dimension(2) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_int_2d

  !------------------------------------------------------------------

  subroutine read_pointarr_realsp_2d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:,:),pointer  :: values

    integer :: varid,status
    integer,dimension(2) :: dimlens
    integer,dimension(2) :: lbounds
    integer,dimension(2) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realsp_2d

  !------------------------------------------------------------------

  subroutine read_pointarr_realdp_2d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:,:),pointer  :: values

    integer :: varid,status
    integer,dimension(2) :: dimlens
    integer,dimension(2) :: lbounds
    integer,dimension(2) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realdp_2d
 
  !------------------------------------------------------------------

  subroutine read_pointarr_logical_2d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:,:),pointer  :: values

    integer :: varid,status
    integer,dimension(2) :: dimlens
    integer,dimension(2) :: lbounds
    integer,dimension(2) :: ubounds
    logical :: nullt
    integer,dimension(:,:),allocatable :: tmp

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
       allocate(tmp(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
        status=nf90_get_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
       where (tmp==1)
          values=.true.
       elsewhere
          values=.false.
       end where
    end if

  end subroutine read_pointarr_logical_2d
    
  !------------------------------------------------------------------
  ! POINTER ARRAYS
  !------------------------------------------------------------------
  ! READ code - 3D
  !------------------------------------------------------------------

  subroutine read_pointarr_int_3d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,dimension(:,:,:),pointer :: values

    integer :: varid,status
    integer,dimension(3) :: dimlens
    integer,dimension(3) :: lbounds
    integer,dimension(3) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2),lbounds(3):ubounds(3)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_int_3d

  !------------------------------------------------------------------

  subroutine read_pointarr_realsp_3d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),dimension(:,:,:),pointer :: values

    integer :: varid,status
    integer,dimension(3) :: dimlens
    integer,dimension(3) :: lbounds
    integer,dimension(3) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2),lbounds(3):ubounds(3)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realsp_3d

  !------------------------------------------------------------------

  subroutine read_pointarr_realdp_3d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),dimension(:,:,:),pointer :: values

    integer :: varid,status
    integer,dimension(3) :: dimlens
    integer,dimension(3) :: lbounds
    integer,dimension(3) :: ubounds
    logical :: nullt

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2),lbounds(3):ubounds(3)))
       status=nf90_get_var(file%ncid,varid,values)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    end if

  end subroutine read_pointarr_realdp_3d
 
  !------------------------------------------------------------------

  subroutine read_pointarr_logical_3d(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,dimension(:,:,:),pointer :: values

    integer :: varid,status
    integer,dimension(3) :: dimlens
    integer,dimension(3) :: lbounds
    integer,dimension(3) :: ubounds
    logical :: nullt
    integer,dimension(:,:,:),allocatable :: tmp

    if(associated(values)) then
       deallocate(values)
       values => null()
    end if

    call read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    if (.not.nullt) then
       ubounds=lbounds+dimlens-1
       allocate(values(lbounds(1):ubounds(1),lbounds(2):ubounds(2),lbounds(3):ubounds(3)))
       allocate(tmp(lbounds(1):ubounds(1),lbounds(2):ubounds(2),lbounds(3):ubounds(3)))
        status=nf90_get_var(file%ncid,varid,tmp)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
       where (tmp==1)
          values=.true.
       elsewhere
          values=.false.
       end where
    end if

  end subroutine read_pointarr_logical_3d

  !------------------------------------------------------------------

  subroutine read_array_common(file,prefix,name,varid,nullt,dimlens,lbounds)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(out)   :: varid
    logical,           intent(out)   :: nullt
    integer,dimension(:),intent(out) :: dimlens
    integer,dimension(:),intent(out) :: lbounds

    character(varnamelen) :: varname,nametest,nulltest,dimname
    integer :: status,namelen,i
    integer,dimension(size(dimlens)) :: dimids

    call new_varname(varname,prefix,file%count)

    status=nf90_inq_varid(file%ncid,varname,varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    status=nf90_inquire_attribute(file%ncid,varid,'varname',len=namelen)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_get_att(file%ncid,varid,'varname',nametest)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    if (namelen>varnamelen) then
       call write_log('Variable name too long',GM_FATAL,__FILE__,__LINE__)
    end if
    nametest(namelen+1:)=repeat(' ',varnamelen-namelen)

    if (name/=nametest) then
       call write_log('Restart read mismatch: '//trim(varname)//', ' &
            //trim(name)//', '//trim(nametest),GM_FATAL)
    end if

    status=nf90_get_att(file%ncid,varid,'NULL',nulltest)
    select case(status)
    case(NF90_NOERR)
       nullt=.true.
    case(NF90_ENOTATT)
       nullt=.false.
       status=nf90_inquire_variable(file%ncid,varid,dimids=dimids)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
       do i = 1,size(dimlens)
          status=nf90_inquire_dimension(file%ncid,dimids(i),name=dimname,len=dimlens(i))
          if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
          if (dimname(1:1)/=com_dims(i)) then
             print*,'DIMENSION name error:',dimname,com_dims(i),i
          end if
       end do
       call read_lbounds(file%ncid,varid,lbounds)
    case default
       call ncdf_err(status,__LINE__,prefix,name,varname)
    end select

    file%count=file%count+1

  end subroutine read_array_common

end module glimmer_restart_pointarr
