! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                               +
! +  glimmer_restart_statscal.f90 - part of the GLIMMER ice model + 
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

module glimmer_restart_statscal

  use glimmer_restart_common

  implicit none

  interface write_statscalvar
     module procedure write_statscalvar_int,  write_statscalvar_realsp, write_statscalvar_realdp
     module procedure write_statscalvar_char, write_statscalvar_logical
  end interface

  interface read_statscalvar
     module procedure read_statscalvar_int,  read_statscalvar_realsp, read_statscalvar_realdp
     module procedure read_statscalvar_char, read_statscalvar_logical
  end interface

  private
  public :: write_statscalvar, read_statscalvar

contains

  !------------------------------------------------------------------
  ! STATIC SCALAR VARIABLES
  !------------------------------------------------------------------
  ! WRITE routines
  !------------------------------------------------------------------

  subroutine write_statscalvar_int(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(in)    :: values

    integer :: status,varid

    call write_scalar_common(file,prefix,name,NF90_INT,varid)
    status=nf90_put_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine write_statscalvar_int

  !------------------------------------------------------------------

  subroutine write_statscalvar_realsp(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),          intent(in)    :: values

    integer :: status,varid

    call write_scalar_common(file,prefix,name,NF90_FLOAT,varid)
    status=nf90_put_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine write_statscalvar_realsp

  !------------------------------------------------------------------

  subroutine write_statscalvar_realdp(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),          intent(in)    :: values

    integer :: status,varid

    call write_scalar_common(file,prefix,name,NF90_DOUBLE,varid)
    status=nf90_put_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine write_statscalvar_realdp

  !------------------------------------------------------------------

  subroutine write_statscalvar_char(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    character(*),      intent(in)    :: values

    integer :: status,varid

    call write_scalar_common(file,prefix,name,NF90_CHAR,varid)
    call set_define(file)
    status=nf90_put_att(file%ncid,varid,'value',trim(values))
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    call end_define(file)

  end subroutine write_statscalvar_char

  !------------------------------------------------------------------

  subroutine write_statscalvar_logical(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,           intent(in)    :: values

    integer :: status,varid

    call write_scalar_common(file,prefix,name,NF90_INT,varid)
    if (values) then
       status=nf90_put_var(file%ncid,varid,1)
    else
       status=nf90_put_var(file%ncid,varid,0)
    end if
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine write_statscalvar_logical

  !------------------------------------------------------------------

  subroutine write_scalar_common(file,prefix,name,typecode,varid)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(in)    :: typecode
    integer,           intent(out)   :: varid

    character(varnamelen) :: varname
    integer :: status

    call new_varname(varname,prefix,file%count)

    ! Create new variable, and label it
    call set_define(file)
    status=nf90_def_var(file%ncid,varname,typecode,varid=varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    call end_define(file)
    file%count=file%count+1

  end subroutine write_scalar_common
  
  !------------------------------------------------------------------
  ! STATIC SCALAR VARIABLES
  !------------------------------------------------------------------
  ! READ routines
  !------------------------------------------------------------------

  subroutine read_statscalvar_int(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(out)   :: values

    integer :: varid,status

    call read_scalar_common(file,prefix,name,varid)

    status=nf90_get_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine read_statscalvar_int

  !------------------------------------------------------------------

  subroutine read_statscalvar_realsp(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(sp),          intent(out)   :: values

    integer :: varid,status

    call read_scalar_common(file,prefix,name,varid)

    status=nf90_get_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine read_statscalvar_realsp

  !------------------------------------------------------------------

  subroutine read_statscalvar_realdp(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    real(dp),          intent(out)   :: values

    integer :: varid,status

    call read_scalar_common(file,prefix,name,varid)

    status=nf90_get_var(file%ncid,varid,values)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

  end subroutine read_statscalvar_realdp

  !------------------------------------------------------------------

  subroutine read_statscalvar_char(file,prefix,name,values)

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    character(*),      intent(out)   :: values

    integer :: varid,status,attlen
    character(500) :: tmpchar

    call read_scalar_common(file,prefix,name,varid)

    status=nf90_inquire_attribute(file%ncid,varid,'value',len=attlen)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)
    status=nf90_get_att(file%ncid,varid,'value',tmpchar)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

    values=tmpchar(:attlen)

  end subroutine read_statscalvar_char

  !------------------------------------------------------------------

  subroutine read_statscalvar_logical(file,prefix,name,values)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    logical,           intent(out)   :: values

    integer :: varid,status,tmp

    call read_scalar_common(file,prefix,name,varid)

    status=nf90_get_var(file%ncid,varid,tmp)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name)

    if (tmp==0) then
       values=.false.
    else if (tmp==1) then
       values=.true.
    else
       call write_log('Error in logical type for RESTART',GM_FATAL)
    end if

  end subroutine read_statscalvar_logical

  !------------------------------------------------------------------

  subroutine read_scalar_common(file,prefix,name,varid)

    use glimmer_log

    type(restart_file),intent(inout) :: file
    character(*),      intent(in)    :: prefix
    character(*),      intent(in)    :: name
    integer,           intent(inout) :: varid

    character(varnamelen) :: varname,nametest
    integer :: status,namelen

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

    file%count=file%count+1

  end subroutine read_scalar_common

end module glimmer_restart_statscal
