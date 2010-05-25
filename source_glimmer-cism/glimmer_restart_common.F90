! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                             +
! +  glimmer_restart_common.f90 - part of the GLIMMER ice model + 
! +                                                             +
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

module glimmer_restart_common

  use netcdf
  use glimmer_global, only: sp,dp

  implicit none

  integer,parameter :: restartflen = 80
  integer,parameter :: varnamelen = 50
  character(1),dimension(3) :: com_dims=(/'x','y','t'/)
  integer,parameter :: RESTART_READ  = 0
  integer,parameter :: RESTART_CREATE = 1

  type restart_file
     character(restartflen) :: fname
     integer :: ncid
     logical :: def
     integer :: count
     logical :: write   !If false, we're open to read
  end type restart_file

  ! Interfaces ----------------------------------------
  ! Pointer scalar variables

!!$  interface write_pointscalvar
!!$
!!$  end interface
!!$
!!$  interface read_pointscalvar
!!$
!!$  end interface

  ! Allocatable arrays

!!$  interface write_allocarr
!!$     module procedure write_allocarr_int_1d, write_allocarr_realsp_1d, write_allocarr_realdp_1d
!!$     module procedure write_allocarr_int_2d, write_allocarr_realsp_2d, write_allocarr_realdp_2d
!!$     module procedure write_allocarr_int_3d, write_allocarr_realsp_3d, write_allocarr_realdp_3d
!!$  end interface
!!$
!!$  interface read_allocarr
!!$     module procedure read_allocarr_int_1d, read_allocarr_realsp_1d, read_allocarr_realdp_1d
!!$     module procedure read_allocarr_int_2d, read_allocarr_realsp_2d, read_allocarr_realdp_2d
!!$     module procedure read_allocarr_int_3d, read_allocarr_realsp_3d, read_allocarr_realdp_3d
!!$  end interface

  ! Pointer arrays

contains

  !-----------------------------------------------------------------
  ! Shared stuff
  !-----------------------------------------------------------------

  function open_restart_file(fname,mode)

    use glimmer_log

    type(restart_file) :: open_restart_file
    character(*),intent(in) :: fname
    integer,optional,intent(in) :: mode
    integer :: status
    integer :: md

    if (present(mode)) then
       md=mode
    else
       md=RESTART_READ
    end if

    select case(md)
    case(RESTART_READ)
       open_restart_file%fname=fname
       status = nf90_open(fname,NF90_NOWRITE,open_restart_file%ncid)
       if (status/=0) call ncdf_err(status,__LINE__,fname)
       open_restart_file%def = .false.
       open_restart_file%count = 0
       open_restart_file%write = .false.
    case(RESTART_CREATE)
       open_restart_file%fname=fname
       status = nf90_create(fname,NF90_CLOBBER,open_restart_file%ncid)
       if (status/=0) call ncdf_err(status,__LINE__,fname)
       open_restart_file%def = .true.
       open_restart_file%count = 0
       open_restart_file%write = .true.
    case default
       call write_log('Unknown mode in open_restart_file',GM_FATAL)
    end select

  end function open_restart_file

  !------------------------------------------------------------------

  subroutine close_restart_file(file)

    type(restart_file) :: file
    integer :: status

    status = nf90_close(file%ncid)
    if (status/=0) call ncdf_err(status,__LINE__,file%fname)

  end subroutine close_restart_file

  !------------------------------------------------------------------

  subroutine new_dimension(file,prefix,len,id)

    type(restart_file) :: file
    character(1),intent(in) :: prefix
    integer,intent(in) :: len
    integer,intent(out) :: id

    integer :: status
    character(10) :: dimname

    call set_define(file)

    write(dimname,'(I9)')len
    dimname=trim(prefix)//trim(adjustl(dimname))

    status=nf90_inq_dimid(file%ncid,dimname,id)
    if (status==NF90_NOERR) then
       return
    else if (status==NF90_EBADDIM) then
       status=nf90_def_dim(file%ncid,dimname,len,id)
       if (status==NF90_EBADDIM) call ncdf_err(status,__LINE__,file%fname,prefix)
    else
       call ncdf_err(status,__LINE__)
    end if

  end subroutine new_dimension

  !------------------------------------------------------------------

  subroutine set_define(file)

    type(restart_file),intent(inout) :: file
    integer :: status

    if (.not.file%def) then
       status = nf90_redef(file%ncid)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,file%fname)
       file%def=.true.
    end if

  end subroutine set_define

  !------------------------------------------------------------------

  subroutine end_define(file)

    type(restart_file),intent(inout) :: file
    integer :: status

    if (file%def) then
       status = nf90_enddef(file%ncid)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,file%fname)
       file%def=.false.
    end if

  end subroutine end_define

  !------------------------------------------------------------------

  subroutine check_read(file)
    
    use glimmer_log

    type(restart_file),intent(inout) :: file
    
    if (file%write) then
       call write_log('Attempted to read from restart file open for write',GM_FATAL)
    end if

  end subroutine check_read

  !------------------------------------------------------------------

  subroutine check_write(file)
    
    use glimmer_log

    type(restart_file),intent(inout) :: file
    
    if (.not.file%write) then
       call write_log('Attempted to write to restart file open for read',GM_FATAL)
    end if

  end subroutine check_write

  !------------------------------------------------------------------
  ! Utility code
  !------------------------------------------------------------------

  subroutine new_varname(varname,prefix,count)

    character(*),intent(out) :: varname
    character(*),intent(in)  :: prefix
    integer,     intent(in)  :: count

    write(varname,'(I9)')count
    varname=trim(prefix)//trim(adjustl(varname))

  end subroutine new_varname

  !------------------------------------------------------------------

  subroutine write_lbounds(ncid,varid,lb)

    integer,intent(in) :: ncid
    integer,intent(in) :: varid
    integer,dimension(:),intent(in) :: lb

    integer :: nbound,i,status
    character :: itxt

    nbound=size(lb)

    do i=1,nbound
       write(itxt,'(I1)')i
       status=nf90_put_att(ncid,varid,'lbound'//itxt,lb(i))
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,' in WRITE_LBOUNDS')
    end do

  end subroutine write_lbounds

  !------------------------------------------------------------------

  subroutine read_lbounds(ncid,varid,lb)

    use netcdf

    integer,intent(in)  :: ncid
    integer,intent(in)  :: varid
    integer,dimension(:),intent(out) :: lb

    integer :: nbound,i,status
    character :: itxt

    nbound=size(lb)

    do i=1,nbound
       write(itxt,'(I1)')i
       status=nf90_get_att(ncid,varid,'lbound'//itxt,lb(i))
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,' in READ_LBOUNDS')
    end do

  end subroutine read_lbounds

  !------------------------------------------------------------------

  subroutine write_allocatable(file,prefix,name,alloc)

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    logical :: alloc

    character(varnamelen) :: varname
    integer :: varid,status

    call set_define(file)

    call new_varname(varname,prefix,file%count)
    status=nf90_def_var(file%ncid,varname,NF90_CHAR,varid=varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    if (.not.alloc) then
       status=nf90_put_att(file%ncid,varid,name='UNALLOC',values='UNALLOC')
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

    call end_define(file)
    file%count=file%count+1

  end subroutine write_allocatable

  !------------------------------------------------------------------

  subroutine read_allocatable(file,prefix,name,alloc)

    use glimmer_log

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    logical :: alloc

    character(varnamelen) :: varname,nametest,nulltest
    integer :: varid,status,namelen

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

    status=nf90_get_att(file%ncid,varid,'UNALLOC',nulltest)
    select case(status)
    case(NF90_NOERR)
       alloc=.false.
    case(NF90_ENOTATT)
       alloc=.true.
    case default
       call ncdf_err(status,__LINE__,prefix,name,varname)
    end select

    file%count=file%count+1

  end subroutine read_allocatable
  
  !------------------------------------------------------------------

  subroutine write_pointer(file,prefix,name,assoc)

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    logical :: assoc

    character(varnamelen) :: varname
    integer :: varid,status

    call set_define(file)

    call new_varname(varname,prefix,file%count)
    status=nf90_def_var(file%ncid,varname,NF90_CHAR,varid=varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    if (.not.assoc) then
       status=nf90_put_att(file%ncid,varid,name='NULL',values='NULL')
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    end if

    call end_define(file)
    file%count=file%count+1

  end subroutine write_pointer

  !------------------------------------------------------------------

  subroutine write_null_array_pointer(file,prefix,name)

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name

    character(varnamelen) :: varname
    integer :: varid,status

    call set_define(file)

    call new_varname(varname,prefix,file%count)
    status=nf90_def_var(file%ncid,varname,NF90_CHAR,varid=varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='NULL',values='NULL')
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    file%count=file%count+1

  end subroutine write_null_array_pointer

  !------------------------------------------------------------------

  subroutine write_array_pointer(file,prefix,name,sh)

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    integer,dimension(:) :: sh

    character(varnamelen) :: varname
    integer :: varid,status,dimid

    call set_define(file)
    call new_dimension(file,com_dims(1),size(sh),dimid)

    call new_varname(varname,prefix,file%count)
    status=nf90_def_var(file%ncid,varname,NF90_INT,(/dimid/),varid)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    status=nf90_put_att(file%ncid,varid,name='varname',values=name)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    call end_define(file)
    status=nf90_put_var(file%ncid,varid,sh)
    if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)

    file%count=file%count+1

  end subroutine write_array_pointer

  !------------------------------------------------------------------

  subroutine read_array_pointer(file,prefix,name,sh,assoc)

    use glimmer_log

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    integer,dimension(:) :: sh
    logical,intent(out) :: assoc

    character(varnamelen) :: varname,nametest,nulltest
    integer :: varid,status,namelen

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
       assoc=.false.
    case(NF90_ENOTATT)
       assoc=.true.
       status=nf90_get_var(file%ncid,varid,sh)
       if (status/=NF90_NOERR) call ncdf_err(status,__LINE__,prefix,name,varname)
    case default
       call ncdf_err(status,__LINE__,prefix,name,varname)
    end select

    file%count=file%count+1

  end subroutine read_array_pointer

  !------------------------------------------------------------------

  subroutine read_pointer(file,prefix,name,assoc)

    use glimmer_log

    type(restart_file), intent(inout) :: file
    character(*) :: prefix
    character(*) :: name
    logical :: assoc

    character(varnamelen) :: varname,nametest,nulltest
    integer :: varid,status,namelen

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
       assoc=.false.
    case(NF90_ENOTATT)
       assoc=.true.
    case default
       call ncdf_err(status,__LINE__,prefix,name,varname)
    end select

    file%count=file%count+1

  end subroutine read_pointer

  !------------------------------------------------------------------

  subroutine ncdf_err(status,line,char1,char2,char3)

    integer :: status
    integer :: line
    character(*),optional :: char1,char2,char3
    
    character(150) :: char

    char = ''
    if (present(char1)) char=trim(char)//trim(char1)
    if (present(char2)) char=trim(char)//' '//trim(char2)
    if (present(char3)) char=trim(char)//' '//trim(char3)

    print*,'NetCDF ERROR: ',trim(nf90_strerror(status)),' at ',line
    if (present(char1)) print*,'Diagnostics: ',char
    stop

  end subroutine ncdf_err

end module glimmer_restart_common
