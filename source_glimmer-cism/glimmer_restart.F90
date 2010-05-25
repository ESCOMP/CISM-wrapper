! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +                                                           +
! +  glimmer_restart.f90 - part of the GLIMMER ice model      + 
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

#ifdef HAVE_CONFIG_H
#include <config.inc>
#endif

module glimmer_restart
  
  implicit none

  !*FD This module reads and writes all the module variables
  !*FD in the glimmer code-base to file.

contains

  subroutine glimmer_write_mod_rst(rfile)

    use glimmer_log
    use glimmer_restart_common

#ifdef RESTARTS
    use glimmer_config
    use glimmer_coordinates
    use glimmer_daily_pdd
    use glimmer_map_types
    use glimmer_ncdf
    use glimmer_pdd
    use glimmer_sparse
    use glimmer_ts
    use glimmer_scales
    use profile
    use glimmer_paramets
#endif

    type(restart_file) :: rfile      !*FD Open restart file 

#ifdef RESTARTS
    call glimmer_config_modrsw(rfile)
    call glimmer_coordinates_modrsw(rfile)
    call glimmer_daily_pdd_modrsw(rfile)
    call glimmer_map_types_modrsw(rfile)
    call glimmer_ncdf_modrsw(rfile)
    call glimmer_pdd_modrsw(rfile)
    call glimmer_sparse_modrsw(rfile)
    call glimmer_ts_modrsw(rfile)
    call glimmer_scales_modrsw(rfile)
    call profile_modrsw(rfile)
    call glimmer_paramets_modrsw(rfile)
#else
    call write_log('No restart code available - rebuild GLIMMER with --enable-restarts',GM_FATAL)
#endif

  end subroutine glimmer_write_mod_rst

  subroutine glimmer_read_mod_rst(rfile)

    use glimmer_log
    use glimmer_restart_common

#ifdef RESTARTS
    use glimmer_config
    use glimmer_coordinates
    use glimmer_daily_pdd
    use glimmer_map_types
    use glimmer_ncdf
    use glimmer_pdd
    use glimmer_sparse
    use glimmer_ts
    use glimmer_scales
    use profile
    use glimmer_paramets
#endif

    type(restart_file) :: rfile      !*FD Open restart file 

#ifdef RESTARTS
    call glimmer_config_modrsr(rfile)
    call glimmer_coordinates_modrsr(rfile)
    call glimmer_daily_pdd_modrsr(rfile)
    call glimmer_map_types_modrsr(rfile)
    call glimmer_ncdf_modrsr(rfile)
    call glimmer_pdd_modrsr(rfile)
    call glimmer_sparse_modrsr(rfile)
    call glimmer_ts_modrsr(rfile)
    call glimmer_scales_modrsr(rfile)
    call profile_modrsr(rfile)
    call glimmer_paramets_modrsr(rfile)
#else
    call write_log('No restart code available - rebuild GLIMMER with --enable-restarts',GM_FATAL)
#endif

  end subroutine glimmer_read_mod_rst

end module glimmer_restart
