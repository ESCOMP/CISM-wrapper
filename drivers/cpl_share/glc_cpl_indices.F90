module glc_cpl_indices
  
  use seq_flds_mod
  use mct_mod
  use glc_constants, only : glc_nec

  implicit none

  SAVE
  public    ! By default make data private

  ! drv -> glc

  integer :: index_x2g_Ss_tsrf01
  integer :: index_x2g_Ss_topo01
  integer :: index_x2g_Fgss_qice01
  integer :: index_x2g_Ss_tsrf02
  integer :: index_x2g_Ss_topo02
  integer :: index_x2g_Fgss_qice02
  integer :: index_x2g_Ss_tsrf03
  integer :: index_x2g_Ss_topo03
  integer :: index_x2g_Fgss_qice03
  integer :: index_x2g_Ss_tsrf04
  integer :: index_x2g_Ss_topo04
  integer :: index_x2g_Fgss_qice04
  integer :: index_x2g_Ss_tsrf05
  integer :: index_x2g_Ss_topo05
  integer :: index_x2g_Fgss_qice05
  integer :: index_x2g_Ss_tsrf06
  integer :: index_x2g_Ss_topo06
  integer :: index_x2g_Fgss_qice06
  integer :: index_x2g_Ss_tsrf07
  integer :: index_x2g_Ss_topo07
  integer :: index_x2g_Fgss_qice07
  integer :: index_x2g_Ss_tsrf08
  integer :: index_x2g_Ss_topo08
  integer :: index_x2g_Fgss_qice08
  integer :: index_x2g_Ss_tsrf09
  integer :: index_x2g_Ss_topo09
  integer :: index_x2g_Fgss_qice09
  integer :: index_x2g_Ss_tsrf10
  integer :: index_x2g_Ss_topo10
  integer :: index_x2g_Fgss_qice10
  integer :: nflds_x2g

  ! glc -> drv

  integer :: index_g2x_Sg_frac01
  integer :: index_g2x_Sg_topo01
  integer :: index_g2x_Fsgg_rofi01
  integer :: index_g2x_Fsgg_rofl01
  integer :: index_g2x_Fsgg_hflx01
  integer :: index_g2x_Sg_frac02
  integer :: index_g2x_Sg_topo02
  integer :: index_g2x_Fsgg_rofi02
  integer :: index_g2x_Fsgg_rofl02
  integer :: index_g2x_Fsgg_hflx02
  integer :: index_g2x_Sg_frac03
  integer :: index_g2x_Sg_topo03
  integer :: index_g2x_Fsgg_rofi03
  integer :: index_g2x_Fsgg_rofl03
  integer :: index_g2x_Fsgg_hflx03
  integer :: index_g2x_Sg_frac04
  integer :: index_g2x_Sg_topo04
  integer :: index_g2x_Fsgg_rofi04
  integer :: index_g2x_Fsgg_rofl04
  integer :: index_g2x_Fsgg_hflx04
  integer :: index_g2x_Sg_frac05
  integer :: index_g2x_Sg_topo05
  integer :: index_g2x_Fsgg_rofi05
  integer :: index_g2x_Fsgg_rofl05
  integer :: index_g2x_Fsgg_hflx05
  integer :: index_g2x_Sg_frac06
  integer :: index_g2x_Sg_topo06
  integer :: index_g2x_Fsgg_rofi06
  integer :: index_g2x_Fsgg_rofl06
  integer :: index_g2x_Fsgg_hflx06
  integer :: index_g2x_Sg_frac07
  integer :: index_g2x_Sg_topo07
  integer :: index_g2x_Fsgg_rofi07
  integer :: index_g2x_Fsgg_rofl07
  integer :: index_g2x_Fsgg_hflx07
  integer :: index_g2x_Sg_frac08
  integer :: index_g2x_Sg_topo08
  integer :: index_g2x_Fsgg_rofi08
  integer :: index_g2x_Fsgg_rofl08
  integer :: index_g2x_Fsgg_hflx08
  integer :: index_g2x_Sg_frac09
  integer :: index_g2x_Sg_topo09
  integer :: index_g2x_Fsgg_rofi09
  integer :: index_g2x_Fsgg_rofl09
  integer :: index_g2x_Fsgg_hflx09
  integer :: index_g2x_Sg_frac10
  integer :: index_g2x_Sg_topo10
  integer :: index_g2x_Fsgg_rofi10
  integer :: index_g2x_Fsgg_rofl10
  integer :: index_g2x_Fsgg_hflx10
  integer :: nflds_g2x	

contains

  subroutine glc_cpl_indices_set( )

    type(mct_aVect) :: g2x      ! temporary
    type(mct_aVect) :: x2g      ! temporary

    ! Determine attribute vector indices

    ! create temporary attribute vectors
    call mct_aVect_init(x2g, rList=seq_flds_x2g_fields, lsize=1)
    call mct_aVect_init(g2x, rList=seq_flds_g2x_fields, lsize=1)

    !-------------------------------------------------------------
    ! glc -> drv
    !-------------------------------------------------------------

    if (glc_nec >=  1) index_g2x_Sg_frac01   = mct_avect_indexra(g2x,'Sg_frac01') 
    if (glc_nec >=  1) index_g2x_Sg_topo01   = mct_avect_indexra(g2x,'Sg_topo01')
    if (glc_nec >=  1) index_g2x_Fsgg_rofi01 = mct_avect_indexra(g2x,'Fsgg_rofi01')
    if (glc_nec >=  1) index_g2x_Fsgg_rofl01 = mct_avect_indexra(g2x,'Fsgg_rofl01')
    if (glc_nec >=  1) index_g2x_Fsgg_hflx01 = mct_avect_indexra(g2x,'Fsgg_hflx01')

    if (glc_nec >=  2) index_g2x_Sg_frac02   = mct_avect_indexra(g2x,'Sg_frac02')
    if (glc_nec >=  2) index_g2x_Sg_topo02   = mct_avect_indexra(g2x,'Sg_topo02')
    if (glc_nec >=  2) index_g2x_Fsgg_rofi02 = mct_avect_indexra(g2x,'Fsgg_rofi02')
    if (glc_nec >=  2) index_g2x_Fsgg_rofl02 = mct_avect_indexra(g2x,'Fsgg_rofl02')
    if (glc_nec >=  2) index_g2x_Fsgg_hflx02 = mct_avect_indexra(g2x,'Fsgg_hflx02')

    if (glc_nec >=  3) index_g2x_Sg_frac03   = mct_avect_indexra(g2x,'Sg_frac03')
    if (glc_nec >=  3) index_g2x_Sg_topo03   = mct_avect_indexra(g2x,'Sg_topo03')
    if (glc_nec >=  3) index_g2x_Fsgg_rofi03 = mct_avect_indexra(g2x,'Fsgg_rofi03')
    if (glc_nec >=  3) index_g2x_Fsgg_rofl03 = mct_avect_indexra(g2x,'Fsgg_rofl03')
    if (glc_nec >=  3) index_g2x_Fsgg_hflx03 = mct_avect_indexra(g2x,'Fsgg_hflx03')
 
    if (glc_nec >=  4) index_g2x_Sg_frac04   = mct_avect_indexra(g2x,'Sg_frac04')
    if (glc_nec >=  4) index_g2x_Sg_topo04   = mct_avect_indexra(g2x,'Sg_topo04')
    if (glc_nec >=  4) index_g2x_Fsgg_rofi04 = mct_avect_indexra(g2x,'Fsgg_rofi04')
    if (glc_nec >=  4) index_g2x_Fsgg_rofl04 = mct_avect_indexra(g2x,'Fsgg_rofl04')
    if (glc_nec >=  4) index_g2x_Fsgg_hflx04 = mct_avect_indexra(g2x,'Fsgg_hflx04')

    if (glc_nec >=  5) index_g2x_Sg_frac05   = mct_avect_indexra(g2x,'Sg_frac05')
    if (glc_nec >=  5) index_g2x_Sg_topo05   = mct_avect_indexra(g2x,'Sg_topo05')
    if (glc_nec >=  5) index_g2x_Fsgg_rofi05 = mct_avect_indexra(g2x,'Fsgg_rofi05')
    if (glc_nec >=  5) index_g2x_Fsgg_rofl05 = mct_avect_indexra(g2x,'Fsgg_rofl05')
    if (glc_nec >=  5) index_g2x_Fsgg_hflx05 = mct_avect_indexra(g2x,'Fsgg_hflx05')

    if (glc_nec >=  6) index_g2x_Sg_frac06   = mct_avect_indexra(g2x,'Sg_frac06')
    if (glc_nec >=  6) index_g2x_Sg_topo06   = mct_avect_indexra(g2x,'Sg_topo06')
    if (glc_nec >=  6) index_g2x_Fsgg_rofi06 = mct_avect_indexra(g2x,'Fsgg_rofi06')
    if (glc_nec >=  6) index_g2x_Fsgg_rofl06 = mct_avect_indexra(g2x,'Fsgg_rofl06')
    if (glc_nec >=  6) index_g2x_Fsgg_hflx06 = mct_avect_indexra(g2x,'Fsgg_hflx06')

    if (glc_nec >=  7) index_g2x_Sg_frac07   = mct_avect_indexra(g2x,'Sg_frac07')
    if (glc_nec >=  7) index_g2x_Sg_topo07   = mct_avect_indexra(g2x,'Sg_topo07')
    if (glc_nec >=  7) index_g2x_Fsgg_rofi07 = mct_avect_indexra(g2x,'Fsgg_rofi07')
    if (glc_nec >=  7) index_g2x_Fsgg_rofl07 = mct_avect_indexra(g2x,'Fsgg_rofl07')
    if (glc_nec >=  7) index_g2x_Fsgg_hflx07 = mct_avect_indexra(g2x,'Fsgg_hflx07')

    if (glc_nec >=  8) index_g2x_Sg_frac08   = mct_avect_indexra(g2x,'Sg_frac08')
    if (glc_nec >=  8) index_g2x_Sg_topo08   = mct_avect_indexra(g2x,'Sg_topo08')
    if (glc_nec >=  8) index_g2x_Fsgg_rofi08 = mct_avect_indexra(g2x,'Fsgg_rofi08')
    if (glc_nec >=  8) index_g2x_Fsgg_rofl08 = mct_avect_indexra(g2x,'Fsgg_rofl08')
    if (glc_nec >=  8) index_g2x_Fsgg_hflx08 = mct_avect_indexra(g2x,'Fsgg_hflx08')

    if (glc_nec >=  9) index_g2x_Sg_frac09   = mct_avect_indexra(g2x,'Sg_frac09')
    if (glc_nec >=  9) index_g2x_Sg_topo09   = mct_avect_indexra(g2x,'Sg_topo09')
    if (glc_nec >=  9) index_g2x_Fsgg_rofi09 = mct_avect_indexra(g2x,'Fsgg_rofi09')
    if (glc_nec >=  9) index_g2x_Fsgg_rofl09 = mct_avect_indexra(g2x,'Fsgg_rofl09')
    if (glc_nec >=  9) index_g2x_Fsgg_hflx09 = mct_avect_indexra(g2x,'Fsgg_hflx09')

    if (glc_nec >= 10) index_g2x_Sg_frac10   = mct_avect_indexra(g2x,'Sg_frac10')
    if (glc_nec >= 10) index_g2x_Sg_topo10   = mct_avect_indexra(g2x,'Sg_topo10')
    if (glc_nec >= 10) index_g2x_Fsgg_rofi10 = mct_avect_indexra(g2x,'Fsgg_rofi10')
    if (glc_nec >= 10) index_g2x_Fsgg_rofl10 = mct_avect_indexra(g2x,'Fsgg_rofl10')
    if (glc_nec >= 10) index_g2x_Fsgg_hflx10 = mct_avect_indexra(g2x,'Fsgg_hflx10')

    !-------------------------------------------------------------
    ! drv -> glc
    !-------------------------------------------------------------

    if (glc_nec >=  1) index_x2g_Ss_tsrf01   = mct_avect_indexra(x2g,'Ss_tsrf01')
    if (glc_nec >=  1) index_x2g_Ss_topo01   = mct_avect_indexra(x2g,'Ss_topo01')
    if (glc_nec >=  1) index_x2g_Fgss_qice01 = mct_avect_indexra(x2g,'Fgss_qice01')

    if (glc_nec >=  2) index_x2g_Ss_tsrf02   = mct_avect_indexra(x2g,'Ss_tsrf02')
    if (glc_nec >=  2) index_x2g_Ss_topo02   = mct_avect_indexra(x2g,'Ss_topo02')
    if (glc_nec >=  2) index_x2g_Fgss_qice02 = mct_avect_indexra(x2g,'Fgss_qice02')

    if (glc_nec >=  3) index_x2g_Ss_tsrf03   = mct_avect_indexra(x2g,'Ss_tsrf03')
    if (glc_nec >=  3) index_x2g_Ss_topo03   = mct_avect_indexra(x2g,'Ss_topo03')
    if (glc_nec >=  3) index_x2g_Fgss_qice03 = mct_avect_indexra(x2g,'Fgss_qice03')

    if (glc_nec >=  4) index_x2g_Ss_tsrf04   = mct_avect_indexra(x2g,'Ss_tsrf04')
    if (glc_nec >=  4) index_x2g_Ss_topo04   = mct_avect_indexra(x2g,'Ss_topo04')
    if (glc_nec >=  4) index_x2g_Fgss_qice04 = mct_avect_indexra(x2g,'Fgss_qice04')

    if (glc_nec >=  5) index_x2g_Ss_tsrf05   = mct_avect_indexra(x2g,'Ss_tsrf05')
    if (glc_nec >=  5) index_x2g_Ss_topo05   = mct_avect_indexra(x2g,'Ss_topo05')
    if (glc_nec >=  5) index_x2g_Fgss_qice05 = mct_avect_indexra(x2g,'Fgss_qice05')

    if (glc_nec >=  6) index_x2g_Ss_tsrf06   = mct_avect_indexra(x2g,'Ss_tsrf06')
    if (glc_nec >=  6) index_x2g_Ss_topo06   = mct_avect_indexra(x2g,'Ss_topo06')
    if (glc_nec >=  6) index_x2g_Fgss_qice06 = mct_avect_indexra(x2g,'Fgss_qice06')

    if (glc_nec >=  7) index_x2g_Ss_tsrf07   = mct_avect_indexra(x2g,'Ss_tsrf07')
    if (glc_nec >=  7) index_x2g_Ss_topo07   = mct_avect_indexra(x2g,'Ss_topo07')
    if (glc_nec >=  7) index_x2g_Fgss_qice07 = mct_avect_indexra(x2g,'Fgss_qice07')

    if (glc_nec >=  8) index_x2g_Ss_tsrf08   = mct_avect_indexra(x2g,'Ss_tsrf08')
    if (glc_nec >=  8) index_x2g_Ss_topo08   = mct_avect_indexra(x2g,'Ss_topo08')
    if (glc_nec >=  8) index_x2g_Fgss_qice08 = mct_avect_indexra(x2g,'Fgss_qice08')

    if (glc_nec >=  9) index_x2g_Ss_tsrf09   = mct_avect_indexra(x2g,'Ss_tsrf09')
    if (glc_nec >=  9) index_x2g_Ss_topo09   = mct_avect_indexra(x2g,'Ss_topo09')
    if (glc_nec >=  9) index_x2g_Fgss_qice09 = mct_avect_indexra(x2g,'Fgss_qice09')

    if (glc_nec >= 10) index_x2g_Ss_tsrf10   = mct_avect_indexra(x2g,'Ss_tsrf10')
    if (glc_nec >= 10) index_x2g_Ss_topo10   = mct_avect_indexra(x2g,'Ss_topo10')
    if (glc_nec >= 10) index_x2g_Fgss_qice10 = mct_avect_indexra(x2g,'Fgss_qice10')

    call mct_aVect_clean(x2g)
    call mct_aVect_clean(g2x)

  end subroutine glc_cpl_indices_set

end module glc_cpl_indices
