lib_LIBRARIES = libslap.a
if EXT_BLAS
 BLAS =
else
 BLAS = blas.f
endif
libslap_a_SOURCES = $(BLAS) dcgs.f dlaputil.f dmset.f dmvops.f mach.f xersla.f
EXTRA_libslap_a_SOURCES = blas.f
