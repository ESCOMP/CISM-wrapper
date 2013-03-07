#! /bin/csh -f 

# directory in which glc is built
set glc_dir=$EXEROOT/glc

# directory in which glc obj files are built
set glc_obj_dir=$OBJROOT/glc/obj

# directory in which glimmer-cism library is created
set cism_libdir=$glc_dir/lib

# directory in which we can find source mods
set sourcemod_dir=$CASEROOT/SourceMods/src.cism

cd $glc_obj_dir

set comp = 'unknown'
if ($COMP_INTERFACE == 'MCT' ) set comp = mct
if ($COMP_INTERFACE == 'ESMF') set comp = esmf

# ----------------------------------------------------------------------
# Create Filepath
# ----------------------------------------------------------------------
# The following just gives the filepath for the cesm-specific code:
# the glimmer-cism stuff is picked up by the cmake-based build
cat >! Filepath << EOF
$sourcemod_dir
$CODEROOT/glc/cism/drivers/cpl_share
$CODEROOT/glc/cism/drivers/cpl_$comp
$CODEROOT/glc/cism/source_glc
$CODEROOT/glc/cism/mpi
EOF

# ----------------------------------------------------------------------
# Set options to cmake
# ----------------------------------------------------------------------
# Note that some other generic CMAKE options are set in the Makefile
set cmake_opts=""
set cmake_opts="$cmake_opts -D CISM_COUPLED=ON"
set cmake_opts="$cmake_opts -D CISM_USE_MPI_WITH_SLAP=ON"
# CISM_USE_GPTL_INSTRUMENTATION is unnecessary (and possibly harmful)
# when built inside CESM; for CESM we instead use -DCCSMCOUPLED, which
# also gives us timing instrumentation
set cmake_opts="$cmake_opts -D CISM_USE_GPTL_INSTRUMENTATION=OFF"
set cmake_opts="$cmake_opts -D CISM_BINARY_DIR=$glc_dir"
set cmake_opts="$cmake_opts -D CMAKE_Fortran_MODULE_DIRECTORY=$glc_obj_dir"
set cmake_opts="$cmake_opts -D GLIMMER_NETCDF_DIR="\$"(NETCDF_PATH)"
set cmake_opts="$cmake_opts -D CISM_MPI_INC_DIR="\$"(INC_MPI)"
set cmake_opts="$cmake_opts -D GLIMMER_SOURCEMOD_DIR=$sourcemod_dir/glimmer-cism"
if ($CISM_USE_TRILINOS == 'TRUE') then
    set cmake_opts="$cmake_opts -D NO_TRILINOS=OFF"
    set cmake_opts="$cmake_opts -D CISM_MPI_MODE=ON"
    set cmake_opts="$cmake_opts -D CISM_SERIAL_MODE=OFF"
    set cmake_opts="$cmake_opts -D GLIMMER_TRILINOS_DIR="\$"(TRILINOS_PATH)"
else
    set cmake_opts="$cmake_opts -D NO_TRILINOS=ON"
    set cmake_opts="$cmake_opts -D CISM_MPI_MODE=OFF"
    set cmake_opts="$cmake_opts -D CISM_SERIAL_MODE=ON"
endif

# ----------------------------------------------------------------------
# create the glimmer-cism makefile by running cmake (done via a rule
# in the system-level makefile)
# ----------------------------------------------------------------------
$GMAKE $glc_dir/Makefile MODEL=cism USER_CMAKE_OPTS="$cmake_opts" GLC_DIR=$glc_dir -f $CASETOOLS/Makefile || exit 1

# ----------------------------------------------------------------------
# create the glimmer-cism library (or libraries), using the makefile
# created by cmake
# ----------------------------------------------------------------------
pushd $glc_dir
$GMAKE -j $GMAKE_J || exit 2
popd

# ----------------------------------------------------------------------
# Create dependency file needed for building the cesm-specific portion
# of glc
#
# Note: This generally doesn't need to be done explicitly (it is done
# automatically via CESM's makefile). But we do it explicitly so we
# can add stuff to the dependency file
# ----------------------------------------------------------------------
$GMAKE $glc_obj_dir/Depends MODEL=cism -f $CASETOOLS/Makefile || exit 3

# Append 'libglimmercismfortran.a' to the end of each line in the
# Depends file that does not already contain it.
# 
# Rationale: Some of the source files in the cesm-specific code depend
# on files included in this library. Ideally, we would be able to
# determine the actual dependencies, but that's not easy with the
# current tools and the fact that we build the glimmer-cism code using
# a different build system than the cesm-specific code. So for now, we
# just rebuild all the cesm-specific code whenever anything in the
# libglimmercismfortran.a library changes.
#
# (We only add libglimmercismfortran.a to lines not already containing
# this string so that when we rebuild we don't get an ever-growing
# string of instances of 'libglimmercismfortran.a'.)
cat >! $glc_obj_dir/Depends.awk <<EOF
/libglimmercismfortran\.a/ {print \$0; next}
{print \$0 " $cism_libdir/libglimmercismfortran.a"}
EOF
awk -f $glc_obj_dir/Depends.awk $glc_obj_dir/Depends > $glc_obj_dir/Depends.temp || exit 4
mv $glc_obj_dir/Depends.temp $glc_obj_dir/Depends || exit 5

# ----------------------------------------------------------------------
# create the cesm-specific portion of the glc library using cesm's makefile
# ----------------------------------------------------------------------
$GMAKE complib -j $GMAKE_J MODEL=cism COMPLIB=$LIBROOT/libglc.a GLC_DIR=$glc_dir -f $CASETOOLS/Makefile || exit 6

exit 0

