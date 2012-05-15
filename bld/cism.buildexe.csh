#! /bin/csh -f 

cd $OBJROOT/glc/obj

cat >! Filepath << EOF
$CASEROOT/SourceMods/src.cism
$CODEROOT/glc/cism/drivers/cpl_share
$CODEROOT/glc/cism/drivers/cpl_mct
$CODEROOT/glc/cism/source_glc
$CODEROOT/glc/cism/source_glimmer-cism
$CODEROOT/glc/cism/source_slap
$CODEROOT/glc/cism/mpi
EOF

gmake complib -j $GMAKE_J MODEL=cism COMPLIB=$LIBROOT/libglc.a -f $CASETOOLS/Makefile MACFILE=$CASEROOT/Macros.$MACH || exit 2

