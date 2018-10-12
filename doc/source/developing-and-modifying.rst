.. _developing-and-modifying:

*****************************
Developing and modifying CISM
*****************************

=================================
 Source code directory structure
=================================

FIXME: Bill S needs to rewrite this section

In the CESM directory structure, each model component sits under a
directory with a three-letter acronym: e.g., *atm* for the atmosphere
model, *lnd* for the land surface, *ocn* for the ocean, and *ice* for
sea ice. The ice sheet component resides in a directory called *glc*.
Within the *glc* directory are three subdirectories: *sglc* (a stub
model), *xglc* (a “dead” model), and *cism* (the physical model).

Inside the *cism* subdirectory are several more subdirectories:

-  *glimmer-cism*, which contains source code from Glimmer-CISM. This is
       organized into various sub-directories, some of which are used in
       CESM (chiefly *libglide, libglimmer, libglimmer-solve,
       libglint*), and others which are only used when building
       Glimmer-CISM in standalone mode, outside the context of CESM
       (e.g., *example-drivers*)

-  *source\_glc*, which contains wrapper modules that link Glimmer-CISM
       to the CESM coupler.

-  *drivers*, which contains two versions of the *glc* driver: one for
       use in the MCT coupling framework and the other for the ESMF
       framework.

-  *bld,* which contains files required to build the code and create
       namelist files. The sub-directory *namelist\_files* contains xml
       files that describe all possible namelist / configuration
       settings, and their default values (see Section 2.7 for the
       preferred way to modify these settings).

-  *tools*, which contains tools for generating land/ice-sheet grid
       overlap files.

-  *test*, which contains code to test parts of the source code.

-  *mpi* and *serial*, which have appropriate versions of source code
       that can be used for parallel and serial runs, respectively. The
       *serial* directory is obsolete; now the *mpi* directory is used
       even when running on a single processor.

The files of most interest to users are in the *glimmer-cism* and
*source\_glc* directories. The safest way to change source code in these
directories is to copy the file to the
*SourceMods/src.cism/glimmer-cism* or *SourceMods/src.cism/*
subdirectories within the case directory and edit the file there. When
the code is built, the contents of these directories will automatically
overwrite any files of the same name in the model source code
directories. See section 2.8 for details.

====================================
 Simulating the Greenland Ice Sheet
====================================

FIXME: Gunter needs to rewrite this section

A primary motivation for having
a CESM ice sheet model is to do climate change experiments with a
dynamic Greenland Ice Sheet (GrIS). The first step is to simulate a
present-day (or preindustrial) ice sheet that is in steady-state with
the CESM climate and is not too different in thickness, extent, and
velocity from the real GrIS. If we cannot do this, then either we will
start climate change simulations with an unrealistic GrIS, or we will
start with a realistic GrIS that is far from steady state, making it
difficult to distinguish the climate-change signal from model
transients.

It can be challenging to generate a realistic ice sheet, for several
reasons: (1) The surface mass balance computed in CESM could be
inaccurate; (2) the present-day GrIS may not be in steady-state with
the present-day (or preindustrial) climate. Our working hypotheses are
that (1) If the SMB is reasonably accurate, we can obtain a reasonable
large-scale thickness and extent for the GIS; (2) With a higher-order
dynamics scheme and some judicious tuning, we can generate ice streams
and outlet glaciers in the right locations with realistic velocities;
and (3) The present-day GrIS is not far from steady-state with the
preindustrial climate. These hypotheses are now being tested. Early
results are reported in a series of papers (Lipscomb et al. 2012;
Vizcaíno et al. 2012a and 2012b) submitted to the CESM special issue
of the *Journal of Climate*.

Obtaining an accurate surface mass balance may require some tuning in
CTSM; see Section 5 for details. The current default settings
may not be optimal. The config files will be updated when we have more
experience in running the model.

=================
 Ice sheet grids
=================

FIXME: Bill S needs to rewrite this section

Local ice sheet grids must be rectangular; typically they are polar
stereographic projections. For Greenland, three grids are currently
supported, with resolutions of 20 km and 4 km, respectively.
Each local grid is compatible with any of the three supported global
resolutions. The current default is the *gland4* 4 km grid. This can
be changed by modifying CISM\_GRID to the desired value (gland4 or
gland20) in env\_run.xml. A number of configuration defaults depend
on the grid. You can see the effect of changing *CISM\_GRID* by running
*preview\_namelists* before and after making this change. The rules that
determine default values are given in
*models/glc/cism/bld/namelist\_files/namelist\_defaults\_cism.xml*.

