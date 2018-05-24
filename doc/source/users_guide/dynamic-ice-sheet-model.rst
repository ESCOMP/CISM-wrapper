.. _dynamic-ice-sheet-model:

******************************
 The dynamic ice sheet model
******************************

This section gives a brief overview of ice flow modeling and of
CISM, the dynamic ice sheet model in CESM. For more details,
including a technical description of the model, please see Rutt et al.
(2009) and the CISM documentation. The documentation is slightly
out of date but still provides a useful description of Glimmer-CISM’s
dynamical core (GLISSADE) and climate model interface (GLAD).


=======================================
 The Community Ice Sheet Model
=======================================

CISM is a thermomechanical ice sheet model that solves the
equations of ice flow, given suitable approximations and boundary
conditions. The source code is written primarily in Fortran 90 and 95.
The model resides on the github repository
(https://github.com/ESCOMP/cism), where it is under active development.
CISM2.1 is currently the default version of CESM2.0.

CISM2.1 introduces a new dynamical core, GLISSADE, which runs in parallel
and solves equations – for the conservation of mass, momentum, and
internal energy. The version of GLISSADE currently in CESM gives several
choices of supported Stokes approximations: the shallow-ice approximation
(SIA), the shallow-shelf approximation (SSA), the depth-integrated viscosity
approximation (DIVA), and the Blatter-Pattyn (BP) approximation; DIVA is
the default solver for CISM in the CESM framework.

In previous versions of CESM with CISM coupling capabilities, the
dynamical core of the model was known as GLIDE and only solved for the
shallow-ice approximation in serial. While this option is still available
it is no longer supported.

The surface boundary conditions (e.g., the surface temperature and
surface mass balance) are supplied by a climate driver. When
CISM is run in CESM, the climate driver is GLAD, which receives
the temperature and SMB from the coupler and passes them to the
ice-sheet grid. The lower boundary conditions are given by an isostasy
model, which computes the elevation of the lower surface, and by a
geothermal model, which supplies heat fluxes at the lower boundary.

The new version of CISM adds a set of basal sliding and calving law
options of varying complexity. The defaults are chosen to support a
stable simulation of the Greenland Ice Sheet (GrIS) and are set to
pseudo-plastic for the sliding law and shelf-free for the calving law.

The model currently has simple treatments of basal hydrology.
More complex schemes for subglacial water hydrology and
evolution of basal till strength are being developed.


Directory structure
-------------------

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

Coupling to CTSM
---------------

GLAD is the new climate model interface of CISM and is responsible for:

1. handling time stepping and temporal averaging.
2. providing a simpler interface to the climate model.
3. translating inputs and outputs into appropriate quantities.

In contrast with the old interface GLINT, GLAD receives already-dowscaled
fields on the ice sheet grid. Consequently GLAD does not do any upscaling,
dowscaling or interpolation. Additionaly it is no longer possible to
use the positive-degree-day (PDD) scheme (while this option was previously
available with GLINT it was unabled for CESM runs).

In general there can be multiple non-overlapping ice sheet grids, but
only Greenland is currently enabled.

GLAD needs to know (1) one or more 2D fields necessary for computing
the surface mass balance, (2) an upper boundary condition, usually
surface temperature.
The computation of the surface mass balance (SMB) for land ice embedded
in CTSM. In this case the required input to GLAD is the SMB itself.
This is the preferred approach for climate-change experiments. The mass
balance is computed for a specified number of elevation classes for each
grid cell on the coarser land grid (~100 km). This is much less
computationally expensive than computing the SMB for each cell on the
finer ice sheet grid (~4 km). Values of 1, 3, 5, 10 and 36 elevation
classes are currently supported, with 10 being the default.

For the SMB scheme, the fields passed to GLAD are (1) the surface mass
balance, *qsmb* (kg/m:sup:`2`/s, positive for ice growing, negative for
ice melting), (2) the surface temperature, *Tsfc* (deg C), and (3) the
surface elevation, *topo* (m) for each elevation class. These fields are
received from the coupler once per simulation day, accumulated and
averaged over the course of a mass balance accumulation time step
(typically one year) and then downscaled to the ice sheet grid. The
downscaling occurs in two phases. First, the values on the global grid
are interpolated in the horizontal to the local ice sheet grid. Next,
for each local grid cell, values are linearly interpolated between
adjacent elevation classes. For example, suppose that at a given
location the coupler supplies a surface mass balance at elevations of
300 and 500 m, whereas the local grid cell has an elevation of 400 m.
Then the local SMB is equal to the mean of the values at 300 and 500 m.

In some parts of the ice sheet grid the fields supplied by CTSM are not
valid, simply because there are no land-covered global grid cells in the
vicinity. For this reason, GLAD computes a mask on the ice sheet grid at
initialization. The mask has a value of 1 for global grid cells that
have a nonzero land fraction (and hence supply valid data) and is zero
otherwise. GLAD then computes a local mask for each grid cell on the
ice sheet grid. The local mask has a value of 1 if one or more of the
four nearest global neighbors supplies valid data (i.e., has a global
mask value of 1). Otherwise, the local mask has a value of zero. In this
case ice sheets are not allowed to exist, and in output files, the SMB
and temperature fields are given arbitrary values, typically zero. This
masking has not been a restriction in practice, since the Greenland ice
sheet does not extend far from the land margin. Alternatives may need to
be considered for modeling the Antarctic ice sheet.

After receiving the surface mass balance, GLAD calls the ice sheet
dynamics model, which returns a new profile of ice sheet area and
extent. The following fields are returned from GLAD to the coupler:
(1) the ice area fraction, *gfrac*, (2) the ice sheet elevation,
*gtopo* (m), (3), the frozen portion of the freshwater runoff, *grofi*,
(4) the liquid portion of the runoff, *grofl*, and (5) the heat flux
from the ice sheet interior to the surface, *ghflx*. These fields
are computed for each elevation class of each grid cell. The frozen
runoff corresponds to iceberg calving and the liquid runoff to basal
meltwater. Surface runoff is not supplied by GLAD because it has already
been computed in CTSM.

There are two modes of coupling CISM to CTSM: one-way and two-way.
For one-way coupling, CISM receives the surface mass balance from CTSM
via the coupler, and the ice sheet extent and thickness evolve accordingly.
However, the land surface topography is fixed, and the fields received
by CTSM from the ice sheet model are ignored. In this case CTSM computes
surface runoff as in earlier versions of CCSM; excess snow is assumed
to run off, and melted ice stays put at the surface. (See Section 4
for more details.) For two-way coupling, the CTSM surface topography
is modified based on input from the ice sheet model. In this case,
surface runoff is computed in a more realistic way; excess snow remains
in place and is converted to ice, and melted ice runs off. In either case,
CTSM computes the surface runoff, which is directed toward the ocean by
the river routing scheme.

Configuring and running the model
---------------------------------

**Timesteps:** There are several kinds of timesteps in CISM.

1. The *forcing timestep* is the interval in hours between calls to
   GLAD. Currently, the forcing timestep is the same as the *coupling
   interval* at which information is passed from the coupler to GLC. The
   forcing timestep is determined by the CISM namelist variables
   *dt\_option* and *dt\_count*. It is 24 hours by default for most
   compsets, but 1 year for TG compsets. Note that these are the only
   values that have been tested extensively; results should be checked
   carefully if the forcing timestep is changed from these defaults.

2. The *mass balance timestep* is the interval over which
   accumulation/ablation forcing data is summed and averaged. This
   timestep is set in subroutine *glad\_mbc\_init* in module
   *glad\_mbal\_coupling.F90*. The current default is one year. With the
   default settings of the forcing timestep and mass balance timestep, GLAD
   will accumulate forcing data from the coupler over 365 daily forcing
   timesteps and average the data. The mass balance timestep must be an
   integer multiple of the forcing timestep.

3. The *ice sheet timestep* is the interval in years between calls to
   the dynamic ice sheet model, GLISSADE. The ice sheet timestep should
   divide evenly into the mass balance timestep. The current default is
   0.1 year for 4-km, and 0.5 year for 20-km.

Two optional runtime parameters can be used to make the time-stepping
more intricate:

1. The mass balance accumulation time, *mbal\_accum\_time* (in years),
   is the period over which mass balance information is accumulated
   before calling GLISSADE. By default, the mass balance accumulation time
   is equal to either the ice sheet timestep or the mass balance
   timestep, whichever is larger. (For current defaults, this means that
   *mbal\_accum\_time* is set equal to the mass balance timestep: 1
   year.) But suppose, for example, that the ice sheet timestep is 5
   years. If we set mbal\_accum\_time = 1.0, we accumulate mass balance
   information for 1 year and use this mass balance to force the ice
   sheet model, thus avoiding 4 additional years of accumulating mass
   balance data. **Note that this parameter cannot currently be modified
   via *user\_nl\_cism*, because it is not recommended that users change
   it.**

2. The timestep multiplier\ *, ice\_tstep\_multiply*, is equal to the
   number of ice sheet timesteps executed for each accumulated mass
   balance field. Suppose that the mass balance timestep is 1 year, the
   ice sheet timestep is 1 year, and ice\_tstep\_multiply = 10. GLAD
   will accumulate and average mass balance information for 1 year, then
   execute 10 ice sheet model timesteps of 1 year each. In other words,
   the ice sheet dynamics is accelerated relative to the land and
   atmosphere. This option may be useful in CESM for multi-millennial
   ice-sheet simulations where it is impractical to run the atmosphere
   and ocean models for more than a few centuries.

These time options (apart from the forcing timestep) are set in
*cism.config*. This file contains (or may contain) the following
timestep information:

1. The ice sheet timestep *dt* (in years) is set in the section
   [*time*\ ] in the ice config file.

2. The mass balance time step is not set directly in the config file,
   but is set to the number of hours in a year (i.e., 8760 hours
   for a 365-day year).

3. The values of *ice\_tstep\_multiply* and *mbal\_accum\_time*, if
   present, are listed in the section [*GLAD climate*\ ].

Note that the total length of the simulation is not determined by
CISM, but is set in the file *env\_run.xml* in the case directory.

**Input/output:** All model I/O is in netCDF format. The *cism.config*
file controls input. Near the end of this file, there is a section
labeled [*CF input*\ ]. This section contains the name of the ice sheet
grid file used for initialization. This file typically includes the ice
thickness and surface elevation, or equivalent information, in each grid
cell. Other information (e.g., internal ice temperature) may be
included; if not, then these fields are set internally by CISM.

Model history frequency is controlled by *HIST\_OPTION* and *HIST\_N* in
*env\_run.xml*. By default, history files are written once a year. Among
the standard fields written to the history file are the ice thickness
(*thk*), upper surface elevation (*usurf*), bedrock elevation (*topg*)
along with the surface mass balance (*acab*) and surface air temperature
(*artm*) downscaled to the ice sheet grid; these fields are set by the
variable *cesm\_history\_vars* in *cism\_in*.

Model restart frequency is coordinated by the CESM coupler. The restart
or hotstart file contains all the fields required for exact restart.
However, the restart will be exact only if the file is written
immediately after an ice dynamics time step. This will normally be the
case for restart files written at the end of any model year.

Many other fields can be written out if desired, simply by adding them
to the variable list, *cesm\_history\_vars*. The source files with names
“\*\_io.F90” specify the fields than can be written out. The easiest way
to write out new variables is to add them to a file ending in “vars.def”
and then rebuild the “\*\_io.F90” files using a python script. The
necessary script can be found in $CASEROOT/Buildconf/cismIOconf. See the
README.cismIO file in that directory for details.

**Grids:**

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

**Simulating the Greenland Ice Sheet:** A primary motivation for having
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

