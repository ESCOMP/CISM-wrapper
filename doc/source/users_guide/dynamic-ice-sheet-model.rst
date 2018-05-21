.. _dynamic-ice-sheet-model:

******************************
 The dynamic ice sheet model
******************************

This section gives a brief overview of ice flow modeling and of
Glimmer-CISM, the dynamic ice sheet model in CESM. For more details,
including a technical description of the model, please see Rutt et al.
(2009) and the Glimmer-CISM documentation. The documentation is slightly
out of date but still provides a useful description of Glimmer-CISM’s
dynamical core (GLIDE) and climate model interface (GLINT). Updated
documentation will be provided with the release of Glimmer-CISM 2.0 in
2013.

=======================
 Equations of ice flow
=======================

Here we give a short overview of the equations of ice flow. For more
details, see, e.g., Greve and Blatter (2009) and the Glimmer-CISM
documentation.

An ice sheet is typically modeled as an incompressible, heat-conducting,
viscous, non-Newtonian fluid. The basic field equations can be written
as (e.g., Pattyn 2003)

.. math::
   :label: basic1

   \nabla \bullet \boldsymbol{u} = 0

.. math::
   :label: basic2

   \rho \frac{\boldsymbol{du}}{dt} = \nabla \bullet \boldsymbol{\sigma} + \rho\boldsymbol{g} 

.. todo:: finish the above the equations 

where is the material derivative, is the ice density, is the 3D
velocity, is the temperature in degrees Celsius, is the gravitational
acceleration, is the stress tensor, is the specific heat of ice, is the
thermal conductivity, and is the deformational heat source. These three
equations express conservation of mass, linear momentum, and energy,
respectively. The continuity equation implies that glacier ice is
incompressible.

In Cartesian coordinates with , the continuity equation becomes

.. todo:: enter equation (4)

where . The momentum equations are

.. todo:: enter equations (5, 6, 7)

where is the force per unit area in the direction on the plane normal to
the direction, and acceleration terms (which are small for ice sheets)
have been neglected. Equations – are known as the Stokes equations.
Since the stress tensor is symmetric (ensuring conservation of angular
momentum), only six of the nine components are independent. It is
convenient to write as

.. todo:: enter equations (8)

where is the stress deviator tensor, is the Kronecker delta, and is the
static pressure, defined as

.. todo:: enter equations (9)

This definition implies that , since the three normal stresses are
negative for an ice sheet at rest. The stress deviator tensor is
traceless: .

The components of are related to the strain rate by means of a
constitutive law. The standard constitutive law is Glen’s flow law:

.. todo:: enter equations (10)

where the strain rate tensor is the symmetric part of the tensor :

.. todo:: enter equations (11)

Equation implies that , like , is traceless. The effective stress is a
function of the second invariant of the stress deviator tensor and may
be written as

.. todo:: enter equations (12)

The exponent of is usually chosen as . The rate factor is typically
computed using an Arrhenius relation (Payne et al. 2000):

.. todo:: enter equations (13)

where is a proportionality constant, is the activation energy for creep,
is the universal gas constant, and is the absolute temperature corrected
for the dependence of the melting point on pressure. Often it is
desirable to express the deviatoric stress in terms of the strain rate.
Using the relation , equation can be inverted to give

.. todo:: enter equations (14)

where . This expression is of the standard form for a viscous fluid,

.. todo:: enter equations (15)

where is the effective viscosity.

Using –, the Stokes equations – together with the continuity equation
can be written as a system of four coupled equations with four unknowns:
*u*, *v*, *w*, and *p*. Since these equations are hard to solve, most
numerical ice sheet models solve the momentum equation in approximate
form. For example, Pattyn (2003) neglects the first two terms on the LHS
of and uses a hydrostatic approximation,

.. todo:: enter equations (16)

to eliminate . After some algebraic manipulation, the resulting momentum
equations are

.. todo:: enter equations (17)

.. todo:: enter equations (18)

where is the surface elevation. This is a set of two coupled equations
in two unknowns, *u* and *v*, which are easier to solve than the Stokes
equations. Once *u* and *v* are determined, *w* and *p* are found using
the continuity equation (given that at the lower boundary) and the
hydrostatic relation.

Equations – are often referred to as a “higher-order” approximation of
the Stokes equations. Other higher-order approximations exist; for
example, Schoof and Hindmarsh (2010) used an additional simplification
to obtain a vertically averaged higher-order model. In this model, *u*
and *v* are solved in a single layer (rather than three dimensions as in
the Pattyn model), and the velocities at other elevations are found by
vertical integration.

Two lower-order approximations are widely used. The most common is the
shallow-ice approximation (SIA), in which vertical shear stresses are
assumed to be dominant, and lateral and longitudinal stresses are
neglected. In other words, the third term on the LHS of and is assumed
to be much larger than the first two terms. The SIA is valid in the
slow-moving interior of ice sheets, where basal sliding is small and the
motion is dominated by vertical shear. Another common approximation is
the shallow-shelf approximation (SSA), in which lateral and longitudinal
stresses are assumed to dominate, and vertical shear stresses are
ignored. The SSA is valid for floating ice shelves, where the basal
shear stress is negligible and there is little or no vertical shear. The
SSA is sometimes used in modified form to treat flow in regions of rapid
sliding, such as ice streams, where the basal shear stress is small but
nonzero (e.g., MacAyeal 1989).

=======================================
 The Glimmer Community Ice Sheet Model
=======================================

Glimmer-CISM is a thermomechanical ice sheet model that solves the
equations of ice flow, given suitable approximations and boundary
conditions. The source code is written primarily in Fortran 90 and 95.
The model resides on the BerliOS repository
(http://glimmer-cism.berlios.de/), where it is under active development.
The version targeted for CESM is developed on an NCAR Subversion
repository (https://svn-cism-model.cgd.ucar.edu/). The ice-sheet model
in CESM consists primarily of source code from Glimmer-CISM version 1.9.

The dynamical core of the model, known as GLIDE, solves equations – for
the conservation of mass, momentum, and internal energy. The version of
GLIDE currently in CESM uses the shallow-ice approximation. However, a
higher-order model is under development and will be included in future
releases.

The surface boundary conditions (e.g., the surface temperature and
surface mass balance) are supplied by a climate driver. When
Glimmer-CISM is run in CESM, the climate driver is GLINT, which receives
the temperature and SMB from the coupler and downscales them to the
ice-sheet grid. The lower boundary conditions are given by an isostasy
model, which computes the elevation of the lower surface, and by a
geothermal model, which supplies heat fluxes at the lower boundary.

The model currently has simple treatments of basal hydrology and
sliding. More complex schemes for subglacial water hydrology and
evolution of basal till strength are being developed. Glimmer-CISM also
provides several simple schemes for calving at the margins; these will
be replaced by more realistic lateral boundary conditions in the future.

For a detailed description of Glimmer-CISM’s dynamical core and software
design, see Rutt et al. (2009) and the latest model documentation.

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

Coupling to CLM
---------------

GLINT, the climate model interface of Glimmer-CISM, is designed to
accumulate, average, and downscale fields received from other climate
model components. These fields are interpolated from a global grid to
the individual ice sheet grid(s). In general there can be multiple
non-overlapping ice sheet grids, but only Greenland is currently
enabled. The global grid must be a regular lat-lon grid, but the
latitudes need not be equally spaced. For CESM the global grid is
assumed to be the same as the CLM grid.

GLINT needs to know (1) one or more 2D fields necessary for computing
the surface mass balance, (2) an upper boundary condition, usually
surface temperature, and (3) the latitudes and longitudes of the grid
cells where these fields are defined. There are two general ways of
computing the surface mass balance:

1. a positive-degree-day (PDD) scheme, either annual or daily, for which
   the required inputs to GLINT are the 2-m air temperature and the
   precipitation. This is the default scheme for Glimmer-CISM, but it
   may not be appropriate for climate change studies. The PDD option is
   not currently enabled for CESM runs

2. a surface-mass-balance (SMB) scheme for land ice embedded in CLM. In
   this case the required input to GLINT is the SMB itself. This is the
   preferred approach for climate-change experiments. The mass balance
   is computed for a specified number of elevation classes for each grid
   cell on the coarser land grid (~100 km). This is much less
   computationally expensive than computing the SMB for each cell on the
   finer ice sheet grid (~10 km). Values of 1, 3, 5, 10 and 36 elevation
   classes are currently supported, with 10 being the default.

For the SMB scheme, the fields passed to GLINT are (1) the surface mass
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

In some parts of the ice sheet grid the fields supplied by CLM are not
valid, simply because there are no land-covered global grid cells in the
vicinity. For this reason, GLINT computes a mask on the global grid at
initialization. The mask has a value of 1 for global grid cells that
have a nonzero land fraction (and hence supply valid data) and is zero
otherwise. GLINT then computes a local mask for each grid cell on the
ice sheet grid. The local mask has a value of 1 if one or more of the
four nearest global neighbors supplies valid data (i.e., has a global
mask value of 1). Otherwise, the local mask has a value of zero. In this
case ice sheets are not allowed to exist, and in output files, the SMB
and temperature fields are given arbitrary values, typically zero. This
masking has not been a restriction in practice, since the Greenland ice
sheet does not extend far from the land margin. Alternatives may need to
be considered for modeling the Antarctic ice sheet.

After downscaling the surface mass balance to the ice sheet grid, GLINT
calls the ice sheet dynamics model, which returns a new profile of ice
sheet area and extent. The following fields can be upscaled to the
global grid and returned from GLINT to the coupler: (1) the ice area
fraction, *gfrac*, (2) the ice sheet elevation, *gtopo* (m), (3), the
frozen portion of the freshwater runoff, *grofi*, (4) the liquid portion
of the runoff, *grofl*, and (5) the heat flux from the ice sheet
interior to the surface, *ghflx*. These fields are computed for each
elevation class of each grid cell. The frozen runoff corresponds to
iceberg calving and the liquid runoff to basal meltwater. Surface runoff
is not supplied by GLINT because it has already been computed in CLM.
Upscaling is not enabled in the current release but will be included in
the near future.

There are two modes of coupling Glimmer-CISM to CLM: one-way and
two-way. For one-way coupling, Glimmer-CISM receives the surface mass
balance from CLM via the coupler, and the ice sheet extent and thickness
evolve accordingly. However, the land surface topography is fixed, and
the fields received by CLM from the ice sheet model are ignored. In this
case CLM computes surface runoff as in earlier versions of CCSM; excess
snow is assumed to run off, and melted ice stays put at the surface.
(See Section 4 for more details.) For two-way coupling, the CLM surface
topography is modified based on input from the ice sheet model. In this
case, surface runoff is computed in a more realistic way; excess snow
remains in place and is converted to ice, and melted ice runs off. In
either case, CLM computes the surface runoff, which is directed toward
the ocean by the river routing scheme. Only one-way coupling is
currently enabled, but two-way coupling is under development and will be
released in 2014.

Configuring and running the model
---------------------------------

**Timesteps:** There are several kinds of timesteps in Glimmer-CISM.

1. The *forcing timestep* is the interval in hours between calls to
   GLINT. Currently, the forcing timestep is the same as the *coupling
   interval* at which information is passed from the coupler to GLC. The
   forcing timestep is determined by the CISM namelist variables
   *dt\_option* and *dt\_count*. It is 24 hours by default for most
   compsets, but 1 year for TG compsets. Note that these are the only
   values that have been tested extensively; results should be checked
   carefully if the forcing timestep is changed from these defaults.

2. The *mass balance timestep* is the interval over which
   accumulation/ablation forcing data is summed and averaged. This
   timestep is set in subroutine *glint\_mbal\_init* in module
   *glint\_mbal.F90*. The current default is one year. With the default
   settings of the forcing timestep and mass balance timestep, GLINT
   will accumulate forcing data from the coupler over 365 daily forcing
   timesteps and average the data before downscaling it to the local ice
   sheet grid. The mass balance timestep must be an integer multiple of
   the forcing timestep.

3. The *ice sheet timestep* is the interval in years between calls to
   the dynamic ice sheet model, GLIDE. The ice sheet timestep should
   divide evenly into the mass balance timestep. The current default is
   0.05 year for 5-km, and 0.1 year for 10-km and 20-km.

Two optional runtime parameters can be used to make the time-stepping
more intricate:

1. The mass balance accumulation time, *mbal\_accum\_time* (in years),
   is the period over which mass balance information is accumulated
   before calling GLIDE. By default, the mass balance accumulation time
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
   ice sheet timestep is 1 year, and ice\_tstep\_multiply = 10. GLINT
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
   but is related to the accumulation/ablation mode, *acabmode*, which
   is set in the *section* [*GLINT climate*\ ]. If *acabmode = 0* (the
   default value for CESM runs), then the mass balance time step is set
   to the number of hours in a year (i.e., 8760 hours for a 365-day
   year).

3. The values of *ice\_tstep\_multiply* and *mbal\_accum\_time*, if
   present, are listed in the section [*GLINT climate*\ ].

See the Glimmer-CISM documentation for more details.

Note that the total length of the simulation is not determined by
Glimmer-CISM, but is set in the file *env\_run.xml* in the case
directory.

**Input/output:** All model I/O is in netCDF format. The *cism.config*
file controls input. Near the end of this file, there is a section
labeled [*CF input*\ ]. This section contains the name of the ice sheet
grid file used for initialization. This file typically includes the ice
thickness and surface elevation, or equivalent information, in each grid
cell. Other information (e.g., internal ice temperature) may be
included; if not, then these fields are set internally by Glimmer-CISM.

Model history frequency is controlled by *HIST\_OPTION* and *HIST\_N* in
*env\_run.xml*. By default, history files are written once a year. Among
the standard fields written to the history file are the ice thickness
(*thk*), upper surface elevation (*usurf*), temperature (*temp*), and
velocity (*uvel*, *vvel*) fields, along with the surface mass balance
(*acab*) and surface air temperature (*artm*) downscaled to the ice
sheet grid; these fields are set by the variable *cesm\_history\_vars*
in *cism\_in*.

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

**Grids:** GLINT can downscale fields from any global lat/lon grid. The
latitudes need not be equally spaced. Three global grid resolutions are
currently supported: T31 (spectral), FV2 (~2:sup:`o` finite-volume), and
FV1 (~1:sup:`o` finite-volume). The global resolution (i.e., the
resolution of the land and atmosphere) is set when a case is created.

Local ice sheet grids must be rectangular; typically they are polar
stereographic projections. For Greenland, three grids are currently
supported, with resolutions of 20 km, 10 km, and 5 km, respectively.
Each local grid is compatible with any of the three supported global
resolutions. There are two versions of the 5 km grid – *gland5* and
*gland5UM* – which provide different data for initializing the model.
The current default is the *gland5UM* 5 km grid. This can be changed by
modifying CISM\_GRID to the desired value (gland5, gland10 or gland20)
in env\_run.xml. A number of configuration defaults depend on the grid.
You can see the effect of changing *CISM\_GRID* by running
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
inaccurate; (2) Glimmer-CISM currently uses the shallow-ice
approximation, which is not accurate for fast outlet glaciers; and (3)
the present-day GrIS may not be in steady-state with the present-day (or
preindustrial) climate. Our working hypotheses are that (1) If the SMB
is reasonably accurate, we can obtain a reasonable large-scale thickness
and extent for the GIS; (2) With a higher-order dynamics scheme and some
judicious tuning, we can generate ice streams and outlet glaciers in the
right locations with realistic velocities; and (3) The present-day GrIS
is not far from steady-state with the preindustrial climate. These
hypotheses are now being tested. Early results are reported in a series
of papers (Lipscomb et al. 2012; Vizcaíno et al. 2012a and 2012b)
submitted to the CESM special issue of the *Journal of Climate*.

Obtaining an accurate surface mass balance may require some tuning in
CLM; see Section 5 for details. We are also experimenting with different
dynamics settings in the ice config file. The current default settings
may not be optimal. The config files will be updated when we have more
experience in running the model.

