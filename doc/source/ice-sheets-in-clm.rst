.. _ice-sheets-in-clm:

*****************************************************************
Ice sheets in the Community Land Model, and glacier-land coupling
*****************************************************************

FIXME: This section needs major review and rework. Portions of it could be deleted,
pointing to the appropriate section in the CLM documentation.

This section describes changes made in the Community Land Model to
accommodate ice sheets. For more information, see the CLM
documentation.

================================
 CISM's climate model interface
================================

Glad is the new climate model interface of CISM and is responsible for:

1. handling time stepping and temporal averaging.
2. providing a simpler interface to the climate model.
3. translating inputs and outputs into appropriate quantities.

In contrast with the old interface Glint, Glad receives already-dowscaled
fields on the ice sheet grid. Consequently Glad does not do any upscaling,
dowscaling or interpolation. Also, it is not possible to
use the positive-degree-day (PDD) scheme with Glad. (While this option was previously
available with Glint, it was disabled for CESM runs).

In general there can be multiple non-overlapping ice sheet grids, but
only Greenland is currently enabled.

Glad needs to know (1) one or more 2D fields necessary for computing
the surface mass balance, (2) an upper boundary condition, usually
surface temperature.
The computation of the surface mass balance (SMB) for land ice is embedded
in CTSM. In this case the required input to Glad is the SMB itself.
This is the preferred approach for climate-change experiments. The mass
balance is computed for a specified number of elevation classes for each
grid cell on the coarser land grid (~100 km). This is much less
computationally expensive than computing the SMB for each cell on the
finer ice sheet grid (~4 km). Values of 1, 3, 5, 10 and 36 elevation
classes are currently supported, with 10 being the default.

For the SMB scheme, the fields passed to Glad are (1) the surface mass
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
location the coupler supplies the SMB at elevations of
300 and 500 m, whereas the local grid cell has an elevation of 400 m.
Then the local SMB is equal to the mean of the values at 300 and 500 m.

In some parts of the ice sheet grid the fields supplied by CTSM are not
valid, simply because there are no land-covered global grid cells in the
vicinity. For this reason, Glad computes a mask on the ice sheet grid at
initialization. The mask has a value of 1 for global grid cells that
have a nonzero land fraction (and hence supply valid data) and is zero
otherwise. Glad then computes a local mask for each grid cell on the
ice sheet grid. The local mask has a value of 1 if one or more of the
four nearest global neighbors supplies valid data (i.e., has a global
mask value of 1). Otherwise, the local mask has a value of zero. In this
case ice sheets are not allowed to exist, and in output files, the SMB
and temperature fields are given arbitrary values, typically zero. This
masking has not been a restriction in practice, since the Greenland ice
sheet does not extend far from the land margin. Alternatives may need to
be considered for modeling the Antarctic ice sheet.

After receiving the surface mass balance, Glad calls the ice sheet
dynamics model, which returns a new profile of ice sheet area and
extent. The following fields are returned from Glad to the coupler:
(1) the ice area fraction, *gfrac*, (2) the ice sheet elevation,
*gtopo* (m), (3), the frozen portion of the freshwater runoff, *grofi*,
(4) the liquid portion of the runoff, *grofl*, and (5) the heat flux
from the ice sheet interior to the surface, *ghflx*. These fields
are computed for each elevation class of each grid cell. The frozen
runoff corresponds to iceberg calving and the liquid runoff to basal
meltwater. Surface runoff is not supplied by Glad because it has already
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

================================================
 CLM and the surface mass balance of ice sheets
================================================

The surface mass balance of a glacier or ice sheet is the net annual
accumulation/ablation of mass at the upper surface. Ablation is defined
as the mass of water that runs off to the ocean. Not all the surface
meltwater runs off; some of the melt percolates into the snow and
refreezes. Accumulation is primarily by snowfall and deposition, and
ablation is primarily by melting and evaporation/sublimation.

Two kinds of surface mass balance schemes are widely used in ice sheet
models:

-  positive-degree-day (PDD) schemes, in which the melting is
   parameterized as a linear function of the number of degree-days above
   the freezing temperature. The proportionality factor is empirical and
   is larger for bare ice than for snow.

-  surface-energy-balance (SEB) schemes, in which the melting depends on
   the sum of the radiative, turbulent, and conductive fluxes reaching
   the surface.

The original version of Glimmer had only a PDD scheme. It is
generally believed that PDD schemes are not appropriate for climate
change studies, because empirical degree-day factors could change in a
warming climate (e.g., Bougamont et al. 2007). 
In CESM, the ice-sheet surface mass balance is computed using an SEB
scheme in CLM. Before discussing the scheme, it is useful to describe
CLM’s hierarchical data structure. Each grid cell is divided into one or
more landunits; landunits can be further divided into columns; and
columns can be subdivided into plant functional types, or PFTs. Each
column within a landunit is characterized by a distinct snow/soil or
snow/ice temperature and water profile. PFTs within a column have the
same vertical snow/soil profiles but can have different surface fluxes.
In the current version, landunit areas in each grid cell are fixed at
initialization, but PFT and column areas can evolve during the
simulation.

CLM supports several landunits per grid cell: e.g., soil,
urban, wetland, lake, glacier, and crop. Each of these landunits generally
contains a single column, and soil columns (but not urban, wetland,
lake, and glacier columns) consist of multiple PFTs.
Glacier landunits, however, can be configured to contain multiple columns,
The default for glacier landunits over ice sheets is 10 elevation
classes whose lower limits are 0, 200, 400, 700, 1000, 1300, 1600, 2000,
2500, and 3000 m. Each column is characterized by a fractional area and
surface elevation that are read in during model initialization. The
fractional area and elevation of each column are allowed to evolve
during the run. Each glacier\_mec column within a grid cell has distinct
ice and snow temperatures, snow water content, surface fluxes, and
surface mass balance.

These elevation classes provide a mechanism for downscaling the surface
mass balance from the relatively coarse (~100 km) land grid to the finer
(~5 km) ice sheet grid. The SMB is computed for each elevation class in
each grid cell and is accumulated, averaged, and passed to the GLC
(dynamic ice-sheet) component via the coupler once per day. The mass
balance is downscaled by the coupler, and then passed to Glad on the
ice-sheet grid as described above.

There are several reasons to compute the SMB in CLM rather than CISM:

1. It is much cheaper to compute the SMB in CLM for ~10 elevation
   classes than in CISM. For example, suppose we are running CLM
   at a resolution of ~50 km and Glimmer at ~5 km. Greenland has
   dimensions of about 1000 x 2000 km. For CLM we would have 20 x 40 x
   10 = 8,000 columns, whereas for GLIMMER we would have 200 x 400 =
   80,000 columns.

2. We can use the sophisticated snow physics parameterization already in
   CLM instead of implementing a separate scheme for CISM. Any
   improvements to the CLM are applied to ice sheets automatically.

3. The atmosphere model can respond during runtime to ice-sheet surface
   changes. As shown by Pritchard et al. (2008), runtime albedo feedback
   from the ice sheet is critical for simulating ice-sheet retreat on
   paleoclimate time scales. Without this feedback the atmosphere warms
   much less, and the retreat is delayed.

4. It is easier to conserve mass, given that the rate of surface ice
   growth or melting computed in CLM is equal to the rate seen by the
   dynamic ice sheet model. (Ensuring exact mass conservation
   turns out to be fairly intricate, because of approximations made 
   in interpolating from the CLM grid to the ice-sheet grid.)

==========================================================
 Details of the surface-mass-balance and coupling schemes
==========================================================

FIXME: Much of this section is now obsolete and needs to be rewritten.

When the model is initialized, CLM reads a high-resolution data file
classifying each point as soil, urban, lake, wetland, glacier, or glacier\_mec.
For runs with dynamic ice sheets, the default is to classify all 
glaciated regions as glacier\_mec. If there are no dynamic ice sheets,
then these regions are normally classified as glacier landunits with a 
single column per landunit. Glacier\_mec columns, like glacier columns,
are initialized with a temperature of 250 K. 
While glacier columns are initialized with a snow liquid water equivalent
(LWE) equal to the maximum allowed value of 1 m, glacier\_mec columns
begin with a snow LWE of 0.5 m so that they will reach their equilibrium
mean snow depth sooner. Glacier\_mec columns typically require several
decades of spin-up to equilibrate with a given climate.

Surface fluxes and the vertical temperature profile are computed
independently for each glacier\_mec column. Each column consists of 15
ice layers and up to 5 snow layers, depending on snow thickness. As for
other landunits with a snow cover, surface albedos are computed based on
snow fraction, snow depth, snow age, and solar zenith angle. By default,
the bare ice albedo is prescribed to be 0.60 for visible radiation and
0.40 for near IR; this is lower than the values assumed by CLM for
glacier landunits (0.80 for visible radiation and 0.55 for near IR). The
latter values are higher than those usually assumed by glaciologists.

The atmospheric surface temperature, potential temperature, specific
humidity, density, and pressure are downscaled from the mean gridcell
elevation to the glacier\_mec column elevation using a specified lapse
rate (typically 6.0 deg/km) and an assumption of uniform relative
humidity. At a given time, lower-elevation columns can undergo surface
melting while columns at higher elevations remain frozen. This gives a
more accurate simulation of summer melting, which is a highly nonlinear
function of air temperature. The precipitation rate and radiative fluxes
are not currently downscaled, but could be in the future if care were
taken to preserve the cell-integrated values.

CLM has a somewhat unrealistic treatment of accumulation and melting for
glacier landunits. The snow depth is limited to a prescribed depth of 1
m liquid water equivalent, with any additional snow assumed to run off
to the ocean. (This amounts to a crude parameterization of iceberg
calving.) Snow melting is treated in a realistic fashion, with meltwater
percolating downward through snow layers as long as the snow is
unsaturated. Once the underlying snow is saturated, any additional
meltwater runs off. When glacier ice melts, however, the meltwater is
assumed to remain in place until it refreezes. In warm parts of the ice
sheet, the meltwater does not refreeze, but stays in place indefinitely.

In the modified CLM with glacier\_mec columns, snow in excess of the
prescribed maximum depth is assumed to turn into ice, contributing a
positive surface mass balance to the ice sheet model. Melting ice is
assumed to run off to the ocean, giving a negative surface mass balance.
The net SMB associated with ice formation (by conversion from snow) and
melting/runoff is computed for each column, averaged over the coupling
interval, and sent to the coupler. This quantity, denoted *qice*, is
then passed via the coupler to Glint, along with the surface elevation
*topo* in each column. Glint downscales the SMB (renamed as *qsmb*) to
the local elevation on the ice sheet grid, interpolating between values
in adjacent elevation classes. The units of *qice* are mm/s, or
equivalently km/m\ :sup:`2`/s. If desired, the downscaled quantities can
be multiplied by a normalization factor to conserve mass globally. (This
normalization is not yet implemented.)

Note that the surface mass balance typically is defined as the total
accumulation of ice and snow, minus the total ablation. The *qice* flux
passed to Glint is the mass balance for ice alone, not snow. We can
think of CLM as owning the snow, whereas Glimmer owns the underlying
ice. Fluctuations in snow depth between 0 and 1 m LWE are not reflected
in the SMB passed to Glint.

In addition to *qice* and *topo*, the ground surface temperature *tsfc*
is passed from CLM to Glint via the coupler. This temperature serves as
the upper boundary condition for Glimmer-CISM’s temperature calculation.

Given the SMB from the land model, Glimmer-CISM executes one or more
dynamic time steps and then has the option to upscale the new ice sheet
geometry to the global grid and return it to CLM via the coupler. The
fields passed to the coupler for each elevation class are the ice sheet
fractional area (*gfrac*), surface elevation (*gtopo*), liquid (basal
meltwater) runoff *grofl*, frozen (calving) runoff *grofi*, and surface
conductive heat flux *ghflx*.

The current coupling is one-way only. That is, CLM sends the SMB and
surface temperature to Glint but does not do anything with the fields
that are returned. The CLM surface topography is therefore fixed in
time. One-way coupling is reasonable for runs of ~100 years or less, in
which ice-sheet elevation changes are modest. For longer runs with
larger elevation changes, two-way coupling is highly desirable. A
two-way coupling scheme is under development.

================
 Model controls
================

The number of elevation classes is determined by the variable *GLC\_NEC*
in the file *env\_run.xml* in the case directory. Values of 1, 3, 5, 10
and 36 elevation classes are currently supported by the code, with 10
classes being the default. **However, running with anything other than
10 elevation classes will require that you create your own surface
dataset – see the relevant question in the Frequently Asked Questions
section, below. Furthermore, there is currently a bug in CLM that
prevents running the glacier\_mec code with 1 elevation class.** (This
bug is in the setting of the elevation of virtual columns in
subgridMod.F90.)

The array *glc\_topomax*, which is read from CLM's surface dataset (set
by models/lnd/clm/tools/mksurfdata\_map/src/mkglcmecMod.F90), defines
the maximum elevation (in meters) in each class. For 10 elevation
classes, glc\_topomax is set to (0, 200, 400, 700, 1000, 1300, 1600,
2000, 2500, 3000, 10000). Note that this array must also agree with the
*topomax* array set in CISM, in *glint\_type.F90.*

At initialization, CLM's surface dataset specifies the areal percentage
of each grid cell classified as wetland, vegetation, lake, urban,
glacier, or glacier\_mec. For glacier\_mec cells, the area and surface
elevation are specified in each elevation class. The glacier\_mec area
in a given grid cell is fixed; glacier\_mec landunits cannot change to
vegetated landunits or vice versa. This restriction will be relaxed in
future model releases.

The fundamental control variable is *create\_glacier\_mec\_landunit*, a
logical variable declared in *clm\_varctl.F90*. It is false by default,
but is automatically set to true when we create a case that includes a
dynamic ice sheet component (e.g., IG, FG, or BG). If
*create\_glacier\_mec\_landunit* = T, the following occurs:

-  Memory is allocated for the areal percentage (*pct\_glcmec*) and
   surface elevation (*topo\_glcmec*) in each elevation class, and these
   values are read in from the surface dataset. The sum of *pct\_glcmec*
   in each grid cell is checked to make sure it agrees with *pctgla*,
   the total glaciated fraction in each grid cell.

-  Glacier\_mec landunits and columns are defined for all grid cells
   where either (1) the fractional glacier area is greater than zero or
   (2) the dynamic ice sheet model may require a surface mass balance,
   even if CLM does not have glacier landunits in that location. To
   allow for case (2), grid overlap files have been precomputed. For
   given resolutions of CLM and Glimmer-CISM, these files identify all
   land-covered grid cells that overlap any part of the ice sheet grid.
   In these overlapping cells, glacier\_mec columns are defined in all
   elevation classes. Some columns may have zero area and are called
   “virtual” columns. These columns do not affect energy exchange
   between the land and the atmosphere, but are included for potential
   forcing of Glimmer-CISM.

The logical variable *glc\_smb* determines what kind of information is
passed from CLM to the ice sheet model via the coupler. If *glc\_smb* is
true, then the surface mass balance is passed. Specifically, *qice* is
interpreted by the ice sheet model as a flux (kg/m:sup:`2`/s) of ice
freezing/melting. If *glc\_smb* is false, then the ice sheet model
should compute the surface mass balance using a positive-degree-day
scheme, with *qice* interpreted as the precipitation and *tsfc* as the
2-m air temperature. (However, the PDD option is not currently
supported.) In either case, *tsfc* is downscaled and applied as the
upper boundary condition for the dynamic ice sheet.

The logical variable *glc\_dyntopo* controls whether CLM surface
topography changes dynamically as the ice sheet evolves (i.e., whether
the coupling is one-way or two-way). The default (and the only option
currently supported) is *glc\_dyntopo* = F, in which case the land
topography is fixed. In this case the surface runoff for glacier\_mec
landunits is computed as for glacier landunits: (1) Any snow in excess
of 1 m LWE runs off to the ocean, and (2) Melted ice remains in place
until it refreezes. Excess snow and melted ice still contribute to
positive and negative values, respectively, of *qice*, but only for the
purpose of forcing Glimmer-CISM.

If *glc\_dyntopo* = T, then CLM receives updated topographic information
from the ice sheet model. In this case the CLM surface runoff is
computed in a more realistic way: (1) Any snow in excess of 1 m LWE is
assumed to turn to ice and does not run off. (2) Melted ice runs off.

Two physical parameters may be useful for tuning the surface mass
balance: (1) the surface bare ice albedo, *albice*, which is set in
SurfaceAlbedoMod.F90, and (2) the surface air temperature lapse rate,
*lapse\_glcmec*, which is used for downscaling temperature and is set in
*clm\_varcon.F90*. By default, the bare ice albedo is 0.80 for visible
wavelengths and 0.55 for near IR, but for glacier\_mec columns the bare
ice albedo is automatically changed to 0.60 / 0.40 (for the two
wavelengths) in the namelist. The default lapse rate is 6.0 deg/km.

The snow albedo is not easily tunable. It is computed in a complicated
way based on snow fraction, snow depth, snow age, and solar zenith
angle. Snow albedo in glacier\_mec columns is treated identically to
snow in other landunits.

Another possible tuning mechanism is to convert rain to snow and vice
versa as a function of surface temperature. This conversion would
violate energy conservation, but might give more realistic precipitation
fields in columns with elevations much higher or lower than the gridcell
mean.

The default values of *albice*, *create\_glacier\_mec\_landunit*,
*glc\_smb*, and *glc\_dyntopo* may each be overwritten by specifying the
desired values in the namelist. This is done automatically for *albice*
and *create\_glacier\_mec\_landunit* when a case is created with dynamic
ice sheets.

==============================================
 CLM surface datasets used for runs with CISM
==============================================

**The default CLM glacier cover differs depending on whether you are
using CLM4.0 or CLM4.5.**

For CLM4.0: When running a compset with CISM present (i.e., an IG, FG or
BG compset, for which CLM will use multiple elevation classes), the
default CLM surface dataset uses a newer dataset to specify glacier
coverage, compared with CLM runs in the absence of CISM. In most places,
the new dataset is based on the Randolph Glacier Inventory (Arendt et
al. 2012). Over Greenland, however, the surface dataset uses data from
CISM's *gland5UM* initialization file, so that CLM and CISM give
consistent glacier coverage at initialization. These new glacier
coverage fields are described in more detail in the CESM1.1 addendum to
the CLM4 tech note.

For CLM4.5: Existing surface datasets were generated using the Randolph
Glacier Inventory (Arendt et al. 2012) everywhere. In contrast to
CLM4.0, CISM’s Greenland glacier cover is *not* used for existing CLM4.5
surface datasets.

See Section 6.2 for information on generating your own surface datasets.
For example, that section provides guidance on creating surface datasets
for CLM4.5 that are consistent with CISM over Greenland.

