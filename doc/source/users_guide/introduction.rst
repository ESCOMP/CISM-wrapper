.. _introduction:

*****************
Introduction
*****************

This document accompanies the Community Earth System Model (CESM) User’s
Guide and is intended for users who would like to run CESM with dynamic
ice sheets and/or an improved surface mass balance scheme for glaciated
regions. For more information, please see the CESM User’s Guide:
http://www.cesm.ucar.edu/models/cesm1.2/.

The introduction provides some scientific background, along with a brief
history of land-ice model development within CESM. Section 2 is a
quick-start guide for new users. Section 3 describes how to run the
standalone ice sheet model within CESM, forced by output from a
previous, coupled run. Section 4 describes Glimmer, the Community Ice
Sheet Model (Glimmer-CISM), the dynamic ice sheet model in CESM. Section
5 gives a detailed description of the surface-mass-balance scheme for
ice sheets in the Community Land Model (CLM). Section 6 answers some
common questions about model usage.

It should be emphasized that this is an early implementation with a
number of scientific limitations that are detailed below. Model
developers are keenly aware of these limitations and are actively
addressing them. Several major improvements are planned for the next one
to two years and will be released as they become available.

This documentation is itself in progress. If you find errors, or if you
would like to have some additional information included, please contact
the authors at lipscomb@lanl.gov or sacks@ucar.edu.

=======================
 Scientific background
=======================

Historically, ice sheet models were not included in global climate
models (GCMs), because they were thought to be too sluggish to respond
to climate change on decade-to-century time scales. In the Community
Climate System Model (CCSM), as in many other global climate models, the
extent and elevation of the Greenland and Antarctic ice sheets were
assumed to be fixed in time. Interactions between ice sheets and other
parts of the climate system were largely ignored.

Recent observations, however, have established that the Greenland and
Antarctic ice sheets can respond to atmospheric and ocean warming on
time scales of a decade or less. Satellite gravity measurements have
shown that both ice sheets are losing mass at a rate of ~200 Gt/yr
(Velicogna 2009). (A mass loss of 360 Gt corresponds to global sea-level
rise of 1 mm.) Greenland mass loss is caused by increased surface
melting and the acceleration of large outlet glaciers (van den Broeke et
al. 2009). In Antarctica, mass is being lost primarily because of the
acceleration of outlet glaciers, especially in the Amundsen Sea
Embayment of West Antarctica (Rignot et al. 2008).

Small glaciers and ice caps (GIC) also have retreated in recent years.
Although the total volume of GIC (~0.6 m sea-level equivalent; Radić and
Hock 2010) is much less than that of the Greenland ice sheet (~7 m) and
the Antarctic ice sheet (~60 m), glaciers and ice caps can respond
quickly to climate change. Mass loss from GIC has grown in recent years
and has been estimated at ~400 Gt/yr (Meier et al. 2007). GCMs generally
assume that the mass of glaciers and ice caps, like that of ice sheets,
is fixed.

Global sea level is rising at a rate of about 30 cm/century, with
primary contributions from land ice retreat and ocean thermal expansion
(Cazenave et al. 2008). Estimates of 21\ :sup:`st` century ice-sheet
mass loss and sea-level rise are highly uncertain. The IPCC Fourth
Assessment Report (Meehl et al. 2007) projected 18 to 59 cm of sea-level
rise by 2100 but specifically excluded ice-sheet dynamical feedbacks, in
part because existing ice sheet models were deemed inadequate. A widely
cited semi-empirical study (Rahmstorf 2007) estimated 40 to 150 cm of
21\ :sup:`st` century sea-level rise, based on the assumption that the
rate of rise is linearly proportional to the increase in global mean
temperatures from preindustrial values. The assumptions of
semi-empirical models, however, may not be valid as additional land-ice
processes come into play.

Modeling of land ice has therefore taken on increased urgency. Several
workshops (e.g., Little et al. 2007; Lipscomb et al. 2009) have called
for developing improved ice sheet models. There is general agreement on
the need for (1) “higher-order” flow models with a unified treatment of
vertical shear stresses and horizontal-plane stresses, (2) finer grid
resolution (~5 km or less) for ice streams, outlet glaciers, and other
regions where the flow varies rapidly on small scales, and (3) improved
treatments of key physical processes such as basal sliding, subglacial
water transport, iceberg calving, and grounding-line migration. These
improvements are beginning to be incorporated in numerical ice sheet
models. One such model is the Glimmer Community Ice Sheet Model
(Glimmer-CISM), which has been coupled to CESM and is described below.

Although much can be learned from ice sheet models in standalone mode,
coupled models are required to capture important feedbacks. For example,
surface ablation may be underestimated if an ice sheet model is forced
by an atmospheric model that does not respond to changes in surface
albedo and elevation (Pritchard et al. 2008). At ice sheet margins,
floating ice shelves are closely coupled to the ocean in ways that are
just beginning to be understood and modeled (Holland et al. 2008a,
2008b). Also, changes in ice sheet elevation and surface runoff could
have significant effects on the regional and global circulation of the
atmosphere and ocean. The inclusion of dynamic ice sheets in CESM and
other GCMs will likely lead to scientific insights that cannot be
obtained from standalone ice sheet models.

====================
 Ice sheets in CESM
====================

Since 2006, researchers in the Climate, Ocean and Sea Ice Modeling
(COSIM) group at Los Alamos National Laboratory (LANL) have worked with
scientists at the National Center for Atmospheric Research (NCAR) to
incorporate an ice sheet model in the CCSM/CESM framework. This work has
been funded primarily by the Department of Energy (DOE) Scientific
Discovery through Advanced Computing (SciDAC) program, with additional
support from the National Science Foundation (NSF). The Glimmer ice
sheet model (Rutt et al. 2009), developed by Tony Payne and colleagues
at the University of Bristol, was chosen for coupling. Although
Glimmer’s dynamical core was relatively basic, a higher-order dynamics
scheme was under development. In addition, the model was well structured
and well documented, with an interface (GLINT) to enable coupling to
GCMs.

Glimmer was initially coupled to CCSM version 3.5. The surface mass
balance (SMB; the difference between annual accumulation and ablation)
was computed using Glimmer’s positive-degree-scheme, which uses
empirical formulas to relate surface temperatures to summer melting. It
was decided that the PDD scheme was not appropriate for climate change
modeling, because empirical relationships that are valid for present-day
climate may not hold in the future. Instead, a surface-mass-balance
scheme for ice sheets was developed for the Community Land Model (CLM).
This scheme computes the SMB in each of ~10 elevation classes per grid
cell in glaciated regions. The SMB is passed via the coupler to the ice
sheet component, where it is averaged, downscaled, and used to force the
dynamic ice sheet model at the upper surface. (See Section 4 for
details.) When the CCSM4 coupling framework became available, the
coupling was redone for the new framework.

In 2009, the U.K. researchers who designed Glimmer joined efforts with
U.S. scientists who were developing a Community Ice Sheet Model (CISM),
and the model was renamed Glimmer-CISM. Model development was overseen
by a six-member steering committee including Magnus Hagdorn (U.
Edinburgh), Jesse Johnson (U. Montana), William Lipscomb (LANL), Tony
Payne (U. Bristol), Stephen Price (LANL), and Ian Rutt (U. Swansea). The
model resides on the BerliOS repository
(http://glimmer-cism.berlios.de/). It is an open-source code governed by
the GNU General Public License and is freely available to all. The
version included in the initial CESM release is a close approximation of
Glimmer-CISM version 1.6.

Subsequent ice-sheet model development targeted for CESM has taken place
in a Subversion repository at the National Center for Atmospheric
Research (https://svn-cism-model.cgd.ucar.edu/). CESM1.2 includes a new
version of Glimmer-CISM (version 1.9). This version includes extensive
modifications that have been made in preparation for the upcoming
release of Glimmer-CISM version 2.0, which will include 3d,
1st-order-accurate ice dynamics, support for MPI-based distributed
parallelism, and support for linking to Trilinos solver libraries. When
run with shallow-ice dynamics, Glimmer-CISM 1.9 gives answers that are
approximately the same as version 1.6, which was the version included in
CESM1.1.1 and earlier.

=============
 Limitations
=============

There are a number of significant limitations of the ice sheet model
within CESM. Most of these are under active development by members of
the Land Ice Working Group.

Limitations of the ice sheet model
----------------------------------

-  The model is technically supported but is still undergoing scientific
       testing and validation. We cannot guarantee that the default
       values of model parameters will yield an optimal simulation.

-  The dynamical core is similar to that in the original Glimmer code
       and is based on the shallow-ice approximation (SIA). The SIA is
       valid in the interior of ice sheets, but not in fast-flowing
       regions such as ice shelves, ice streams, and outlet glaciers. A
       higher-order scheme that is valid in all parts of the ice sheet
       is being tested and will become part of CESM in 2014 with the
       release of Glimmer-CISM version 2.0.

-  The current Glimmer-CISM code is serial. This is not a limitation for
       the SIA model, which is computationally fast, but will be an
       issue for the higher-order model. A parallel version of the code
       is under development and is expected to be released by 2014.

-  Glimmer-CISM simulates only the large ice sheets (Greenland and
       Antarctica). There is currently no ability in CESM to simulate
       evolution of smaller glaciers. A separate model for simulating
       smaller glaciers is under development and is expected to be
       released in 2014.

Limitations of other components of the CESM modeling system
-----------------------------------------------------------

- There is no out-of-the-box capability for topography in the atmosphere model (CAM) to
  respond to changes in ice sheet geometry. (However, members of the Land Ice Working
  Group have developed an offline script that can be used for these purposes.)

-  The ice sheet model has not been fully coupled to the ocean model; that
       coupling is under development. For this reason the initial
       implementation is for the Greenland ice sheet only. Since
       ice-ocean coupling is critical for the dynamics of the Antarctic
       ice sheet, it was decided that Antarctic simulations without
       ocean coupling would be of limited scientific value.

-  The division of glaciers into elevation classes in CLM is fairly
       coarse and static in space. The current scheme is reasonable for
       Greenland, but not ideal for mountain glaciers.

What’s new in CESM1.2 with respect to ice sheet modeling?
---------------------------------------------------------

Compared to the CESM1.1 series, there have been a number of improvements
in CESM that are relevant for ice sheet modeling:

-  CESM1.2 includes a new version of Glimmer-CISM (version 1.9). This
   version includes extensive modifications that have been made in
   preparation for the upcoming release of Glimmer-CISM version 2.0,
   which will include 3d, 1st-order-accurate ice dynamics and support
   for MPI-based distributed parallelism. When run with shallow-ice
   dynamics, Glimmer-CISM 1.9 gives answers that are approximately the
   same as version 1.6, which was the version included in CESM1.1.1 and
   earlier.

-  Some changes to default parameter values: For a few configuration
   settings, the numeric value corresponding to each option has changed
   (see
   `http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/nl\_cism.html <http://www.cesm.ucar.edu/models/cesm1.1/cesm/doc/modelnl/nl_cism.html>`__).
   In addition, there have been additional configuration options added
   (the first two below) and changes to some existing default option
   settings:

   -  *temp\_init*

      -  Old: 1 (Initialize temperature to surface air temperature)

      -  New: 2 (Initialize temperature with a linear profile in each
         column)

   -  *basal\_mass\_balance*

      -  Old: 0 (basal mass balance not included in continuity equation)

      -  New: 1 (basal mass balance included in continuity equation)

   -  *sigma*

      -  Old: 2 (read sigma coordinates from config file)

      -  New: 0 (compute standard Glimmer sigma coordinates)

-  New initial conditions have been provided for both CLM and CISM, when
   running the BG1850CN compset at f09 resolution (currently, this is
   the only compset involving CISM that is set up as a “hybrid”
   compset). Importantly, this is the first time we have provided
   spun-up ice sheet initial conditions, so that the ice sheet is in
   rough equilibrium with the CESM climate. However, because of how
   these initial conditions were generated, they will not be in full
   equilibrium with recent versions of CESM. In addition, the ice sheet
   spin-up was done with some altered configuration settings compared to
   the current out-of-the-box settings. For more details, see the README
   file in this subdirectory of CESM’s inputdata directory:
   <ccsm4\_init/bg40.1850.track1.1deg.006b/0863-01-01>. Despite these
   caveats, the CISM initial condition file in this directory
   (bg40.1850.track1.1deg.006b.cism.r.0863-01-01-00000.nc) could be used
   to start the ice sheet in a roughly spun-up state even when running
   with a different compset and/or resolution than this BG1850CN f09
   hybrid compset for which it is used out-of-the-box.

-  In CLM, fixed the *tsrf* field sent to CISM so that it is
   appropriately time-averaged

-  Option to run with 36 elevation classes (200m each), rather than the
   default 10. (Note that no surface datasets exist for this option, but
   they can be created easily using CLM’s mksurfdata\_map tool.)

-  Option in CLM to write the CISM forcing fields (e.g., surface mass
   balance) to history files, for each elevation class. (Previously,
   only the grid cell average could be written to the CLM history file.)
   (This is documented more extensively in Section 6.4.)

-  Added a *CISM\_OBSERVED\_IC* option to force use of observed initial
   conditions rather than a restart file when performing a hybrid run.
   (This is documented more extensively in Section 2.9.)

-  CISM SourceMods have been split into two directories: any changes to
   source code in the *glimmer-cism* subdirectory need to go in
   *SourceMods/src.cism/glimmer-cism/*

-  CISM is now built using the same cmake build system as is used for
   building the standalone code

-  In the CESM xml files, the old *GLC\_GRID* variable has been renamed
   to *CISM\_GRID*; *GLC\_GRID* is now used for a different purpose and
   generally should not be changed once a case has been created

What's new in CESM1.1 with respect to ice sheet modeling?
---------------------------------------------------------

Compared to the CESM1.0 series, there have been a number of improvements
in CESM that are relevant for ice sheet modeling:

-  A new compset type, *TG*, has been added. This allows running the
   standalone ice sheet model forced by output from a previous, coupled
   CESM simulation. We provide a variety of out-of-the-box forcing data,
   or you can generate your own forcing data. See Section 3 for more
   details.

-  Support for longer coupling intervals in CISM and in CESM scripts –
   e.g., a 1-year coupling interval, useful for TG runs of centuries to
   many millennia.

-  Changed the default Greenland ice sheet grid to 5 km (previously was
   20 km)

-  Changed a number of other default CISM configuration settings to
   produce more robust ice sheet evolution, especially at 5 km
   resolution

-  Ensemble capability for all CESM components, including CISM (see
   Section 6.1 for details)

-  More robust namelist generation facility, standardized across CESM
   components (see Section 2.7 for details)

-  Enabled ESMF interface for CISM

-  Fixed memory leak in CISM

-  Bug fix for glacier virtual columns in CLM

-  New high-resolution *pct\_glacier* input file for CLM, based on the
   Randolph Glacier Inventory, and new CLM surface datasets based on
   this file (see Section 5.4 for details)

-  New diagnostic capabilities in CLM, including the ability to output
   fields averaged only over the glacier portion of each grid cell (see
   Section 6.5 for details)

-  New IG4804 compset

-  Improved testing capability for TG compsets in the CESM test
   framework

Known problems in CESM1.2
-------------------------

The following are known problems in CESM1.2 that are relevant for ice
sheet modeling:

-  CISM restarts can only occur on year boundaries.

-  CLM's interpinic tool does not work properly for input files with
   multiple glacier elevation classes.

-  The current TG forcing data were generated with a bug in the *tsrf*
   field: each day, the value from a single timestep was sent from CLM
   to CISM, rather than this field being time-averaged.

-  CLM's code for multiple elevation classes (and thus coupling to CISM)
   does not work correctly for GLC\_NEC=1 (i.e., a single elevation
   class, but using the glc\_mec code)

-  There are a number of bugs with the use of a calendar that includes
   leap years; for now we recommend only using a no-leap calendar.

