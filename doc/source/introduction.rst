.. _introduction:

*****************
Introduction
*****************

This document accompanies the Community Earth System Model (CESM) User’s
Guide and is intended for users who would like to run CESM with dynamic
ice sheets and/or an improved surface mass balance scheme for glaciated
regions. For more information, please see the CESM User’s Guide.

The introduction provides some scientific background, along with a brief
history of land-ice model development within CESM. Section 2 is a
quick-start guide for new users. Section 3 describes how to run the
standalone ice sheet model within CESM, forced by output from a
previous, coupled run. Section 4 describes the Community Ice
Sheet Model (CISM), the dynamic ice sheet model in CESM. Section
5 gives a detailed description of the surface-mass-balance scheme for
ice sheets in the Community Land Model (CLM). Section 6 answers some
common questions about model usage.

It should be emphasized that coupled land-ice modeling is a work in progress.
Ice sheets are a relatively new CESM component, having been introduced in CESM1.0.
Ice sheet dynamics and physics in CISM continue to evolve, as do CESM coupling
capabilities and the ice sheet surface climate.
Many scientific limitations remain; in particular, CESM2.0 does not
incorporate a dynamic Antarctic ice sheet or interactive ice sheet--ocean coupling.
New developments targeting Antarctica and other marine ice sheets
will be added to CESM as they become available.

This documentation is itself in progress. If you find errors or would like
to have some additional information included, please contact the authors:
William Lipscomb (lipscomb@ucar.edu), William Sacks (sacks@ucar.edu),
and Gunter Leguy (gunterl@ucar.edu).

===============================
 The Community Ice Sheet Model
===============================

CISM is a thermomechanical ice sheet model that solves the
equations of ice flow, given suitable approximations and boundary
conditions. The source code is written primarily in Fortran 90 and 95.
The model resides on the github repository
(https://github.com/ESCOMP/cism), where it is under active development.
CISM2.1 is currently the default ice sheet model in CESM2.0.

CISM2.1 introduces a new dynamical core, Glissade, which runs in parallel
and solves equations for the conservation of mass, momentum, and
internal energy. Glissade supports several approximations of the
Stokes equations for ice flow: the shallow-ice approximation
(SIA), the shallow-shelf approximation (SSA), a depth-integrated viscosity
approximation (DIVA), and the Blatter-Pattyn (BP) approximation.
DIVA is the default solver for CISM in the CESM framework.

In previous versions of CESM with CISM coupling capabilities, the
dynamical core of the model was known as Glide and only solved the
shallow-ice approximation in serial. While this option is still available,
it is no longer supported.

The surface boundary conditions (e.g., the surface temperature and
surface mass balance) are supplied by a climate driver. When
CISM is run in CESM, the climate driver is Glad, which receives
the temperature and SMB from the coupler and passes them to the
ice-sheet grid. The lower boundary conditions are given
by a geothermal dataset, which supplies heat fluxes at the lower boundary,
and by a basal topography dataset.
Optionally, the basal topography can be modified at runtime by an isostasy model;
isostasy is turned off by default.

The new version of CISM adds a set of basal sliding and calving law
options of varying complexity. The default options and parameters 
were chosen based on standalone Greenland simulations (with SMB forcing
from the RACMO2 regional climate model) to support a
stable, reasonably accurate simulation of the Greenland Ice Sheet;
see Lipscomb et al. (2018) for details.
The default basal sliding option is pseudo-plastic sliding,
with parameters based on Aschwanden et al. (2016).
The default calving law is "no-float", with all floating ice
calving immediately.

The model currently has simple treatments of basal hydrology, including
a local till model used in conjunction with pseudo-plastic sliding.
More complex schemes for subglacial water hydrology and
evolution of basal till strength are being developed.

=======================
 Scientific background
=======================

Historically, ice sheet models were not included in global climate
models, because they were thought to be too sluggish to respond
to climate change on decade-to-century time scales. In the Community
Climate System Model (CCSM), as in many other global climate models, the
extent and elevation of the Greenland and Antarctic ice sheets were held fixed.
Interactions between ice sheets and other parts of the climate system were
largely ignored.

Recent observations, however, have established that the Greenland and
Antarctic ice sheets are losing mass in response to atmospheric and
ocean warming (Shepherd et al. 2012, Church et al. 2013).
The current ice sheet contribution to global mean sea level rise
is about 1 mm/yr, with somewhat greater losses from Greenland
than from Antarctica. 
Greenland mass loss is caused primarily by increased surface melting
and runoff, and secondarily by increased glacier outflow.
In Antarctica, where there is little surface ablation,
mass is lost primarily because of greater outflow, triggered in part
by ocean warming and increased melting beneath ice shelves.

Glaciers outside the two ice sheets also have retreated in recent years
(Vaughan et al. 2013).
Although the total volume of the Earth's glaciers (~0.4 m sea-level equivalent)
is much less than that of the Greenland ice sheet (~7 m) and
the Antarctic ice sheet (~60 m), glaciers can respond
quickly to climate change. Total glacier mass loss is ~1 mm/yr,
similar to the loss from the two ice sheets. Together, glaciers and
ice sheets account for the majority of recent sea level rise,
with ocean thermal expansion contributing most of the remainder.

As land ice modeling has become more urgent, there have been major advances
in ice sheet models (ISMs). Early ISMs used either the
shallow-ice approximation (SIA) or the shallow-shelf approximation (SSA).
The SIA, which assumes that vertical shear stresses are dominant, is valid
for slow-moving ice-sheet interiors, whereas the SSA, which assumes that flow
is dominated by lateral and longitudinal stresses in the horizontal plane,
applies to floating ice shelves. Neither approximation is valid for ice streams
and outlet glaciers where both vertical-shear and horizontal-plane stresses are important.
Advanced ISMs developed in recent years solve the more accurate Stokes equations 
or various higher-order (also known as first-order) approximations (Pattyn et al., 2008).
Among the models that solve Stokes or higher-order equations are the Parallel Ice Sheet Model
(Bueler and Brown, 2009; Winkelmann et al., 2011), the Ice Sheet System Model (Larour et al., 2012),
the Penn State Model (Pollard and DeConto, 2012), BISICLES (Cornford et al., 2013),
Elmer-Ice (Gagliardini et al., 2013), MPAS-Albany Land Ice (Hoffman et al., 2018), 
and CISM (Lipscomb et al. 2018).

Although much can be learned from ice sheet models in standalone mode,
coupled models are required to capture important feedbacks (Vizcaino et al. 2014).
For example, changes in ice sheet albedo and elevation can affect
regional atmospheric circulation and climate.  Also, ice sheet changes
can modify the climate through modified freshwater fluxes to the ocean.
The addition of interactive ice sheets to CESM and other Earth System models (ESMs)
will likely lead to scientific insights that would not be possible with
standalone ice sheet models.

====================
 Ice sheets in CESM
====================

Starting in 2006, researchers in the Climate, Ocean and Sea Ice Modeling
(COSIM) group at Los Alamos National Laboratory (LANL) worked with
scientists at the National Center for Atmospheric Research (NCAR) to
incorporate an ice sheet model in the CCSM/CESM framework. This work was
funded primarily by the Department of Energy (DOE) Scientific
Discovery through Advanced Computing (SciDAC) program, with additional
support from the National Science Foundation (NSF). The Glimmer ice
sheet model (Rutt et al. 2009), developed by Tony Payne and colleagues
at the University of Bristol, was chosen for coupling. Although
Glimmer’s dynamical core was relatively basic, a higher-order dynamics
scheme was under development. In addition, the model was well structured
and well documented, with an interface (Glint) to enable coupling to ESMs.

In 2009, the U.K. researchers who designed Glimmer joined efforts with
U.S. scientists who were developing a Community Ice Sheet Model (CISM),
and the model was renamed Glimmer-CISM. Model development was overseen
by a six-member steering committee including Magnus Hagdorn (U.
Edinburgh), Jesse Johnson (U. Montana), William Lipscomb (LANL), Tony
Payne (U. Bristol), Stephen Price (LANL), and Ian Rutt (U. Swansea).

Glimmer had a positive-degree-day (PDD) scheme, which uses
empirical formulas to relate surface temperatures to summer melting.
PDDs schemes, however, are not ideal for climate change
modeling, because empirical relationships that are valid for present-day
climate may not hold in the future. Instead, a surface-mass-balance
scheme for ice sheets was developed for the Community Land Model (CLM).
This scheme computes the SMB in each of ~10 elevation classes per grid
cell in glaciated regions. The SMB is passed via the coupler to the ice
sheet component, where it is averaged, downscaled, and used to force the
dynamic ice sheet model at the upper surface. (See Section 4 for details.)

In 2010, CESM1.0 was released with an initial implementation of ice sheets
(Lipscomb et al. 2013). The dynamic ice sheet model was a close approximation
of Glimmer-CISM version 1.6, a serial code with shallow-ice dynamics.
Optionally, the SMB was computed by CLM in multiple elevation classes 
for glaciated regions. 

Subsequent ice-sheet model development targeted for CESM 
was led by DOE-funded researchers at LANL, Oak Ridge National Laboratory (ORNL),
and Sandia National Laboratories (SNL). Much of this work was done
under the Ice Sheet Initiative for CLimate ExtremeS (ISICLES) project,
followed by the Predicting Ice Sheet and Climate Evolution at Extreme Scales
(PISCEES) project, culminating in the 2014 release of CISM 2.0
(Price et al. 2014). CISM2.0 was a major advance over Glimmer, 
with support for parallel simulations using higher-order dynamics,
as well as a suite of test cases and links to third-party solver libraries.

Subsequently, CISM development shifted to NCAR, with primary support from
the National Science Foundation. Recent work has focused on making the model
more practical and robust for century-to-millenial scale Greenland simulations.
CISM2.1, released concurrently with CESM2.0 in 2018, includes an efficient
depth-integrated velocity solver and more realistic options for basal sliding
and iceberg calving, among other innovations.
CISM participated in the initMIP-Greenland project on ice sheet initialization
(Goelzer et al. 2018), and a developmental model version was used
for the follow-up initMIP-Antarctica experiments.

Meanwhile, other parts of CESM have evolved to support land-ice science.
In CESM2.0 the surface mass balance of the Greenland and Antarctic ice sheets 
is computed by default in CLM using multiple elevation classes, in simulations
with or without dynamic ice sheets. Also, CESM now supports interactive coupling
of CISM with CLM, allowing the land topography and surface types to evolve
as the ice sheet advances and retreats.
The surface melt climate of both ice sheets has improved with the inclusion of a 
deep firn model that allows for meltwater infiltration and refreezing, 
as well as realistic firn densification rates. Surface winds over ice sheets
are more accurate with a new drag parameterization, and a bias in high-latitude 
longwave cloud forcing is much reduced.


=============
 Limitations
=============

CISM's land-ice capabilities continue to be actively developed, and
some significant limitations remain, as decribed below.


Limitations of the ice sheet model itself
-----------------------------------------

-  CISM2.1 has been designed primarily for simulating Greenland, and is missing 
   some features needed for optimal simulation of marine ice sheets.
   By default, CISM uses a "no-float" scheme in which floating ice immediately calves.
   Although calving and sub-shelf melting schemes are available, they are relatively simple
   and may require hand tuning.
   Also, the CISM2.1 release does not have a grounding-line parameterization (GLP).
   A developmental branch of CISM includes a GLP that will be part of a future release,
   and more realistic calving and basal melting schemes are under development.

-  To date, CISM has been run for Greenland mainly on 4-km grids. The model can be run
   at higher resolutions, but is less well tested and less robust.


Limitations of CISM within CESM
-------------------------------

- CISM restarts can only occur on day boundaries

- There are a number of bugs with the use of a calendar that includes leap years;
  currently, you can only run CISM with a no-leap calendar.

- There is a bug in the outputting of time-average history fields from CISM; currently,
  only instantaneous fields are supported.

Limitations of other components of the CESM modeling system
-----------------------------------------------------------

-  In CESM2.0, CISM can be coupled interactively to CLM, but coupling to the ocean
   is very limited. The ice sheet model can send calving fluxes to the ocean,
   but there is currently no mechanism to compute sub-ice-shelf melt rates
   based on ocean conditions, and ocean boundaries do not evolve in response to ice-shelf changes.
   Offline scripts have been developed to support coupling to the Community Atmosphere
   Model (CAM), adjusting CAM's notion of surface topography; however, this atmosphere
   coupling is not available out-of-the-box and is not officially supported.

-  CESM2.0 does not include a prognostic glacier model. Also, by default, the SMB for mountain glaciers
   is computed for a single elevation class at the mean topography, since
   multiple elevation classes have not been found to improve the SMB outside of ice sheets.


===========================================================
 What's new in CESM2.0 with respect to ice sheet modeling?
===========================================================

Compared to the CESM1 series, there have been a number of improvements
in CESM for land-ice modeling:

-  CESM includes CISM2.1, with a suite of velocity solvers including a 3D higher-order solver
   and a depth-integrated higher-order solver in addition to the SIA and SSA.
   New physics options are available, in particular for basal sliding and iceberg calving.
   Associated with these options are many new default options and parameters for Greenland simulations.

-  The default Greenland grid resolution is now 4 km.

-  By default, the SMB is computed by CLM in multiple elevation classes for ice sheets.
   An SMB downscaled to CISM's ice sheet grid is now available for all coupled simulations
   with an active land model, not just simulations with dynamic ice sheets.

-  CISM can be coupled interactively to CLM, with changes in
   ice sheet extent and thickness feeding back on land surface elevation
   and surface types.

-  CLM includes many improved snow parameterizations. These include both improvements in
   the properties of fresh snow and improvements in the evolution of the snow pack. CLM
   now includes a deep firn model that allows for meltwater infiltration and refreezing,
   as well as realistic firn densification rates.

- See the other sections of this document for more detailed descriptions of new land-ice
  capabilities.

===================================
 Significant changes since CESM2.0
===================================

CESM2.0 only allowed running a single ice sheet at a time. Since then, CISM and the CESM infrastructure have
been extended to allow running multiple ice sheets in a single simulation. There is out-of-the-box support for running Greenland and Antarctica, but other ice sheets can be added as well.
