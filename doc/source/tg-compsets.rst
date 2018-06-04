.. _tg-compsets:

**************************************************************
Running the standalone ice sheet model within CESM: T compsets
**************************************************************

============
 Background
============

The ice sheet model requires much less computational time per simulated
year than other CESM components, but often needs to be run for many more
model years (e.g., tens of thousands of years rather than a century).
Thus, it can be desirable to run CISM in standalone mode, forced by
output from a previous CESM run. In this way, you can cycle many times
through the forcing data from a previous run (e.g., to spin up the ice
sheet model), or run parameter sensitivity analyses much faster than you
could within the coupled system.

A run with standalone CISM in the CESM context is known as a T compset. (In the past this
was referred to as a TG compset. Now the G appears near the end of the compset name, so
compsets have names like T1850G.)  This compset uses the active ice sheet model forced by
a data land model; all other components are stubs. Before running a T compset, you must
have coupler history files from a previous run that included CLM (see
:numref:`tg-with-existing-data`). Alternatively, you can run with existing forcing data
(see :numref:`tg-with-your-own-data`).

.. _tg-with-existing-data:

====================================
 Running with existing forcing data
====================================

There is currently just a single set of forcing data available for running T
compsets. These forcing data were created with software testing rather than scientific
validity in mind. They were created from 30 years of an ``I1850Clm50Sp`` compset (CLM
forced by a data atmosphere with GSWP3 forcing, starting from an already-near-spun-up
state, with nominally year-1850 forcings and satellite phenology). The resolution was
``f09_g17``. The code base was close to the final CESM2.0 release. For more details, see
`<https://svn-ccsm-inputdata.cgd.ucar.edu/trunk/inputdata/lnd/dlnd7/CPLHIST_SNO/i.e20.I1850Clm50Sp.f09_g17.001_c180502/README>`__.

There are two out-of-the-box T compsets that use these forcing data: T1850G, which uses
CISM2, and T1850G1, which uses CISM1. **You should run these compsets at f09_g17
resolution --- i.e., with the same land resolution and ocean mask used to create the
forcing data.** You can use any CISM resolution, although the current forcing data only
have complete forcings for Greenland, not Antarctica.

So a typical ``create_newcase`` command when running with CISM2, with the standard 4-km
Greenland grid, would look like:

.. code-block:: console

   ./create_newcase --case my_t_case --compset T1850G --res f09_g17_gl4

.. _tg-with-your-own-data:

=================================================
 Creating and running with your own forcing data
=================================================

FIXME: Bill S needs to update this section

Currently, TG compsets are only able to handle forcing data from a
previous CESM run (although, in theory, it should be possible to “fake”
CESM output by creating files with the same format as existing TG
forcing files). Thus, performing a TG run with your own forcing data is
a two-step process: (1) Perform a CESM run that includes an active land
model (CLM), saving the necessary forcing files, and (2) perform a TG
run using these new forcing data.

Performing a run to create forcing data
=======================================

To create the necessary forcing data (surface mass balance and surface
temperature in each glacier elevation class), you need to perform a CESM
run using a compset that includes an active land model (CLM) and an
active ice sheet model (CISM). This can be done with IG, FG or BG
compsets, or their variations.

In order to save the necessary forcing data, you need to set the driver
namelist variable, histaux\_s2x1yr. This can be set by adding the
following line to user\_nl\_cpl in your case directory:

    histaux\_s2x1yr = .true.

This will cause the coupler to write out annual averages of the forcing
fields sent from CLM to CISM. The files containing these averages will
appear in the cpl/hist directory within your archive space, with names
like:

    $CASE.cpl.hs2x.0001-01-01.nc

A TG run that later uses these coupler history files as forcing should
give *nearly* identical CISM results as the original run. Small
differences arise because these forcing files are written with single
precision, leading to roundoff error on the order of 10\ :sup:`-7`. If
you need exact reproducibility, you can make a small source code
modification prior to performing this run: In the routine
*seq\_hist\_writeaux* in models/drv/driver/seq\_hist\_mod.F90, change
the four instances of use\_float=.true. to use\_float=.false.

Performing a TG run using your own forcing data
===============================================

To perform a standalone CISM run forced by your newly-created forcing
data, first create a new case using one of the existing TG compsets (see
above table). It can be easiest to use the compset whose forcing data
most closely matches yours (e.g., if you are using forcing data from a
future RCP simulation, then use TGRCP85), but the choice of TG compset
does not matter as long as you take care in setting the necessary xml
variables appropriately, as described below. The resolution of the TG
run (as specified by the -res flag to create\_newcase) should match the
resolution of the run used to create the forcing data. You *can* run
with a different CISM\_GRID than the one used to create the forcing
data.

The following variables in env\_run.xml should be modified appropriately
for your forcing data:

DLND\_CPLHIST\_DIR: Directory in which your cpl.hs2x files can be found

DLND\_CPLHIST\_CASE: Name of the case used to create the cpl.hs2x files
(files are assumed to be named
$DLND\_CPLHIST\_CASE.cpl.hs2x.yyyy-01-01.nc)

DLND\_CPLHIST\_YR\_START: First year of forcing data (can be set later
than the first existing year of data if you want to use a subset of the
available years)

DLND\_CPLHIST\_YR\_END: Last year of forcing data (can be set earlier
than the last existing year of data if you want to use a subset of the
available years)

RUN\_STARTDATE: Determines the model year in which the run starts. This
can be set to anything you want, but a good convention is:

-  For transient TG runs forced by output from a transient CESM run, set
   to the first year of forcing data (this corresponds to the real-world
   year, in some sense)

-  For non-transient TG runs (forced either by output from a
   non-transient run, or by cycling through the available forcing data
   multiple times), set to 0001-01-01 (in this case, there is no
   real-world meaning to the start year)

   DLND\_CPLHIST\_YR\_ALIGN: The simulation year corresponding to
   DLND\_CPLHIST\_YR\_START. This will usually be the same as the year
   in RUN\_STARTDATE, but it can be set to a different year to start the
   simulation with a different year of forcing data.

   GLC\_NEC: The number of glacier elevation classes. This must agree
   with the value used in the run that created the forcing data.

-  preview\_namelists

-  cp CaseDocs/dlnd.streams.txt.sno.cplhist
   ./user\_dlnd.streams.txt.sno.cplhist

-  chmod u+w user\_dlnd.streams.txt.sno.cplhist

Then change the domain file in *user\_dlnd.streams.txt.sno.cplhist* to
correspond to the domain file that was used for the run that created the
forcing data. This file is listed in the *fileNames* subsection of the
*domainInfo* section in that file, and is currently hard-coded to
*domain.lnd.fv0.9x1.25\_gx1v6.090309.nc*.

**Important note:** Once you have created a
*user\_dlnd.streams.txt.sno.cplhist* file, further changes to the
*DLND\_\** variables in *env\_run.xml* will not be picked up correctly
by the scripts. Thus, if you need to change any of these variables,
first remove the *user\_dlnd.streams.txt.sno.cplhist* file, then make
necessary modifications to these xml variables. Finally, repeat the
above procedure for modifying the domain file.

==============================================
 Changes to some CESM defaults for T compsets
==============================================

T compsets have much lower computational expense per simulation year and much greater
typical run lengths compared to most CESM configurations. Thus, a number of settings are
changed automatically when running with a T compset. These include:

- Default run length: 5 years (rather than 5 days)

- Default coupling frequency: annual (rather than daily or more frequent)

