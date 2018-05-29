.. _tg-compsets:

***************************************************************
Running the standalone ice sheet model within CESM: TG compsets
***************************************************************

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

A run with standalone CISM in the CESM context is known as a TG compset.
This compset uses the active ice sheet model forced by a data land
model; all other components are stubs. Before running a TG compset, you
must have coupler history files from a previous run that included CLM
(see Section 3.2). Alternatively, you can run with existing forcing data
(see Section 3.3).

====================================
 Running with existing forcing data
====================================

There are currently four out-of-the-box TG compsets, using different
periods of forcing data, as shown in the following table:

+-----------------+----------------------+------------------------------------------------------------------------------------------------+
| Compset alias   | Compset short name   | Forcing data                                                                                   |
+=================+======================+================================================================================================+
| TG              | TG2000\_G1           | From a BG20TRCN run (20:sup:`th` century transient, fully coupled), years 1976 – 2005          |
+-----------------+----------------------+------------------------------------------------------------------------------------------------+
| TG1850          | TG1850\_G1           | From a BG1850CN run (preindustrial spinup, fully coupled)                                      |
+-----------------+----------------------+------------------------------------------------------------------------------------------------+
| TG20TR          | TG20TR\_G1           | From a BG20TRCN run (20:sup:`th` century transient, fully coupled), years 1850 – 2005          |
+-----------------+----------------------+------------------------------------------------------------------------------------------------+
| TGRCP85         | TGRCP8\_G1           | From a BGRCP85 run (21:sup:`st` century, RCP 8.5 scenario, fully coupled), years 2006 – 2100   |
+-----------------+----------------------+------------------------------------------------------------------------------------------------+

The fully-coupled runs that provided the forcing data for these compsets
are described in a file in the CESM inputdata repository, alongside the
forcing data
(https://svn-ccsm-inputdata.cgd.ucar.edu/trunk/inputdata/lnd/dlnd7/CPLHIST_SNO/run_documentation.txt_c120808.txt).

To run with one of these existing sets of forcing data, simply create a
case with one of the above compsets. **The resolution for this run
should be f09\_g16, since that is the resolution at which the forcing
data were created.** (In theory, you can run using a different
resolution, but that will involve a spatial interpolation of the forcing
data.) You *can* run with a different CISM\_GRID than the one used to
create the forcing data (gland5UM).

=================================================
 Creating and running with your own forcing data
=================================================

Currently, TG compsets are only able to handle forcing data from a
previous CESM run (although, in theory, it should be possible to “fake”
CESM output by creating files with the same format as existing TG
forcing files). Thus, performing a TG run with your own forcing data is
a two-step process: (1) Perform a CESM run that includes an active land
model (CLM), saving the necessary forcing files, and (2) perform a TG
run using these new forcing data.

=========================================
 Performing a run to create forcing data
=========================================

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

=================================================
 Performing a TG run using your own forcing data
=================================================

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

===============================================
 Changes to some CESM defaults for TG compsets
===============================================

TG compsets have much lower computational expense per simulation year
and much greater typical run lengths compared to most CESM
configurations. Thus, a number of settings are changed automatically
when running with a TG compset. These include:

Default run length: 10 years (rather than 5 days)

Default coupling frequency: annual (rather than daily or more frequent)

Default PE layout: single processor

