.. _quickstart:

*******************
Quick-start guide
*******************

This section provides a recipe for checking out code from the CESM
repository, building and running a case, and modifying parameters
related to land ice. It is assumed that the reader is already somewhat
familiar with CESM and is working on a machine that supports CESM out of
the box. For more details on building and running CESM, please see the
CESM User’s Guide, from which this section has been adapted. See also
the CESM website, *http://www.cesm.ucar.edu/models/cesm1.2/.*

==========
 Overview
==========

CESM consists of six physical models: atmosphere (atm), ocean (ocn),
land (lnd), sea ice (ice), land ice (glc), and river runoff (rof). A
central coupler coordinates the models and passes information between
them. Each model can have active, data, dead, and stub components. An
active component is a prognostic physical model. Data components provide
scientifically valid input data for cases where an active model is not
needed, whereas dead components generate invalid data that is used only
for system testing. Stub components are present only to meet interface
requirements when a component is not needed.

The active components of CESM are CAM (the Community Atmosphere Model),
POP (the Parallel Ocean Program), CLM (the Community Land Model), CICE
(the Community Ice CodE), CISM (the Community Ice Sheet Model), and RTM
(the River Transport Model). Currently, CISM is synonymous with
Glimmer-CISM, the dynamic ice sheet model described in Section 4. The
ice-sheet surface mass balance is computed in CLM and passed to CISM via
the coupler.

These components can be run in many different configurations. A
particular mix of components, along with component-specific
configuration and namelist settings, is called a component set, or
“compset”. Among the common compsets are B (fully coupled, with all
active components), F (active CAM, CLM, and CICE, with a data ocean),
and I (active CLM with data atmosphere and stub ice and ocean). These
configurations all use a stub land-ice component. The corresponding
configurations with an active land-ice component are denoted BG, FG, and
IG. In addition, starting with CESM1.1, a TG compset allows running the
ice sheet model in standalone mode, forced by output from a previous BG,
FG or IG run. TG compsets are described in detail in Section 3.

The CESM components can be run on a variety of grids. The atmosphere and
land are often run on the same grid, but CESM supports running them on
different grids. The ocean and sea ice components must always run on the
same grid. The ice-sheet component has a grid for each active ice sheet.
Unlike the other component grids, which are global, the ice-sheet grids
have limited domains. The current grids are polar stereographic
projections with rectangular grid cells.

Three Greenland grids are supported, with resolutions of 20 km, 10 km,
and 5 km, respectively. Antarctic grids at comparable resolutions will
be supported in a future release. Each Greenland grid can be run with
one of three land/atmosphere grids: T31 (a coarse-resolution spectral
grid with dimensions 48 x 96), FV2 (a 1.9\ :sup:`o`\ x2.5\ :sup:`o`
finite volume grid), and FV1 (a 0.9\ :sup:`o`\ x1.25\ :sup:`o`
finite-volume grid). The land resolution is important for ice sheets
because a higher-resolution land grid can be expected to provide a more
realistic surface mass balance.

These definitions will be used in the rest of the section:

-  $COMPSET is the component set.

-  $RES is the grid resolution.

-  $MACH is the machine name.

-  $CCSMROOT is the CESM root directory.

-  $CASE is the case name.

-  $CASEROOT is the pathname of the directory where the case is created.

-  $EXEROOT is the pathname of the executable directory, which is
   usually different from $CASEROOT.

-  $RUNDIR is the directory where CESM is run

-  $DOUT\_S\_ROOT is the root directory for local short-term archiving.

======================
 Downloading the code
======================

CESM code is publicly available through a Subversion repository. You
will need Subversion client software, version 1.4.2 or later. For more
information on Subversion, see http://subversion.tigris.org.

To check out a release version of the code, you should register as a
CESM user here:

http://www.cesm.ucar.edu/models/cesm1.0/register/register_cesm1.0.cgi

You will be sent a username and password you can use to access the
repository.

Now log onto the machine where you plan to build and run CESM. For a
list of available releases, type this command:

-  svn list https://svn-ccsm-release.cgd.ucar.edu/model\_versions

The first time you access the repository from a given machine, you will
need to enter your username and password. This information should be
cached automatically so that you do not need to enter it repeatedly.

To check out a particular release (say, model version cesm1.2.0), type
this:

-  svn co
   https://svn-ccsm-release.cgd.ucar.edu/model\_versions/cesm1\_2\_0

This command puts a copy of CESM in a directory called cesm1\_2\_0. This
is the $CCSMROOT directory. If successful, you will see a message of the
form “Checked out revision xx.”

Other useful Subversion commands include *svn info* (for various
information about the release version), *svn status* (for a list of
files that have changed since checkout), and *svn diff* (to see
differences between the release version and your working copy).

You may want to take a quick tour of the model. Type this:

-  cd cesm1\_2\_0

-  ls

You will see directories called *models*, *scripts* and *tools*. The
*scripts* directory contains useful scripts for creating new cases and
tests. The *models* directory contains subdirectories called *atm, ocn,
lnd, ice*, *glc*, and *rof,* corresponding to the physical models of
CESM. There are also directories called *drv* (the driver), *utils*
(various utilities), *csm\_share* (code shared among different models),
and *dead\_share* (code shared among the dead models). The *tools*
directory contains utilities for creating the mapping files that are
needed by the CESM coupler when you define a new model grid, as well as
a *cprnc* tool that is useful for determining the differences between
two model runs.

=================
 Creating a case
=================

The next step is to create a case. From $CCSMROOT, go to the scripts
directory:

-  cd scripts

Here there is a script called *create\_newcase*. The basic form of the
*create\_newcase* command is

-  create\_newcase -case $CASEROOT \\

    -mach $MACH \\

    -compset $COMPSET \\

    -res $RES

For more information about this command, type

-  create\_newcase -h

For a list of supported machines, compsets, and resolutions, type

-  create\_newcase -list machines

-  create\_newcase -list compsets

-  create\_newcase -list grids

Suppose you are running on the yellowstone machine, and you want to
create an IG case (active land and land-ice components, data atmosphere,
stub ocean and sea ice) using the 1.9\ :sup:`o`\ x2.5\ :sup:`o`
finite-volume grid for the land and atmosphere and a 1\ :sup:`o` grid
for the ocean. You want to call this case “testIGfv2\ *”*. You would
type

-  create\_newcase -case testIGfv2 -mach yellowstone -compset IG -res
   f19\_g16

If successful, you will see a message like this:

Successfully created the case for yellowstone

It is possible to configure cases with many different combinations of
compsets and grids, but not all these cases have been validated
scientifically.

In $CCSMROOT/scripts there is now a subdirectory called *testIGfv2*.
This is the $CASEROOT directory. Go to $CASEROOT:

-  cd testIGfv2

This directory contains a number of scripts and xml files. Take a look
at the file *env\_case.xml*. This file specifies $CASE, $CASEROOT,
$CCSMROOT, and other settings that cannot be changed once the case is
created. For an explanation of these and other settings in the various
xml files, see the online documentation for CESM
(http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/modelnl.html).

===================
 Setting up a case
===================

To configure the case, go to $CASEROOT and type

-  cesm\_setup

This command creates a machine-specific Macros file and run script in
$CASEROOT. It also creates the files *user\_nl\_*\ xxx (where *xxx*
denotes the set of components targeted for this case); *these files are
where all user component namelist modifications are now made.*

Once you set up the case, the file *env\_mach\_pes.xml* (which
determines the processor layout) is locked. It cannot be changed unless
you undo and redo the setup, as follows:

-  cesm\_setup -clean

-  # Make changes to env\_mach\_pes.xml

-  cesm\_setup

Although xml files can be edited manually, it is safer to use the
*xmlchange* command. For example, to change $NTASKS\_ATM before setting
up a case, you would type

-  xmlchange -file env\_mach\_pes.xml -id NTASKS\_ATM -val 128

Or simply:

-  xmlchange NTASKS\_ATM=128

Another setting relevant to runs with active land ice is
$CLM\_FORCE\_COLDSTART. Currently, this is automatically set to “on” for
compsets with an active land-ice component (e.g., IG, FG, and BG). This
means that CLM will be initialized with arbitrary initial conditions
rather than a restart file. If this option is set to “off”, the model
will fail at initialization because the current CLM restart files are
not compatible with active land ice.

=================
 Building a case
=================

The $CASEROOT directory should now contain a script called $CASE.build
(*testIGfv2.build* in our case)\ *.* To build the model, run this
script:

-  testIGfv2.build

**Note that the CISM build requires CMake version 2.8.6 or newer.**
CMake is present on all supported machines, but if you are building on
your own machine, you will need to ensure that CMake is installed and in
your path.

The build script does the following:

-  creates component namelists in $RUNDIR.

-  checks for required data sets and (hopefully) downloads missing data
   automatically.

-  creates the required utility and component libraries.

-  creates the model executable in $EXEROOT.

If the build is successful, you will see this:

CCSM BUILDEXE SCRIPT HAS FINISHED SUCCESSFULLY

Otherwise, you will get a message that the build failed, referring you
to a file in $RUNDIR for information about why it failed. Each component
has its own build-log file; for land ice the filename is of the form
*glc.bldlog.yymmdd-hhmmss.*

Look at the file *env\_build.xml*, which contains settings such as
$DEBUG and $EXEROOT. The $DEBUG option is false by default; set it to
true to turn on compile-time and run-time debugging. When running on
yellowstone, $EXEROOT is /glade/scratch/$CCSMUSER/$CASE/bld by default.
A related setting, $RUNDIR, specifies the location of the directory in
which the model is run. $RUNDIR is set in *env\_run.xml*, and on
yellowstone is /glade/scratch/$CCSMUSER/$CASE/run by default.

The *env\_build.nml* file is locked after the code is built. To change
settings after building, you must clean and redo the build:

-  testIGfv2.clean\_build

-  # Make changes to env\_build.xml

-  testIGfv2.build

================
 Running a case
================

Once the code has built successfully, it can be run by submitting the
run script, $CASE.run. On yellowstone, which manages jobs using LSF, the
first few lines of the run script look like this:

#BSUB -n 64

#BSUB -R "span[ptile=64]"

#BSUB -q regular

#BSUB -N

#BSUB -x

#BSUB -a poe

#BSUB -o poe.stdout.%J

#BSUB -e poe.stderr.%J

#BSUB -J testIGfv2

#BSUB -W 0:50

#BSUB -P 12345678

Here, -n specifies the number of processors that will be requested; -q
is the queue; -o and -e give the names of standard output and error
files that will be written to $CASEROOT; -W is the wall-clock time
requested (50 minutes in the above example); and -P is the project code.
The queue can be changed to raise or lower the run priority. The
wall-clock time should be set to a value sufficient to complete the run
(or at least to write restart files) without exceeding the machine
limit. If a machine is busy, then reducing the time requested will
usually allow the run to start earlier.

Next, look at *env\_run.xml*. Unlike the other xml files, this file can
be modified at any point during a sequence of runs. (As mentioned above,
it is safest to use the *xmlchange* command to edit xml files.) Among
the key settings are $RUN\_TYPE, $RUN\_STARTDATE, and (for cases with an
active land-ice component) $CISM\_GRID. The $RUN\_TYPE is *startup* by
default. Other options are *hybrid* and *branch*; these are explained in
Section 2.9. The default start date is compset-dependent, but is often
simply 0001-01-01, which denotes January 1 of year 1. The default value
of $CISM\_GRID is *gland5UM*, which refers to the 5-km Greenland grid,
using an updated dataset from the University of Montana. To run with a
coarser grid, change this to *gland10* or *gland20 (or gland5 to use an
alternative 5-km dataset)*.

Another important setting is $CONTINUE\_RUN, which is false by default.
This means that the run will start from the beginning (usually from the
date 0001-01-01). If $CONTINUE\_RUN is true, then the run will start
from a restart file generated by a previous run. Restart files reside in
$RUNDIR. $RUNDIR also contains several one-line pointer files (e.g,
*rpointer.glc* for land ice) that specify the name of the current
restart file for each component.

The next setting is $RESUBMIT, an integer (set to 0 by default) that
specifies the number of times the run script will be resubmitted
automatically after a run completes. This option is useful for longer
production runs when you are sure the code is working. If $CONTINUE\_RUN
is initially false and $RESUBMIT > 0, then $CONTINUE\_RUN is
automatically set to true when the run script is resubmitted. If you do
not use the $RESUBMIT option, $CONTINUE\_RUN will remain false unless
you change it manually.

To determine the length of the run, you set $STOP\_OPTION and $STOP\_N.
By default, $STOP\_OPTION is *ndays*, and $STOP\_N = 5, which means the
run will complete after 5 model days. Just after building a case, it is
a good idea to do a 5-day trial run before trying a longer run. The
allowed units of $STOP\_OPTION are seconds, minutes, hours, days,
months, and years. The value of $STOP\_N can be any integer.

The settings $RESTART\_OPTION and $RESTART\_N determine how often
restart files are written. By default, these are set to $STOP\_OPTION
and $STOP\_N, respectively, but they can be modified to write restart
files more often.

For runs with an active land-ice component, $HIST\_OPTION and $HIST\_N
are also important. These generally specify the output frequency for
coupler history files, but CISM’s history frequency is currently
hard-wired to be the same as the coupler history frequency. For compsets
with an active land-ice component, these are set to give annual output
by default.

The *env\_run.xml* file also contains settings for archiving. If
$DOUT\_S is TRUE (the default), the model output will be written to
$DOUT\_S\_ROOT for short-term archiving. On yellowstone, $DOUT\_S\_ROOT
is set to /glade/scratch/$CCSMUSER/archive/$CASE by default.

Once you have modified the run script and *env\_run.xml* as needed, you
are ready to submit the run script. The following commands are for a
machine that manages batch jobs using LSF (e.g., yellowstone). The
commands will be different for other load-sharing systems, such as PBS.
To submit the run, type

-  bsub < testIGfv2.run

If successful, you will get a message that the job is submitted. To
check the job status, type

-  bjobs

You will see something like this:

JOBID USER STAT QUEUE FROM\_HOST EXEC\_HOST JOB\_NAME SUBMIT\_TIME

123456 myname PEND regular yslogin1-ib testIGfv2 Dec 31 23:59

If the status is PEND, your job is waiting in the queue; if the status
is RUN, then the job is running. If you type *bjobs* after the job has
finished, you will see something like “No unfinished job found”. In your
$CASEROOT directory there will be two files with names of the form
*cesm.stderr.123456* and *cesm.stdout.123456*. Look at the latter file.
If all has gone well, you will see lines like this near the end of the
file:

Sat Jan 01 01:23:45 MDT 2011 -- CSM EXECUTION HAS FINISHED

(seq\_mct\_drv): ============ SUCCESSFUL TERMINATION OF CPL7-CCSM

If the job starts but fails to complete, the last line may look like
this:

Sat Jan 01 00:12:34 MDT 2011 -- CSM EXECUTION BEGINS HERE

In this case you will have to figure out why the job failed before
completion. A common cause of failure is not allowing enough time to
finish the run. Otherwise, one of the components probably has failed
during execution. You can investigate by looking at the various
component log files in $RUNDIR. Log files are moved from $RUNDIR to the
local archive only if the run completes successfully.

If you are running on a machine that uses PBS instead of LSF (e.g.,
titan, a Cray XT6 machine at Oak Ridge), then to submit the run script
you would type

-  qsub testIGfv2.run

To check the job status, type

-  qstat

When a run finishes successfully, you will find various log files (e.g.,
*glc.log.yymmdd-hhmmss* for the land-ice model) in $CASEROOT/logs. These
log files are also written to the local archive, $DOUT\_S\_ROOT. The
land-ice log files are in $DOUT\_S\_ROOT/glc, and similarly for the
other models.

History files are also written to the archive, usually in netCDF format.
The history files for land ice are in $DOUT\_S\_ROOT/glc/hist, and those
for the land model are in $DOUT\_S\_ROOT/lnd/hist. These files contain
gridded output written at regular intervals, usually once per model year
for land ice and monthly for other active components. These files can be
viewed and post-processed using a netCDF viewer such as ncview (for a
quick look), Ferret, MATLAB, or IDL.

=============================
 Modifying namelist settings
=============================

Once the code is running, you may want to change namelist or
configuration variables. Variables related to land ice are set in the
files *cism\_in*, *cism.config* and *lnd\_in.* These files appear in
$RUNDIR, and also in $CASEROOT/CaseDocs. User modifications can be made
to these files by adding lines to *user\_nl\_cism* (for variables in
*cism\_in* or *cism.config*) or *user\_nl\_clm* (for variables in
*lnd\_in*); this is described in more detail below. The various
*user\_nl\_xxx* files are created when you first run *cesm\_setup* for
your case. They can be modified any time between running *cesm\_setup*
and the start of the run: the model does NOT need to be rebuilt after
making namelist changes in these files.

Most parameters directly relevant to ice sheet modeling are set in
*cism.config.* This config file contains settings used by
Glimmer-CISM—for example, grid information (which is set automatically
based on the value of CISM\_GRID in *env\_run.xml*), physics parameter
settings, and the names and frequency of input and output files. See
Section 4 and the Glimmer-CISM documentation for more information about
these settings. The *cism\_in* file contains some additional parameters
controlling the CISM run.

The file *lnd\_in* provides settings for CLM. An important variable
related to land ice is *create\_glacier\_mec\_landunit*, which is true
by default for any case with active land ice. When this setting is true,
CLM creates special glacier landunits with multiple elevation classes as
described in Section 5. Another relevant setting is *glc\_smb*, which
also is true by default. If *glc\_smb* is true, then CLM sends the
surface mass balance to CISM via the coupler. If *glc\_smb* is false,
CLM instead sends input for Glimmer-CISM’s positive-degree-day scheme.
(The PDD option is not currently enabled in Glimmer-CISM, but will be
supported in a future release.) Finally, *albice* sets the albedo of
bare glacier ice in the visible and near IR bands of the spectrum.

Changes to both *cism.config* and *cism\_in* can be made by adding lines
with the following format to the *user\_nl\_cism* file in your case
directory:

    namelist\_variable = value

Note that there is no distinction in *user\_nl\_cism* between variables
that will appear in *cism\_in* vs. those that will appear in
*cism.config*: CISM's build-namelist utility knows where each variable
belongs. For example, to set the value of *cism\_debug* to .true. and
*basal\_tract\_const* to 1.e-3, include the following in
*user\_nl\_cism*:

cism\_debug = .true.

basal\_tract\_const = 1.e-3

After running *preview\_namelists*, the following will appear in
*cism\_in*:

&cism\_params

...

cism\_debug = .true.

...

/

and the following will appear in *cism.config*:

[parameters]

basal\_tract\_const = 1.e-3

...

Changes to lnd\_in can be made by adding similar lines to user\_nl\_clm.
For example, to change the ice albedo (values give albedo in the visible
and near-infrared), add the following line to *user\_nl\_clm*:

    albice = 0.55,0.45

After changing any of the *user\_nl\_xxx* files, you can preview the
generated namelists by running the *preview\_namelists* utility in the
case directory. Generated namelists will then appear in the
$CASEROOT/CaseDocs, as well as in $RUNDIR.

All of these namelist parameters are documented online, on CESM’s
namelist documentation page.

Note: There appears to be a bug in the parsing of strings in
*user\_nl\_cism* that are bound for *cism.config*: These appear to be
handled correctly if they are single-quoted, but double-quoted strings
lead to buggy behavior.

Modifying source code
---------------------

Advanced users may want to modify source code in the model directories.
CISM source code is located in $CCSMROOT/models/glc/cism, and CLM source
code is in $CCSMROOT/models/lnd/clm/src. Although it is possible to
change the files in these directories, it is safer to copy files to the
appropriate SourceMods directories and edit them there. For example,
modified CLM files are placed in $CASEROOT/SourceMods/src.clm.

The location for modified CISM files depends on whether it is part of
the Glimmer-CISM code, or part of the CESM-specific code. For any source
code that appears in the *glimmer-cism* subdirectory of
*models/glc/cism*, the modified file should go in
$CASEROOT/SourceMods/src.cism/glimmer-cism. For any other source code
(e.g., modifications to code from *source\_glc*), the modified file
should go directly in $CASEROOT/SourceMods/src.cism. **Putting modified
source files in the wrong SourceMods directory can lead to build
errors.**

**Please note that C++ code in the *glimmer-cism* directory can NOT be
modified via SourceMods**; this C++ code must be modified in-place in
the main source code directory.

Once the modified files are in place, the code can be rebuilt. The files
in the SourceMods directories automatically replace the files of the
same name in the model directories.

Branch and hybrid runs
----------------------

As mentioned above, there are three kinds of runs: startup, branch, and
hybrid. In a startup run (the default), all components are initialized
using baseline states that are determined independently by each
component. The start date is determined by the setting $RUN\_STARTDATE
and is sent from the coupler to the components at initialization.

In a branch run, the components are initialized using a consistent set
of restart files from an earlier run. Usually the case name of the
branch ran is different from that of the previous run. Branch runs are
often used for sensitivity studies. For example, suppose you want to
study the effect of changes in the ice albedo. You could spin up the
model for 100 years, then launch a series of branch runs, each starting
from the same point but proceeding with a different albedo value. A
branch run is bit-for-bit identical to a continuation of the original
run provided that no source code or namelist values are changed. The
start date for the branch run is determined by the restart files from
the original run.

Suppose you want to set up a branch run on yellowstone, starting from
the beginning of year 2 of reference case *testIGfv2*. First go to
$CCSMROOT/scripts and create a new case:

-  create\_newcase -case testIGfv2\_br2 -mach yellowstone -compset IG
   -res f19\_g16

-  cd testIGfv2\_br2

Then set up and build:

-  cesm\_setup

-  testIFfv2\_br2.build

Then modify these settings, which appear in *env\_run.xml:*

-  xmlchange RUN\_TYPE=branch

-  xmlchange RUN\_REFCASE=testIGfv2

-  xmlchange RUN\_REFDATE=0002-01-01

Next, copy the restart files from the reference case into $RUNDIR for
the branch case. If the reference restart files are in
/glade/scratch/$USER/archive/testIGfv2/rest, you would go to the new
$RUNDIR and type

-  cp /glade/scratch/$USER/archive/testIGfv2/rest/0002-01-01-00000/\* .

This command will copy the required restart and pointer files to
$RUNDIR. You can then return to the branch $CASEROOT, edit
*env\_run.xml* as needed, and submit the run script.

A hybrid run is similar, except that the starting date (specified by
$RUN\_STARTDATE) can be changed relative to the reference case.
Bit-for-bit reproducibility is generally not possible for hybrid runs,
but the overall climate is continuous provided that no source code or
namelist values are changed. For more details, see the CESM User’s
Guide.

In some situations, you may want to run a case that is a hybrid start
for most components – so that they start from spun-up initial conditions
– but for which CISM starts with observed initial conditions instead of
a restart file. For example, you may not have a CISM restart file for
this case, or the scientific purpose of the run might warrant starting
with observed initial conditions rather than an existing restart file.

To do this, set the *CISM\_OBSERVED\_IC* variable in env\_run.xml:

-  xmlchange CISM\_OBSERVED\_IC=TRUE

This will force CISM to start from the same observed initial conditions
that are used for a startup run.

Note that *CISM\_OBSERVED\_IC* is ignored for startup runs; for branch
runs, it must be FALSE.

============
 Interpinic
============

**CAUTION: The interpinic tool currently does not work correctly if the
input file (the file you are interpolating from) has glacier\_mec
landunits. However, it should work correctly for the first case
described below.**

When CESM is run in a G configuration (i.e., IG, FG, or BG), the land
model is initialized with a surface data set different from the standard
CLM surface data sets. Glacier landunits (with one column each) are
replaced by glacier\_mec landunits (with ~10 columns each). This means
that a G simulation cannot be initialized directly with the output from
a non-G run. Output from the non-G run must first be interpolated into
the CLM data structure appropriate for a G run. This can be done using
the CLM *interpinic* tool.

For example, suppose we have a CLM restart file called
“\ *bcase.1deg.clm2.r.1000-01-01-00000.nc*\ ”. This file contains CLM
output from year 1000 of a fully coupled (B) case. We want to use this
output to initialize CLM when running a BG case (to save the expense of
spinning up the BG case for ~1000 years from a cold start). We will use
the B file to initialize all landunits other than glacier\_mec. For
glacier\_mec landunits, we will use the output from an IG case, e.g.,
“\ *igcase.1deg.clm2.r.0100-01-01-00000.nc*\ ”. The B file will be the
input file, and the IG file will be the output template file (since it
has the required data structures, including glacier\_mec landunits and
columns, for initializing a BG run).

The interpinic tool is located here:
$\ *CCSMROOT/models/lnd/clm/tools/clmXXX/interpinic*, where XXX is
either 4\_0 or 4\_5, depending on whether you are creating initial
conditions for CLM4.0 or CLM4.5. The first step is to build the
interpinic executable. Go to the *src* subdirectory and edit
*interpinic.F90* as follows: search for the variable
“\ *override\_missing*\ ” and change its value from “true” to “false”.
(By setting this variable to false, we stipulate that glacier\_mec
values, which are missing in the input B file, are *not* overwritten in
the output template IG file.) Save the modified version, and build the
executable:

-  gmake interpinic

Copy the executable to the directory containing the input B file and the
output template IG file. Save a copy of the output template file under a
different name, because this file will be rewritten during the
interpinic. Type the following:

-  ./interpinic -i *input\_filename* -o *output\_filename*

Screen output will let you know how the interpinic process is going. If
the process completes successfully, the template version of
*output\_filename* will be converted to a new file of the same name,
containing a mixture of B data (for landunits other than glacier\_mec)
and IG data (for glacier\_mec), and it will be in the correct format for
initializing a BG run.

Note that the input file and output file do not have to reside on the
same grid. If the grids are different, the input file values will be
written to the nearest neighbor point containing an equivalent landunit
in the output file.

Another use of interpinic: **CAUTION: The interpinic tool currently does
not work correctly for this case:** Suppose that the CLM surface data
set has changed, such that the number of glacier\_mec landunits per
gridcell is different. We want to use a restart file from a G run with
the old data set to initialize a G run with the new data set. In this
case the input file is the restart file from the old run, and the output
template file is a restart file from a short (e.g., 5-day) new run. Go
to the interpinic directory and make the executable as above, but do
*not* change the value of *override\_missing*. (In this case we are not
trying to prevent any information in the output file from being
overwritten.) Again, save a copy of the output file and type the
following:

-  ./interpinic –i *input\_filename* –o *output\_filename*

The new version of *output\_filename* will consist of output from the
old run, converted to the data structure required for initializing the
new run.

For more information on interpinic, see the CLM User's Guide.

