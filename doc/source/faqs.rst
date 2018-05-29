.. _faqs:

***************************
Frequently Asked Questions
***************************

This section answers some miscellaneous questions about ice sheet
modeling within CESM.

====================================================================================================================
 How do I run an ensemble of ice sheets, which differ in their parameters, initial conditions, and/or forcing data?
====================================================================================================================

Starting with CESM1.1, it is possible to run an ensemble of any model
component(s) within a single CESM executable. Thus, you have a single
case directory and a single run directory, but with separate input and
output files for each ensemble member. In much of the CESM
documentation, this capability is referred to as "multi-instance". But
here we refer to it as "multiple ensemble members" to avoid confusion
with CISM's capability to run multiple ice sheets (e.g., Greenland and
Antarctica), which is also referred to as multiple instances.

An ensemble can be used in a TG compset to run multiple versions of
CISM, either forced by the same data or each forced by different data.
An ensemble can also be used in a coupled configuration (IG, FG or BG
compsets). In this case, a requirement is that if there are N ensemble
members of any one active component, then there must be N ensemble
members of ALL active components. For example, in an IG compset, if you
want to run with 32 CISM ensemble members, then you must also have 32
CLM ensemble members, where CLM #1 will be coupled to CISM #1, etc. In
this case, you could choose to have either a single data atmosphere, or
32 data atmospheres, each providing their own forcing to one of the 32
CLMs.

Note that different ensemble members can ONLY differ in namelist /
configuration settings and initial conditions / restart files – they are
all using the same model executable.

The use of the ensemble capability is as follows; this assumes you are
running a TG compset:

1. Create the case, as normal.

2. Modify *env\_mach\_pes.xml*:

   a. Specify the desired number of CISM ensemble members by modifying
      *NINST\_GLC. *

   b. Also change *NTASKS\_GLC* to this same value: this specifies the
      total number of GLC tasks, which are divided among the N ensemble
      members.

   c. If you want to use separate forcing data for each ensemble member,
      then also modify *NINST\_LND* and *NTASKS\_LND* similarly, in
      order to run with a separate data land model for each CISM
      ensemble member. (See below for more details on how to use
      different forcing data for each ensemble member.)

   d. Probably also change *NTASKS\_CPL* to be equal to *NTASKS\_GLC*,
      although for large ensembles you may want to experiment with a few
      different values of this variable to obtain the best performance.

3. Run *cesm\_setup*. This will create a separate *user\_nl\_cism\_NNNN*
   file for each ensemble member (where *NNNN* is the number of the
   ensemble member). If you are running with multiple data land ensemble
   members, then there will also be separate *user\_nl\_dlnd\_NNNN* and
   *user\_nl\_dsno\_NNNN* files.

4. Put any namelist / configuration modifications specific to a single
   ensemble member in that member's *user\_nl\_cism\_NNNN* file.
   Modifications that apply to all ensemble members must be put in ALL
   *user\_nl\_cism\_NNNN* files.

5. Build and run the model as normal. Output files will be similar to
   those in a standard run, except there will now be N instances of each
   output file (history, restart and log files), which are
   differentiated by an occurrence of *\_NNNN* in the file name.

=========================================================================
 How do I initialize each ensemble member with a different restart file?
=========================================================================

In each *user\_nl\_cism\_NNNN* file, set the value of the
*cisminputfile* parameter to point to the desired restart file for that
ensemble member. For example:

-  echo "cisminputfile = '/path/to/file/for/member1.nc' " >>
   user\_nl\_cism\_0001

-  echo "cisminputfile = '/path/to/file/for/member2.nc' " >>
   user\_nl\_cism\_0002

====================================================================================
 How do I use different forcing data for each CISM ensemble member in a TG compset?
====================================================================================

To run with different forcing data for each CISM ensemble member in a TG
compset, you need to use N dlnd instances, as described above, where
dlnd #1 will force CISM #1, etc. Note that, even if the different CISM
ensemble members are identical in everything except for their forcings,
you still need to use N CISM ensemble members in this case.

After running *cesm\_setup*, you will see multiple instances of each of
the dlnd input files in CaseDocs: *dlnd\_in\_NNNN, dsno\_in\_NNNN,* and
*dlnd.streams.txt.sno.cplhist\_NNNN.*

-  preview\_namelists

-  cp CaseDocs/dlnd.streams.txt.sno.cplhist\_NNNN
   ./user\_dlnd.streams.txt.sno.cplhist\_NNNN

-  chmod u+w user\_dlnd.streams.txt.sno.cplhist\_NNNN

Then change each *user\_dlnd.streams.txt.sno.cplhist\_NNNN* file as
desired. Usually, you will change the variables listed under *filePath*
and/or *fileNames* in the *fieldInfo* section of this file.

**Important note:** Once you have created a
*user\_dlnd.streams.txt.sno.cplhist* file, further changes to most of
the *DLND\_\** variables in *env\_run.xml* will have no effect. Thus, if
you need to change any of these variables, first remove the
*user\_dlnd.streams.txt.sno.cplhist\_NNNN* files, then make necessary
modifications to these xml variables. Finally, repeat the above
procedure for modifying the streams files.

====================================================================
 How do I force each ensemble member with a different set of years?
====================================================================

If you want to use a different set of years for each CISM ensemble
member in a TG compset, you can do this by setting the *streams*
variable in each of the *user\_nl\_dsno\_NNNN* files. This technique can
be used separately from or together with the above technique for using a
different set of forcing files for each ensemble member.

For example, if you have forcing data from 1850 – 2005, and you want to
force CISM #1 by looping through the years 1850 – 1899, and CISM #2 by
looping through the years 1900 – 2005, you would do the following:

1. Set *DLND\_CPLHIST\_YR\_START* to 1850 and *DLND\_CPLHIST\_YR\_END*
   to 2005 in *env\_run.xml*. These variables determine the full set of
   files available to dlnd, by creating the file names in the
   *dlnd.streams.txt.sno.cplhist\_NNNN* files. It is fine for all
   available years to be listed in each of these streams files. So if
   all you want to do is use a different subset of years for each
   ensemble member (drawing from the same full set of files), you will
   not need to modify these streams files.

2. Add the following line to *user\_nl\_dsno\_0001* (note that the three
   values are: *yr\_align, yr\_start, yr\_end*. This example assumes
   that you want to use *yr\_align*\ =1, which would be appropriate if
   *RUN\_STARTDATE* is year 1; otherwise, you likely will want to set
   *yr\_align* to match the start year of the run):

    streams = "dlnd.streams.txt.sno.cplhist\_0001 1 1850 1899"

1. Add the following line to *user\_nl\_dsno\_0002:*

    streams = "dlnd.streams.txt.sno.cplhist\_0002 1 1900 2005"

===============================================================================================================================================================
 How do I run CLM with something other than 10 elevation classes, and/or how do I create a CLM4.5 surface dataset that is consistent with CISM over Greenland?
===============================================================================================================================================================

These are two separate questions, but both require running CLM’s
*mksurfdata\_map* tool, so they are combined here for convenience.

For compsets that use CLM’s *glacier\_mec* code (IG, FG and BG
compsets), the default is to use 10 elevation classes. As discussed in
Section 5.3, the code currently supports running with 1, 3, 5, 10 or 36
elevation classes. **However, there is currently a bug in CLM that
prevents running the glacier\_mec code with 1 elevation class.** (This
bug is in the setting of the elevation of virtual columns in
subgridMod.F90.)

Each number of elevation classes requires a different surface dataset
for CLM. Currently, CLM surface datasets have only been created with 10
elevation classes. Thus, if you want to run with a different number of
elevation classes, you must first create a new surface dataset, along
with a transient pft dataset if you will be doing a transient run. The
process for creating these datasets is described in the CLM User’s
Guide. Briefly, you will use CLM’s *mksurfdata\_map* tool, contained in
*models/lnd/clm/tools/clmXXX/mksurfdata\_map*, where XXX is either 4\_0
or 4\_5, depending on whether you are creating a surface dataset for
CLM4.0 or CLM4.5. It is easiest to use the *mksurfdata.pl* wrapper
script contained in that directory, providing the argument *-glc\_nec.*
For example, to create a surface dataset with 36 elevation classes at
0.9x1.25 degree resolution, for the year 1850, along with a transient
pft dataset spanning the late 19\ :sup:`th` and 20\ :sup:`th` centuries,
you would run:

-  mksurfdata.pl -res 0.9x1.25 -y 1850-2000 -glc\_nec 36

There is an important difference in behavior between the CLM4.0 vs.
CLM4.5 versions of this tool: In the CLM4.0 version, specifying any
non-zero value for -glc\_nec gives you a version of the percent glacier
raw dataset that is consistent with the CISM dataset over Greenland.
However, in the CLM4.5 version, regardless of the value of -glc\_nec,
the default glacier raw dataset comes entirely from the Randolph Glacier
Inventory. **For CLM4.5, if you want to create a surface dataset that is
consistent with CISM over Greenland, you will need to add the
*-merge\_gis* argument to mksurfdata.pl.**

You can then create a case using an IG, FG or BG compset. Before
running, you will need to change three settings:

-  *GLC\_NEC* in *env\_run.xml* (e.g., *xmlchange GLC\_NEC=36*).

-  Point CLM to the new surface dataset you created (specify *fsurdat*
   in *user\_nl\_clm*).

-  If relevant to your case, point CLM to the new transient pft dataset
   you created (specify *fpftdyn* in *user\_nl\_clm*).

==============================================================
 How do I add new settings in CISM's namelist or config file?
==============================================================

If your code development requires the addition of a runtime setting, set
either in *cism\_in* or *cism.config*, you will need to add information
about the new variable in the xml file that is used to generate these
input files. See the documentation in
*models/glc/cism/bld/README.build-namelist*, and particularly the
section "CISM Use Cases".

=======================================================
 How do I output forcing fields sent from CLM to CISM?
=======================================================

CLM sends three sets of fields to CISM, for each elevation class: *qice,
tsrf* and *topo* (see Section 5.2). It can often be useful to view the
values of these forcing fields for each elevation class within each grid
cell. To do this, you can use the three CLM history variables,
*QICE\_FORC, TSRF\_FORC* and *TOPO\_FORC.* These history variables are
inactive by default, but can be added to any of CLM’s history files
using the *hist\_fincl* CLM namelist variables. For example, to add
*QICE\_FORC* and *TSRF\_FORC* to CLM’s default (monthly) history file,
you would add the following in *user\_nl\_clm*:

hist\_fincl1 = 'QICE\_FORC','TSRF\_FORC'

As with other CLM history variables, additional history files can be
created with different time frequencies. See the CLM User’s Guide for
details on how to do this.

========================================================================
 How do I add a CLM history field that provides averages only over ice?
========================================================================

In general, CLM history fields give weighted averages over the entire
grid cell. If you are interested in diagnostics just over ice landunits
for certain history fields, you can make a source code modification for
each field of interest. This is done in
*models/lnd/clm/src/main/histFldsMod.F90*: Find the history field(s) of
interest in this file, and add the following optional argument to the
*hist\_addfld1d* or *hist\_addfld2d* call for that history field:
*l2g\_scale\_type='ice'*. You may want to copy and paste the call in
order to maintain the original history field and add a new field that
applies just over ice (being sure to change *fname*). For example,
examine the difference between the fields *TSOI* and *TSOI\_ICE*:

call hist\_addfld2d (fname='TSOI', units='K', type2d='levgrnd', &

avgflag='A', long\_name='soil temperature (vegetated landunits only)', &

ptr\_col=clm3%g%l%c%ces%t\_soisno, l2g\_scale\_type='veg')

call hist\_addfld2d (fname='TSOI\_ICE', units='K', type2d='levgrnd', &

avgflag='A', long\_name='soil temperature (ice landunits only)', &

ptr\_col=clm3%g%l%c%ces%t\_soisno, l2g\_scale\_type='ice')

