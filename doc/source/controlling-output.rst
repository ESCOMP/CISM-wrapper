.. _controlling-output:

************************************
Controlling output from CLM and CISM
************************************

FIXME: bring this section up to date.

=========================
 Controlling CISM output
=========================

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
"\*\_io.F90" specify the fields than can be written out. The easiest way
to write out new variables is to add them to a file ending in "vars.def"
and then rebuild the "\*\_io.F90" files using a python script. The
necessary script can be found in $CASEROOT/Buildconf/cismIOconf. See the
README.cismIO file in that directory for details.

=============================================
 Producing land ice-specific output from CLM
=============================================

Outputting forcing fields sent from CLM to CISM
===============================================

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

Adding a CLM history field that provides averages only over ice
===============================================================

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

