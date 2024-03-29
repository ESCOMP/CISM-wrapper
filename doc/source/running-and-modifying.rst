.. _running-and-modifying:

.. include:: <isonum.txt>

*************************************************
Running and modifying the CESM land ice component
*************************************************

This section provides an overview of some of the most important information needed when
running a land ice-focused CESM case. This assumes you are familiar with the basic process
of obtaining and running CESM, as described in the `CESM Quickstart guide
<https://escomp.github.io/cesm>`_ and the `CIME documentation
<http://esmci.github.io/cime/users_guide/>`__.

In CESM, land ice processes are simulated by two components: The land ice (glc) component
and the land (lnd) component. The land ice component is CISM, the Community Ice Sheet
Model; the land component is CLM, the Community Land Model (which is now part of CTSM, the
Community Terrestrial Systems Model). CLM is responsible for computing the surface mass
balance and surface temperature of ice sheets, along with snow pack evolution for all land
cover types. CISM is responsible for ice sheet dynamics and other ice sheet internal
processes. CISM can include one or more independent ice sheets within a given simulation, each with their own grid.

=================================================
 Choosing a CESM configuration for land ice work
=================================================

Choosing a compset
------------------

CESM high-level configurations are known as "compsets" (short for "component sets",
described in more detail in the `CIME documentation
<http://esmci.github.io/cime/users_guide/compsets.html>`__, the `CESM documentation
<https://escomp.github.io/cesm/release-cesm2/cesm_configurations.html>`_, and the `table
of available compsets <http://www.cesm.ucar.edu/models/cesm2.0/cesm/compsets.html>`_). At
the compset level, there are three main modes for configuring CESM's ice sheet component:

1. Using CISM with ice evolution turned on.

   These compsets have ``CISM2%AIS-EVOLVE`` and/or ``CISM2%GRIS-EVOLVE`` in their compset long name. They typically have the
   letter ``G`` somewhere near the end of their alias, with ``Ga`` for a compset with Antarctica, ``Gg`` for a compset with Greenland, and ``Gag`` for a compset with both Antarctica and Greenland. CISM is fully active, receiving SMB
   from CLM and evolving dynamically. In addition, by default, CISM feeds information back
   to other climate system components --- i.e., it is two-way coupled. Specifically, it
   sends:

   - Glacier areas and elevations to CLM

   - Ice and liquid runoff to the ocean

   |

   Part or all of this two-way coupling can be turned off if desired.
   Note that the liquid runoff sent to the ocean consists of meltwater computed in the interior
   or at the base of the ice; surface liquid runoff is handled separately by CLM.

   |

2. Using CISM with one or more ice sheets, with ice evolution turned off.

   These compsets have ``CISM2%AIS-NOEVOLVE`` and/or ``CISM2%GRIS-NOEVOLVE`` in their compset long name. CISM is built into the system but it does very little. CISM serves two roles in the system in this configuration:

   - Over the CISM domain(s) (typically Greenland and/or Antarctica), CISM dictates glacier areas and
     topographic elevations, overriding the values on CLM's surface dataset. CISM also
     dictates the elevation of non-glacier land units in its domain, and only in this
     domain does CLM downscale atmospheric fields to non-glacier land units.

   - CISM provides the grid onto which SMB is downscaled.

   |
     
   It is also possible for a compset to have one or more ice sheets evolving and one or more ice sheets non-evolving.
     
   |

3. Using a stub glacier component (SGLC), completely avoiding the use of CISM.

   These configurations have ``SGLC`` in the compset long name. This is similar to (2), and CLM still computes
   ice sheet surface mass balance. However:

   - Glacier areas and elevations are taken entirely from CLM's surface dataset, and CLM
     does not perform any downscaling to non-glacier land units.

   - Although SMB is still computed in CLM, it won't be downscaled to a high-resolution
     ice sheet grid.

   There are a few possible reasons why you may want to run in this configuration:

   - Single-point and regional runs over glacier regions, where it doesn't make sense to
     run a dynamic ice sheet model.

   - Runs with a Gregorian calendar (i.e., with leap years): CISM does not currently
     support a Gregorian calendar, so these runs need to use SGLC. This includes data
     assimilation and CAM specified dynamics runs.

   - Other cases where you don't want to include CISM because of the extra complexity this
     involves, such as when setting up a new grid (which would require additional mapping
     files for CISM).

   |

A case can include any number of ice sheets. The ice sheet(s) included in the case should be chosen at ``create_newcase`` time, via choice of an appropriate compset. For example, a compset with both Antarctica and Greenland evolving will have ``CISM2%AIS-EVOLVE%GRIS-EVOLVE`` in the compset long name, and will typically have ``Gag`` in the compset alias.

The choice of evolution or non-evolution for each ice sheet can be chosen via the compset, but this can also be safely changed after creating a case by setting the xml variables, ``CISM_EVOLVE_ANTARCTICA``, ``CISM_EVOLVE_GREENLAND``, etc. You also need to set the overall ``CISM_EVOLVE`` to be consistent with the ice sheet-specific variables: ``CISM_EVOLVE`` must be set to ``TRUE`` if *any* ice sheet is set to evolve, and must be set to ``FALSE`` if no ice sheet is set to evolve. (The scripts will check this for you and won't let you run your case if you have failed to set these variables consistently.)

.. _choosing-a-cism-grid:

Choosing a CISM grid
--------------------

The CESM components can be run on a variety of grids, listed in `this table
<http://www.cesm.ucar.edu/models/cesm2.0/cesm/grids.html>`__. The atmosphere and land are
often run on the same grid, but CESM supports running them on different grids. The ocean
and sea ice components must always run on the same grid. The ice-sheet component has a
grid for each active ice sheet.  Unlike the other component grids, which are global, the
ice-sheet grids have limited domains. The current grids are polar stereographic
projections with rectangular grid cells.

Currently, CESM only supports running CISM over Greenland. The standard grid has a
resolution of 4 km; a 20 km grid is also supported for software testing purposes. There is
out-of-the-box support for running the 4 km grid with most or all of the commonly-used
atmosphere/land and ocean grids.

The alias for a typical CESM grid lists the atmosphere/land grid followed by the ocean
grid. For example, ``f09_g17`` runs the atmosphere and land on a 0.9\ |deg|\ x1.25\ |deg|
finite-volume grid and the ocean and sea ice on a displaced Greenland pole 1\ |deg|
grid. If you don't specify the CISM grid explicitly in the grid alias, it will use the
default grid (4 km). For some common grids, you can also specify the grid explicitly in
the alias, using a ``_gris`` element following the ocean grid. For example, for a compset
with CISM2, ``f09_g17_gris4`` is equivalent to ``f09_g17``.

For the ``T1850Gg`` compset (described in :numref:`t-compsets`), you should use grid
``f09_g17_gris4``. For ``T1850Ga``, you should use grid ``f09_g17_ais8``. For information on introducing new ice sheet grids, see :ref:`new-grids`.

If multiple ice sheets are included in the compset, then you must choose a CESM grid that specifies an ice sheet grid for each active ice sheet (regardless of whether each ice sheet is evolving or non-evolving). For example, for the ``T1850Gag`` compset (which includes both Antarctica and Greenland), you can use grid ``f09_g17_ais8gris4``, denoting an 8-km Antarctica grid and a 4-km Greenland grid.

Special considerations for hybrid cases
---------------------------------------

As described in the `CIME documentation
<http://esmci.github.io/cime/users_guide/running-a-case.html#run-type-initialization>`__,
hybrid cases are commonly used to start a case from a pre-existing case. Generally, all
components start up from the restart files in the reference case. However, in some
situations, you may want to run a case that is a hybrid start for most components --- so
that they start from spun-up initial conditions --- but for which CISM starts with
observed initial conditions instead of a restart file. For example, you may not have a
CISM restart file for this case, or the scientific purpose of the run might warrant
starting with observed initial conditions rather than an existing restart file.

To do this, set the ``CISM_OBSERVED_IC`` variable in ``env_run.xml``:

.. code-block:: console

   ./xmlchange CISM_OBSERVED_IC=TRUE

This will force CISM to start from the same observed initial conditions that are used for
a startup run.

Note that ``CISM_OBSERVED_IC`` is ignored for startup runs; for branch runs, it must be
FALSE.

If you are doing a hybrid run where you have changed ``cisminputfile`` to point to a
restart file from a standalone CISM case (i.e., a run done outside of CESM) or a case with
different physics options, then you must also set ``restart = 0`` in
``user_nl_cism``. (Otherwise ``restart`` has a default value of 1 for a hybrid case, which
can give incorrect behavior if you are using a restart file from a case with different
physics options.)

=============================
 Modifying namelist settings
=============================

Once the code is running, you may want to change namelist or configuration
variables. Variables related to land ice are set in the files ``cism_in``, ``cism.ICESHEET.config`` (for each ice sheet --- e.g., ``cism.ais.config``, ``cism.gris.config``)
and ``lnd_in``. These files appear in the run directory, and also in the ``CaseDocs``
subdirectory of the case directory. User modifications can be made to these files by
adding lines to ``user_nl_cism``, ``user_nl_cism_ICESHEET`` (for each ice sheet --- e.g., ``user_nl_cism_ais``, ``user_nl_cism_gris``) or
``user_nl_clm``; this is described in more detail below. The
various ``user_nl_xxx`` files are created when you first run ``case.setup`` for your
case. They can be modified any time between running ``case.setup`` and the start of the
run: the model does NOT need to be rebuilt after making namelist changes in these files.

Most parameters directly relevant to ice sheet modeling are set in ``cism.ICESHEET.config`` for each ice sheet. This
config file contains settings used by CISM --- for example, grid information (which is set
automatically based on the resolution set at ``create_newcase`` time) and physics
parameter settings. The ``cism_in`` file contains some additional parameters controlling
the CISM run. All of the available settings are given in `this table
<http://www.cesm.ucar.edu/models/cesm2.0/component_settings/cism_nml.html>`__ (variables in
namelist groups with ``config`` in their name will go into the ``cism.ICESHEET.config`` files;
others will go into the ``cism_in`` file).

The file ``lnd_in`` provides settings for CLM. CLM's available settings are given in `this
table <https://www.cesm.ucar.edu/models/cesm2/settings/current/clm5_0_nml.html>`__.

``user_nl_cism`` is used for two purposes: (1) settings that appear in ``cism_in``, which apply to all ice sheets; and (2) settings that appear in the ice sheet-specific ``cism.ICESHEET.config`` files, for which you want to make the same change for all ice sheets. Changes to either of those types of settings can be made by adding lines with the
following format to the ``user_nl_cism`` file in your case directory:

.. code-block:: console

    namelist_variable = value

Note that there is no distinction in ``user_nl_cism`` between variables that will appear
in ``cism_in`` vs. those that will appear in the ``cism.ICESHEET.config`` files: CISM's build-namelist
utility knows where each variable belongs. For example, to set the value of ``cism_debug``
to ``.true.`` and ``dt`` to ``0.05`` for all ice sheets, include the following in ``user_nl_cism``:

.. code-block:: console

   cism_debug = .true.
   dt = 0.05

After running ``preview_namelists``, the following will appear in ``cism_in``:

.. code-block:: console

   &cism_params
   ...
   cism_debug = .true.
   ...
   /

and the following will appear in ``cism.ICESHEET.config`` for all ice sheets:

.. code-block:: console

   [time]
   ...
   dt = 0.05
   ...

``user_nl_cism_ICESHEET`` (e.g., ``user_nl_cism_ais``, ``user_nl_cism_gris``) is used for settings that appear in the ice sheet-specific ``cism.ICESHEET.config`` files, for which you want to make a change for just one ice sheet. The syntax for making changes is the same as for ``user_nl_cism``, but now this will only impact the single corresponding ``cism.ICESHEET.config`` file.
   
Changes to ``lnd_in`` can be made by adding similar lines to ``user_nl_clm``.  For
example, to change the ice albedo (values give albedo in the visible and near-infrared),
add the following line to ``user_nl_clm``:

.. code-block:: console

   albice = 0.55,0.45

After changing any of the ``user_nl_xxx`` files, you can preview the generated namelists
by running the ``preview_namelists`` utility in the case directory. Generated namelists
will then appear in the CaseDocs subdirectory of your case as well as in the run
directory.

Note: There appears to be a bug in the parsing of strings in ``user_nl_cism`` that are
bound for ``cism.ICESHEET.config``: These appear to be handled correctly if they are single-quoted,
but double-quoted strings lead to buggy behavior.

=======================
 Modifying source code
=======================

Source code directory structure and repositories
------------------------------------------------

Within a CESM checkout, after all components are obtained with the ``manage_externals``
tool, CISM source code can be found in the directory ``components/cism``. The source code
for CISM within CESM is contained in two separate git repositories:

- The code for CISM itself is in the directory rooted at
  ``components/cism/source_cism``. This is a git repository associated with the `CISM
  GitHub repository <https://github.com/escomp/cism>`_.

- The wrapper code for connecting CISM with CESM is in the other subdirectories of
  ``components/cism``. This top-level directory is a git repository associated with the
  `CISM-wrapper GitHub repository`_.

Other subdirectories of the top-level CISM-wrapper code are:

- ``source_glc``, which contains much of the logic for driving CISM via CESM

- ``drivers``, which contains the code for coupling with CESM via both the NUOPC and MCT coupling
  frameworks

- ``bld``, which mainly consists of code and xml files needed to create the namelist and
  configuration files. Note that the actual build of the model is handled by files in
  ``cime_config``. The most important files in ``bld`` are ``build-namelist`` and the xml
  files in the ``namelist_files`` subdirectory, which describe all possible namelist /
  configuration settings and their default values.

- ``cime_config``, which contains build scripts and configuration files used by `CIME
  <http://esmci.github.io/cime/>`_ (the scripting and infrastructure of CESM).

- ``tools``, which contains tools (now somewhat obsolete) for generating land/ice-sheet
  grid overlap files

- ``test``, which contains code (now somewhat obsolete) for testing some parts of the
  source code

- ``mpi`` and ``serial``, which have appropriate versions of source code that can be used
  for parallel and serial runs, respectively. The ``serial`` directory is obsolete; now
  the ``mpi`` directory is used even when running on a single processor.

- ``manage_externals``, which is the same tool that CESM uses to obtain its
  components. This instance of the tool can be used to obtain a working set of code when
  cloning directly from the `CISM-wrapper GitHub repository`_.

There are two ways to modify source code for a CESM case:

1. Changing code in-place in the source tree

2. Using SourceMods

These two methods are described in more detail below.

Changing code in-place in the source tree
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Changing code in-place allows you to make a git branch of the source code and do your work
leveraging the power and convenience of git. As noted above, there are two separate git
repositories making up CISM within CESM. If you need to modify code both in CISM itself
and in the CISM-wrapper, you will need to create a separate git branch for each (as well
as a separate GitHub fork of each repository if you want to push your work up to GitHub).

This method is best for changes that have some of these characteristics:

- Long-term developments

- Incremental changes towards a final solution

- Changes that apply to many cases

- Changes that touch many files

- Any change intended to eventually come back to master

Using SourceMods
~~~~~~~~~~~~~~~~

Within a case, there is a ``SourceMods`` directory that can contain modified source code
that applies to that specific case. The location for modified CISM files depends on
whether the files are part of the CISM code itself or part of the CISM-wrapper code. For
any source code that appears in the ``source_cism`` subdirectory of ``components/cism``,
the modified file should go in ``$CASEROOT/SourceMods/src.cism/source_cism``. For any
other source code (e.g., modifications to code from ``source_glc``), the modified file
should go directly in ``$CASEROOT/SourceMods/src.cism``. (C++ code in the ``source_cism``
directory can NOT be modified via SourceMods; this C++ code must be modified in-place in
the main source code directory.) Once the modified files are in place, the code can be
rebuilt. The files in the SourceMods directories automatically replace the files of the
same name in the model directories.

This method is suitable for changes that have these characteristics:

- Short-term developments (merging in new changes from master is a pain and very
  error-prone: you don’t have the version control system to help you keep track of your
  changes over time)

- Changes that apply to just one or two cases

- Changes that touch just a few files

.. _CISM-wrapper GitHub repository: https://github.com/escomp/cism-wrapper/
