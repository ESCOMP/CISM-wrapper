.. sectnum::
   :prefix: A.
   :start: 2

.. _new-icesheet:

*****************************
 Introducing a new ice sheet
*****************************

This section describes what is needed when introducing support for a completely new ice sheet --- i.e., an ice sheet other than Greenland or Antarctica. If you are instead adding a new grid for an existing ice sheet (e.g., a new Greenland or Antarctica grid), see :ref:`new-grids`.

=============================================
Changes needed in the CISM-wrapper repository
=============================================

The following changes are needed so that you can create a compset with a new ice sheet and have an appropriate ``cism.ICESHEET.config`` file generated for the new ice sheet:

#. Add an entry to the ``_ICESHEET_OPTIONS`` variable in the ``buildnml`` file (in the ``cime_config`` directory of the CISM-wrapper repository). Each ice sheet has two names associated with it: a short, lowercase name (e.g., ``ais``) and a full, uppercase name (e.g., ``ANTARCTICA``). Both names are given in the ``_ICESHEET_OPTIONS`` variable. The short name is used in the grid specification, the ``user_nl_cism`` file name, the config file name, and various other file names; the full name is used in some XML variables. **It is important that the ordering of ice sheets in _ICESHEET_OPTIONS is consistent with the ordering of grids in the config_grids xml file (e.g., if the Antarctica grid is listed before the Greenland grid in grids defined in that file, then Antarctica must appear before Greenland in this variable). Also, avoid having one ice sheet's name be a prefix of another (e.g., "gris" and "grisa").**

#. Add entries in ``cime_config/config_component.xml`` giving XML variables for the new ice sheet and their relationship with compset long names. Follow the examples of other ice sheets in that file (e.g., do a case-insensitive search for "Antarctica" in that file).

#. Add entries in ``cime_config/namelist_definition_cism.xml`` to add default configuration values for this ice sheet for any variables whose defaults differ between ice sheets. At a minimum, look for any variable in that file that already has a ``<value>`` entry with an ``icesheet`` attribute --- e.g., ``<value icesheet="gris">`` or ``<value icesheet="ais">`` --- and add a new entry for the new ice sheet; sometimes a specific grid might also be needed as an additional attribute. Optionally, there may be some additional variables that currently have the same default value for all ice sheets but for which you want a different default for the new ice sheet; for these, you can replace the single ``<value>`` entry with new ``<value icesheet="xxx">`` entries for each ice sheet.

#. Add entries in ``cime_config/config_archive.xml``: add a new rpointer section for the new ice sheet, as well as rpointer test file entries for this new ice sheet. (You can search for "ais" in that file to see what is needed.)

#. [OPTIONAL] Add one or more new compset aliases in ``cime_config/config_compsets.xml`` using the new ice sheet. Note that only T compsets are defined here; other compsets are defined in other ``config_compsets.xml`` files elsewhere in CESM.

#. [OPTIONAL] Add one or more default processor layouts in ``cime_config/config_pes.xml`` for the new ice sheet. Note that only processor layouts for T compsets are defined here; processor layouts for other compsets are defined in other ``config_pes.xml`` files elsewhere in CESM.

====================================
Changes needed in other repositories
====================================

A number of changes are also needed in other repositories to specify the new CISM grid(s). (See also :ref:`new-grids`.)

..
    In the following references to the ccs_config_cesm repository, I don't mention the specific file(s) to modify because this new repository is in a state of flux, so any reference to specific files may soon be out of date.

#. In the appropriate XML file in https://github.com/ESMCI/ccs_config_cesm add one or more grid definitions for this ice sheet. This involves adding a ``<domain>`` entry (which specifies the dimensions of the grid and an ESMF mesh file that defines the grid) and one or more ``<model_grid>`` entries (which specify grid aliases that can be used to run CISM with this grid together with certain atmosphere/land and ocean grids, possibly in conjunction with other CISM grids if you want to run multiple ice sheets in a single simulation). As with other changes, it is easiest to follow the example of an existing CISM grid; for example, search for "ais8" to see what your additions should look like.

#. [OPTIONAL] To enable fully-coupled (B compset) runs, additional changes are needed in https://github.com/ESMCI/ccs_config_cesm to specify runoff mapping files from the new CISM grid(s) to the desired ocean grid(s). (Mapping files between CISM and other components do not need to be specified, since they will be generated at runtime.) Once again, search for "ais8" to see what your additions should look like.

#. In CTSM, set up an appropriate glacier region (see :ref:`new-grids` for details).

===================================
Caveats when adding a new ice sheet
===================================

**In a run with multiple ice sheets, it is very important that the grids for the different ice sheets do not overlap.** In fact, the restriction is a bit more stringent: **There cannot be any land grid cell that intersects with multiple CISM domains.** This is because the lnd-glc mapping algorithms in CMEPS cannot handle the situation where a given land grid cell intersects with multiple CISM domains. This requirement is NOT checked in the code: it is the user's responsibility to ensure that it is satisfied.
