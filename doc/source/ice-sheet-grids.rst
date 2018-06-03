.. _ice-sheet-grids:

***************
Ice sheet grids
***************

FIXME: Bill S needs to rewrite this section

Local ice sheet grids must be rectangular; typically they are polar
stereographic projections. For Greenland, three grids are currently
supported, with resolutions of 20 km and 4 km, respectively.
Each local grid is compatible with any of the three supported global
resolutions. The current default is the *gland4* 4 km grid. This can
be changed by modifying CISM\_GRID to the desired value (gland4 or
gland20) in env\_run.xml. A number of configuration defaults depend
on the grid. You can see the effect of changing *CISM\_GRID* by running
*preview\_namelists* before and after making this change. The rules that
determine default values are given in
*models/glc/cism/bld/namelist\_files/namelist\_defaults\_cism.xml*.

