.. _b-compsets:

**************************************************************
Running the fully-coupled ice sheet model within CESM: B compsets
**************************************************************

CISM is available to run as part of a fully coupled climate system 
simulation in CESM 2.0. Runs with all active components are known 
as "B compsets", such as the "B1850" compset which runs with all 
active components and repeated pre-industrial forcing. Basic B 
compsets run with CISM in no-evolve mode, and compsets that end 
with G (e.g., "B1850G") have an active, or evolving, CISM 
Greenland ice sheet. 

Any compset, including B compsets, can turn on an actively evolving 
CISM ice sheet model using the xml variable CISM_EVOLVE to TRUE.

The sections below give links to information about CISM's coupling, 
and how to start and run a CESM case in general. After the links 
are descriptions of CISM's time stepping options, including how to 
accelerate the ice sheet relative to the rest of the CESM components, 
which is a popular way to simulate many fully-coupled ice sheet years. 

In a fully-coupled simulation, CISM sends fluxes to the land, ocean 
and river run-off models, but not to the atmosphere model directly. 
In long climate simulations, the changing topography of the ice 
sheet can create a non-negligable forcing on the atmosphere, so a 
method of updating the atmosphere topography to reflect the current 
shape of the ice sheet has been developed. That method is outlined 
below. 

===========
Helpful links for coupling information
===========

- `Details of CISM, CLM and POP coupling <https://escomp.github.io/cism-docs/cism-in-cesm/versions/release-cesm2.0/html/clm-cism-coupling.html#>`_
- `Generating Forcing Data to use in a CISM T compset <https://escomp.github.io/cism-docs/cism-in-cesm/versions/release-cesm2.0/html/t-compsets.html#performing-a-run-to-create-forcing-data>`_
- `CESM Case Control System (how to setup, build, and run an experiment with CESM 2) <https://esmci.github.io/cime/versions/master/html/users_guide/index.html>`_
- `User questions and answers in the DiscussCESM Forums <https://bb.cgd.ucar.edu/cesm/>`_

===========
CISM Time Steps
===========

There are a few different kinds of timesteps in CISM:

1. The *forcing timestep* is the interval in hours between calls to
   Glad. Currently, the forcing timestep is the same as the *coupling
   interval* at which information is passed from the coupler to GLC. The
   forcing timestep is determined by the CISM namelist variables
   *dt\_option* and *dt\_count*. It is 24 hours by default for most
   compsets, but 1 year for T compsets. Note that these are the only
   values that have been tested extensively; results should be checked
   carefully if the forcing timestep is changed from these defaults.

2. The *mass balance timestep* is the interval over which
   accumulation/ablation forcing data is summed and averaged. This
   timestep is set in subroutine *glad\_mbc\_init* in module
   *glad\_mbal\_coupling.F90*. The current default is one year. With the
   default settings of the forcing timestep and mass balance timestep, Glad
   will accumulate forcing data from the coupler over 365 daily forcing
   timesteps and average the data. The mass balance timestep must be an
   integer multiple of the forcing timestep.

3. The *ice sheet timestep* is the interval in years between calls to
   the dynamic ice sheet model, Glissade. The ice sheet timestep should
   divide evenly into the mass balance timestep. The current default is
   0.1 year for 4-km, and 0.5 year for 20-km.

Two optional runtime parameters can be used to make the time-stepping
more intricate:

1. The mass balance accumulation time, *mbal\_accum\_time* (in years),
   is the period over which mass balance information is accumulated
   before calling Glissade. By default, the mass balance accumulation time
   is equal to either the ice sheet timestep or the mass balance
   timestep, whichever is larger. (For current defaults, this means that
   *mbal\_accum\_time* is set equal to the mass balance timestep: 1
   year.) But suppose, for example, that the ice sheet timestep is 5
   years. If we set mbal\_accum\_time = 1.0, we accumulate mass balance
   information for 1 year and use this mass balance to force the ice
   sheet model, thus avoiding 4 additional years of accumulating mass
   balance data. **Note that this parameter cannot currently be modified
   via *user\_nl\_cism*, because it is not recommended that users change
   it.**

2. The timestep multiplier\ *, ice\_tstep\_multiply*, is equal to the
   number of ice sheet timesteps executed for each accumulated mass
   balance field. Suppose that the mass balance timestep is 1 year, the
   ice sheet timestep is 1 year, and ice\_tstep\_multiply = 10. Glad
   will accumulate and average mass balance information for 1 year, then
   execute 10 ice sheet model timesteps of 1 year each. In other words,
   the ice sheet dynamics is accelerated relative to the land and
   atmosphere. This option may be useful in CESM for multi-millennial
   ice-sheet simulations where it is impractical to run the atmosphere
   and ocean models for more than a few centuries.

The *ice\_tstep\_multiply* namelist parameter is the single change needed 
to accelerate the ice sheet model relative to the rest of CESM. Previous 
experiments have run with *ice\_tstep\_multiply* set to 5 or 10, which 
accelerates the ice sheet to 5x or 10x the speed of CESM. So, if 
*ice\_tstep\_multiply* = 5, then for every 100 CESM years of simulation 
run, 500 CISM years are completed.

For each accelerated coupling period, the CISM output fluxes are averaged 
and returned once to the coupler. So, instead of one year of fluxes, CESM 
will see the average of 5 years from CISM if *ice\_tstep\_multiply* = 5. 

Similarly, the CISM history fields are averaged over the accelerated 
timestep, but only written once at the end of the CESM year. So, in a 100 
year CESM simulation with *ice\_tstep\_multiply* = 5, you will have 100 
CISM history files, representing 500 years of CISM evolving ice sheet 
simulation. 

Changing the CISM timestep acceleration does not change the orbital forcing 
in CESM, however. To accelerate the orbital forcings as well, there are 
currently namelist and source modifications needed. Contact a LIWG scientist 
for more information about these.

The time options below (apart from the forcing timestep) are set in
*cism.config*. This file contains (or may contain) the following
timestep information:

1. The ice sheet timestep *dt* (in years) is set in the section
   [*time*\ ] in the ice config file.

2. The mass balance time step is not set directly in the config file,
   but is set to the number of hours in a year (i.e., 8760 hours
   for a 365-day year).

3. The values of *ice\_tstep\_multiply* and *mbal\_accum\_time*, if
   present, are listed in the section [*GLAD climate*\ ].

Note that the total length of the simulation is not determined by
CISM, but is set in the file *env\_run.xml* in the case directory.

===========
CISM Topography Updating Workflow
===========

** These instructions require CESM 2.1.1 or greater. **

1. Edit your ``config_workflow.xml`` file. This is found in ``cime/config/cesm/machines`` . You will need to add the following code to this file anywhere after a ``</workflow_jobs>`` tag. ::

  <workflow_jobs id="topo_regen_10yr_cycle">
    <!-- order matters, jobs will be run in the order listed here -->
    <job name="case.run">
      <template>template.case.run</template>
      <prereq>$BUILD_COMPLETE and not $TEST</prereq>
    </job>
    <job name="case.test">
      <template>template.case.test</template>
      <prereq>$BUILD_COMPLETE and $TEST</prereq>
    </job>
    <job name="case.topo_regen">
      <template>$EXEROOT/../run/dynamic_atm_topo/template.topo_regen</template>
      <!-- If case.run (or case.test) exits successfully then run topo_regen-->
      <dependency>case.run or case.test</dependency>
      <prereq>1</prereq>
      <runtime_parameters>
        <task_count>1</task_count>
        <tasks_per_node>1</tasks_per_node>
        <walltime>0:45:00</walltime>
      </runtime_parameters>
    </job>
    <job name="case.st_archive">
      <template>template.st_archive</template>
      <!-- If case.topo_regen exits successfully then run st_archive-->
      <dependency>case.topo_regen</dependency>
      <prereq>$DOUT_S</prereq>
      <runtime_parameters>
        <task_count>1</task_count>
        <tasks_per_node>1</tasks_per_node>
        <walltime>0:20:00</walltime>
      </runtime_parameters>
    </job>
  </workflow_jobs>


2. Create your case. When you create your case you will need to add the flag ``--workflow topo_regen_10yr_cycle`` . For example: ::

     ./create_newcase --case Test_topo_regen_workflow_m03 --compset B1850G --res f09_g17_gl4 --workflow topo_regen_10yr_cycle --project P93300606 --run-unsupported

3. Go into your new case directory and run ``./case.setup`` you should see a warning that says "Input template file /glade/scratch/katec/Test_topo_regen_workflow_m03/bld/../run/dynamic_atm_topo/template.topo_regen for job case.topo_regen does not exist or cannot be read." If you don't see a warning like this for your case than something has gone wrong. Check that you did the first two steps correctly.

4. If you do get the warning, now it's time to get the topography updating tools. Go to your run directory (so, for the above example case, ``cd /glade/scratch/katec/Test_topo_regen_workflow_m03/run`` and in that directory type: ::

     > svn co https://svn-ccsm-models.cgd.ucar.edu/tools/dynamic_cam_topography/trunk dynamic_atm_topo

This will checkout the topography updater into the "dynamic_atm_topo" subdirectory.

5. Now type ``cd dynamic_atm_topo/bin_to_cube`` and type ``make``. This will build that tool. When it's done type ``cd ../cube_to_target`` and type ``make``. This will build the other tool.

6. Go back to your case directory. Type ``./case.setup --reset`` and now you should see it say: ::

     Writing case.topo_regen script from input template /glade/scratch/katec/Test_topo_regen_workflow_m04/bld/../run/dynamic_atm_topo/template.topo_regen

     Creating file .case.topo_regen

7. Build your case (type ``qcmd -- ./case.build`` on Cheyenne)

8. Change your run parameters. This workflow will have the topography updater run after each successful case.run segment. So, if your segments are 5 years, then the topography will update every 5 years. Previous experiments ran with 10 year segments and the topography updated every 10 years. So, basically the pattern was:

Run for 10 years, Update Topography, Short Term Archiver

To get this you would need to do these xml commands: ::

  ./xmlchange STOP_N = 10
  ./xmlchange STOP_OPTION=nyears
  ./xmlchange REST_N = 10
  ./xmlchange REST_OPTION=nyears
  ./xmlchange RESUBMIT=9

That will run for 10 segments of 10 years or 100 years with the topography updating every 10 years.

9. Submit your run (type ``./case.submit``). You should see three jobs fired off at the same time. Your run job should be queued and then the topography and archive jobs should be holding in the queue waiting for the completion of the run script.

10. After each segment is complete, you should see a ``topo_regen.log`` file in your case directory. You can give those a quick look-through to make sure that the script ran successfully. The script updates the topography file in the run directory and the cam restart file PHIS field. The restart with the updated field is archived. So, you can go through your restarts and plot the PHIS field to make sure the atmosphere is seeing the evolving topography.
