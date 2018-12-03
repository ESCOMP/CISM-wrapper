.. _clm-cism-coupling:

************************
Coupling CLM and CISM
************************

TODO: This section is under construction.  Several section heads are just placeholders for now.


==============================
Modes of coupling CLM and CISM
==============================


One-way (diagnostic) coupling
-----------------------------


Two-way (interactive) coupling
------------------------------


===================================
Brief overview of CLM-CISM coupling
===================================



=====================================
Fields exchanged between CLM and CISM
=====================================

CLM to CISM
-----------


CISM to CLM
-----------


Other fields sent from CISM
---------------------------



=====================================
Remapping fields between CLM and CISM
=====================================


Remapping surface mass balance from CLM to CISM
-----------------------------------------------

As described above, the surface mass balance (SMB) of ice sheets is computed by CLM
for each column (i.e., elevation class) of each glaciated landunit in each grid cell on the land grid.
The SMB is then remapped by the coupler to the finer ice sheet grid and passed to CISM.
When CESM is run with two-way, interactive coupling between glaciers and ice sheets, we want to conserve
the total amount of water in the system, while also mapping SMB smoothly and accurately between grids.

Specifically, we would like the SMB remapping to satisfy the following requirements:

1. ``Conservation``: For any ice sheet defined by a CISM domain, the sum over CLM grid cells of the SMB sent to the coupler
   is equal (within machine roundoff) to the sum over CISM grid cells of the SMB received from the coupler.
   Note that this is a global (i.e., whole-ice-sheet) rather than a local requirement.

2. ``Smoothness``: The remapping is smooth and continuous on the CISM grid, without obvious imprinting of the coarser CLM grid.

3. ``Accuracy``: The SMB applied in CISM at a given location is close to the value computed by CLM at that location
   and elevation.

4. ``Sign preservation``: Any positive SMB in CLM maps to a positive SMB in CISM, and likewise for negative SMB.

Here we describe the algorithm used by the coupler to satisfy these requirements.  First we introduce some notation:

- ``lfrac`` is the fraction of a CLM grid cell that does not overlap the ocean grid and is treated as land.
  Since the ocean and land grids are non-conforming, we can have ``0 < lfrac < 1`` in CLM cells near the ocean boundary.

- ``Sg_icemask_g`` is a binary mask on the CISM grid that identifies cells which are ice-covered and/or land-covered,
  and therefore are eligible to apply a nonzero SMB from CLM.  (Ice-free land cells can have a positive SMB,
  and ice-covered cells can have an SMB of either sign.)  CISM cells that are ice- and/or land-covered have
  ``Sg_icemask_g = 1``, and ice-free ocean cells have ``Sg_icemask_g = 0``.

- ``Sg_icemask_l`` is obtained by mapping ``Sg_icemask_g`` from the CISM grid to the CLM grid.
  Since the grids are different, this mask is not binary; we can have ``0 < Sg_icemask_l < 1``.

- ``g = min(lfrac, Sg_icemask_l)`` is the fraction of CLM-computed SMB that is sent to CISM via the coupler.
  The remaining SMB is not sent to CISM.  A fraction ``lfrac - g`` is sent by the coupler to the runoff model;
  this is the fraction of the cell that is land-covered but does not overlap the CISM grid.  The remaining
  fraction, ``1 - lfrac``, is not sent to either CISM or the runoff model, because any precipitation in
  the non-land part of a CLM cell has already fallen into the ocean.

- :math:`A_i` is the area of a CLM grid cell.  CLM and the coupler agree on the grid cell area.

- :math:`A_j` is the area of a CISM grid cell according to CISM, and :math:`A_j^c` is the area according to the coupler.
  These two areas differ because CISM's stereographic projection does not conserve area.

- :math:`f_{ik}` is the fraction of CLM grid cell *i* occupied by glacier ice in elevation class *k*.

- :math:`q_{ik}` is the SMB of CLM grid cell *i* in elevation class *k*.

- :math:`q_j` is the SMB remapped to CISM grid cell *j*.

Using this notation, we can express the conservation requirement (1):

.. math::
   :label: conservation

   \sum_i{g_i A_i \sum_k{f_{ik} q_{ik}}} = \sum_j{A_j q_j},

where the sum on the LHS is taken over grid cells *i* and columns *k* on the CLM grid, and
the sum on the RHS is taken over grid cells *j* on the CISM grid.

To additionally satisfy sign preservation (4), Eq. :eq:`conservation` is replaced by two equations:
one for the accumulation zone (limited to cells and columns with :math:`q > 0`),
and one for the ablation zone (limited to cells and columns with :math:`q < 0`).

Requirements (2) and (3) are ensured by bilinear remapping in the horizontal plane combined
with linear interpolation in the vertical. These operations are smooth but not conservative.
Thus, in order to satisfy all four requirements, bilinear remapping and vertical interpolation
are followed by a normalization step that guarantees conservation in both the accumulation and ablation zones.

The algorithm proceeds as follows:

1. In CLM, compute the SMB for each grid cell and elevation class (EC) that has nonzero overlap (:math:`g > 0`)
   with the CISM domain, and send to the coupler.

2. Accumulate and average the SMB for each EC over the CLM-CISM coupling interval
   (typically 1 year).

3. At the end of the coupling interval, compute the total SMB in the accumulation and ablation zones of CLM.

4. For each EC, do a bilinear remapping of SMB from the CLM grid to the CISM grid.

5. For each CISM grid cell, do a linear interpolation in elevation space between adjacent ECs, to compute
   the SMB at the CISM cell elevation.  If a cell lies above or below the range of elevations in the
   various ECs, values from the highest and lowest ECs are extrapolated.  *Note: State whether this 
   is a linear extrapolation from the two highest and lowest ECs, or simply an extension of the highest and lowest values.*

6. Compute the total (uncorrected) SMB in the accumulation and ablation zones of CISM.

7. Apply a normalization correction for conservation.  For example, suppose
   :math:`Q_{\text{acc}}^{\text{clm}} = 1.05 \, Q_{\text{acc}}^{\text{cism}}`,
   where :math:`Q_{\text{acc}}` is the total SMB in the accumulation zone of a given model.
   Then in every CISM cell that lies in the accumulation zone, we would multiply the SMB by
   :math:`Q_{\text{acc}}^{\text{clm}}\, / \, Q_{\text{acc}}^{\text{cism}} = 1.05` (and similarly for the ablation zone).

8. Send the normalized SMB on the CISM grid to CISM.

Step 1 is done in CLM at every time step.  The other steps are done in the coupler, with steps 3-8
carried out at the end of the coupling interval.

In practice, normalization factors usually fall between 0.9 and 1.1 at typical CESM global grid resolutions
of :math:`\sim 1^\circ`.  Thus, if an SMB of 1 m/yr is computed in CLM, the downscaled SMB in CISM might differ
by up to 10%.  If we used conservative rather than bilinear remapping, differences also would be up to about 10%,
because of area distortions on CISM's polar stereographic grid.
Thus the local errors for bilinear remapping and renormalization are similar to the local errors for conservative remapping.
Bilinear remapping, however, is far smoother; smoothness is obtained at the cost of local conservation.
