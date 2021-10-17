"""Base class for tests comparing a multi-ice sheet run against a single ice sheet run

This is a CISM-specific test:
(1) Does a run with multiple ice sheets
(2) Does a run where one of those ice sheets is removed (so, if (1) had two ice sheets,
    then (2) would have a single ice sheet)

Verifies that the remaining ice sheet(s) are bit-for-bit according to CISM history files
(other history files should be turned off in this test).

"""

from CIME.SystemTests.system_tests_compare_two import SystemTestsCompareTwo
from CIME.SystemTests.test_utils.user_nl_utils import append_to_user_nl_files
from CIME.XML.grids import GRID_SEP
from CIME.XML.standard_module_setup import *

logger = logging.getLogger(__name__)

class MULTIVSSINGLE(SystemTestsCompareTwo):

    def __init__(self, case, remove_icesheet_xml_name, remove_icesheet_grid_name):
        """Initialize this test instance

        The following specify the ice sheet to turn off:
        - remove_icesheet_xml_name is the name of the icesheet in xml variables like CISM_USE_* (e.g., GREENLAND)
        - remove_icesheet_grid_name is the name of the icesheet in the grid name (e.g., gris)
        """
        self._remove_icesheet_xml_name = remove_icesheet_xml_name
        self._remove_icesheet_grid_name = remove_icesheet_grid_name

        SystemTestsCompareTwo.__init__(self, case,
                                       separate_builds=False,
                                       run_two_suffix="single",
                                       run_one_description="multiple ice sheets",
                                       run_two_description="{} ice sheet removed".format(remove_icesheet_grid_name))

    def _common_setup(self):
        # Turn off history for CLM: We expect answer changes for CLM in the part of the
        # world where we turn off one of the ice sheets in the single-ice sheet case.
        append_to_user_nl_files(caseroot = self._get_caseroot(),
                                component = "clm",
                                contents = "hist_empty_htapes = .true.")

    def _case_one_setup(self):
        # Turn off history output for the given ice sheet in the multi-ice sheet case:
        # otherwise, the comparison between the cases will fail due to a history file
        # being present in one case but not the other.
        append_to_user_nl_files(caseroot = self._get_caseroot(),
                                component = "cism_{}".format(self._remove_icesheet_grid_name),
                                # setting history_frequency to 10000 years should be effectively the same as 'never'
                                contents = "history_frequency = 10000")

    def _case_two_setup(self):
        # Turn off the given ice sheet
        self._case.set_value("CISM_USE_{}".format(self._remove_icesheet_xml_name), "FALSE")
        self._case.set_value("CISM_EVOLVE_{}".format(self._remove_icesheet_xml_name), "FALSE")

        # Remove the given ice sheet from GLC_GRID and related variables
        glc_grid = self._case.get_value("GLC_GRID")
        glc_grid_list = glc_grid.split(GRID_SEP)
        expect(len(glc_grid_list) >= 2, "expect at least two GLC grids")
        glc_domain_mesh = self._case.get_value("GLC_DOMAIN_MESH")
        glc_domain_mesh_list = glc_domain_mesh.split(GRID_SEP)
        glc_grid_list_new = []
        glc_domain_mesh_list_new = []
        for i, one_grid in enumerate(glc_grid_list):
            if not one_grid.startswith(self._remove_icesheet_grid_name):
                glc_grid_list_new.append(one_grid)
                glc_domain_mesh_list_new.append(glc_domain_mesh_list[i])
        expect(len(glc_grid_list_new) == (len(glc_grid_list) - 1),
               "Expected exactly one grid to be removed")
        glc_grid_new = ":".join(glc_grid_list_new)
        glc_domain_mesh_new = ":".join(glc_domain_mesh_list_new)
        self._case.set_value("GLC_GRID", glc_grid_new)
        self._case.set_value("GLC_DOMAIN_MESH", glc_domain_mesh_new)
