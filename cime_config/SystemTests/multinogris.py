"""Implementation of the MULTINOGRIS test

This is a CISM-specific test: Compare a multi-ice sheet run with an equivalent run without
Greenland; the other icesheet(s) should be bit-for-bit.
"""

from CIME.XML.standard_module_setup import *

# We can import multivssingle directly because the SystemTests directory has been added to
# sys.path.
from multivssingle import MULTIVSSINGLE

logger = logging.getLogger(__name__)

class MULTINOGRIS(MULTIVSSINGLE):

    def __init__(self, case):
        MULTIVSSINGLE.__init__(self, case,
                               icesheet_xml_name="GREENLAND",
                               icesheet_grid_name="gris")
