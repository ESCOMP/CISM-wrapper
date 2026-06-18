"""
Substitutions for Sphinx
"""

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.

# pylint: disable=invalid-name

#################################
### Standard Sphinx variables ###
#################################

# General information about the project.
project = u'CESM Land Ice'
copyright = u'2018, Gunter Leguy, William Lipscomb, Bill Sacks'  # pylint: disable=redefined-builtin
author = u'Gunter Leguy, William Lipscomb, Bill Sacks'

# The short X.Y version.
version = u'master'

# The full version, including alpha/beta/rc tags.
release = u'master'

#####################################################
### Custom variables needed for doc-builder setup ###
#####################################################

# Version label used at the top of some pages.
version_label = "the latest development code"

#######################################################
### Custom variables optional for doc-builder setup ###
#######################################################

tex_category = "Miscellaneous"

# Used by HTML help builder
htmlhelp = {
    "basename": "cismwrapdocdoc", # Output file base name
}

# Used for LaTeX output
latex = {
    "target_name": "cismwrapdoc.tex",
    "title": "CISM Wrapper Documentation",
    "documentclass": "manual", # howto, manual, or own class
    "category": tex_category,
}

# Used for man_pages and texinfo_documents
mantex = {
    "name": "cismwrapdoc",
    "title": "cismwrapdoc Documentation",
}

# Used for texinfo_documents
tex = {
    "dirmenu_entry": "cismwrapdoc",
    "description": "One line description of project.",
    "category": tex_category,
}

###############################
### Purely custom variables ###
###############################

nonparamfile_disclaimer_md = (
    "**Note:** The values here should be up-to-date with those used in {{version_label}},"
    " but there may be mistakes."
)
nonparamfile_disclaimer_rst = (
    "**Note:** The values here should be up-to-date with those used in |version_label|,"
    " but there may be mistakes."
)
