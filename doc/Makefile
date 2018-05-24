# Minimal makefile for Sphinx documentation
#
# Possible ways to set the build directory:
# (1) Set BUILDDIR from the command line
# (2) Let BUILDDIR be determined as $(BUILDREPO)/cism-in-cesm/$(VERSION)
#     (2a) Set both BUILDREPO and VERSION directly on the command line
#     (2b) Set BUILDREPO on the command line, let VERSION be auto-determined,
#          based on the current git branch name.
#
# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = cismdoc
SOURCEDIR     = source

ifndef BUILDDIR
ifdef VERSION
VERSION_EXPLICIT = 1
else  # ifdef VERSION
VERSION_EXPLICIT = 0
VERSION := $(shell git symbolic-ref --short -q HEAD)
ifeq ($(VERSION),)
$(error Cannot determine version based on git branch; set VERSION on the command line)
endif  # ifeq ($(VERSION),)
endif  # ifdef VERSION
BUILDDIR = $(BUILDREPO)/cism-in-cesm/$(VERSION)
ifeq ($(VERSION_EXPLICIT),0)
# TODO: use a wildcard-based function here? see https://stackoverflow.com/questions/20763629/test-whether-a-directory-exists-inside-a-makefile (may want to test this first in a simple test makefile)
endif  # ifeq ($(VERSION_EXPLICIT),0)
$(info Using build directory: $(BUILDDIR))
endif  # ifndef BUILDDIR

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
