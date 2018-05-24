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
# If version isn't set on the command-line, then we set it here, by
# getting the current git branch name.
VERSION ?= $(shell git symbolic-ref --short -q HEAD)
ifeq ($(VERSION),)
$(error Cannot determine version based on git branch; set VERSION on the command line)
endif
BUILDDIR = $(BUILDREPO)/cism-in-cesm/$(VERSION)
$(info Using build directory: $(BUILDDIR))
endif

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
