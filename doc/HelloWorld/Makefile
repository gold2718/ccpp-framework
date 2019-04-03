#
# Makefile for HelloWorld example
#

SHELL           = /bin/bash

# My dirs
MKFILE_PATH := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
ROOT = $(shell dirname $(shell dirname $(MKFILE_PATH)))
CAPGEN := $(ROOT)/scripts/ccpp_capgen.py
CAPGEN_ARGS = --host-files $(MKFILE_PATH)/hello_world_host.meta
CAPGEN_ARGS += --scheme-files $(MKFILE_PATH)/hello_scheme.meta
CAPGEN_ARGS += --suites $(MKFILE_PATH)/hello_world_suite.xml
CAPGEN_ARGS += --generate-host-cap --host-name HelloWorld

INCFLAG = -I
INCPATH += $(INCFLAG). $(INCFLAG)$(MKFILE_PATH)
FCFLAGS += -g

# SOURCE FILES
SRCS_F90 = $(MKFILE_PATH)/hello_scheme.F90 $(MKFILE_PATH)/hello_world_host.F90
SRCS_META = $(SRCS_F90:.F90=.meta)
OBJS_F = $(SRCS_F90:.F90=.o)

.PHONY: all
all: capfiles.txt
	echo $(CAPSOURCE)

capfiles.txt: $(SRCS_META)
	echo $(CAPGEN) $(CAPGEN_ARGS)
	$(CAPGEN) $(CAPGEN_ARGS)
	CAPSOURCE = $(shell cat capfiles.txt)

.PHONY: clean
clean:
	rm -f $(CAPSOURCE) *.o *.mod HelloWorld capfiles.txt
