#
# Makefile for HelloWorld example
#

FC              = gfortran

# My dirs
BLDDIR := $(shell pwd -P)
MKFILE_PATH := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
ROOT = $(shell dirname $(shell dirname $(MKFILE_PATH)))
CAPGEN := $(ROOT)/scripts/ccpp_capgen.py
CAPGEN_ARGS := --host-files $(MKFILE_PATH)/hello_world_host.meta
CAPGEN_ARGS += --scheme-files $(MKFILE_PATH)/hello_scheme.meta
CAPGEN_ARGS += --suites $(MKFILE_PATH)/hello_world_suite.xml
CAPGEN_ARGS += --generate-host-cap --host-name HelloWorld
SRCS_META := $(MKFILE_PATH)/hello_world_host.meta
SRCS_META += $(MKFILE_PATH)/hello_scheme.meta
SRCS_XML := $(MKFILE_PATH)/hello_world_suite.xml

INCFLAG = -I
INCPATH += $(INCFLAG)$(BLDDIR) $(INCFLAG)$(MKFILE_PATH)
FCFLAGS += -g

# SOURCE FILES
SRCS_F90 = $(HELLO_SCHEME).F90 $(HELLO_HOST).F90
OBJS_F90 = hello_scheme.o hello_world_host.o

.PHONY: all
all: HelloWorld

HelloWorld: capfiles.txt $(CAPOBJS) $(OBJS_F90)
	$(FC) $(FCFLAGS) -o $@ $(CAPOBJS) $(OBJS_F90)

capfiles.txt: $(SRCS_META) $(SRCS_XML)
	$(CAPGEN) $(CAPGEN_ARGS)
	$(shell $(MKFILE_PATH)/mkcaplocal.sh $(BLDDIR)/capfiles.txt)
	$(eval CAPOBJS=$(shell cat caplocal.txt))

.F90.o:
	$(FC) -c $(INCPATH) $(FCFLAGS) $<

.PHONY: clean
clean:
	rm -f *.o *.mod HelloWorld capfiles.txt caplocal.txt \
              $(shell if [ -f capfiles.txt ]; then cat capfiles.txt; fi)

ccpp_kinds.F90: capfiles.txt

ccpp_kinds.o: ccpp_kinds.F90

HelloWorld_ccpp_cap.o: ccpp_kinds.o HelloWorld_ccpp_cap.F90

ccpp_hello_world_cap.o: ccpp_kinds.o ccpp_hello_world_cap.F90

hello_scheme.o: ccpp_kinds.o $(MKFILE_PATH)/hello_scheme.F90

hello_world_host.o: ccpp_kinds.o $(MKFILE_PATH)/hello_world_host.F90
