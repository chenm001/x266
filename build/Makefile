#=======================================================================
# x266-hw Makefile for bsc-compile
#-----------------------------------------------------------------------
# Copyright (c) 2015, Min Chen
# All rights reserved.
#

default : all

basedir   = $(shell pwd)/..
builddir  = $(basedir)/build

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

# Library components

bsvclibdir  =
bsvclibsrcs =

# Bluespec sources

top_module = mkTb

srcdir = $(basedir)/src
objdir = $(basedir)/src_gen/obj
verdir = $(basedir)/src_gen/ver
simdir = $(basedir)/src_gen/obj_sim
exedir = $(builddir)/bin
tstdir = $(basedir)/test
libdir = $(basedir)/lib
dpidir = $(basedir)/src_tb
cppdir = $(basedir)/src_cpp
exefile = h266enc
tstfile  = $(notdir $(wildcard $(tstdir)/thyuv/*.yuv))
libfile = #$(libdir)/xilinx/glbl.v $(libdir)/xilinx/RAMB16BWER.v

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

BSC_COMP = bsc
#BSC_OPTS = -u -show-module-use -verilog -keep-fires -aggressive-conditions \
#           -relax-method-earliness -relax-method-urgency -v +RTS -K100000000 --RTS \
#           -show-schedule -v -remove-unused-modules -no-runtime-license -cross-info \
#           -keep-inlined-boundaries -opt-undetermined-vals -unspecified-to 'X'

BSC_OPTS  = -u -aggressive-conditions -bdir $(objdir) -vdir $(verdir) -simdir $(simdir) -p +:$(srcdir)
#BSC_OPTS += +RTS -K1024M -RTS
BSC_OPTS += -keep-fires
VER_OPTS  =  -verilog -opt-undetermined-vals -unspecified-to 'X' #-verilog-filter ./my_post.pl
FSDB_OPTS =  -Xv +64bit #-D BSC_FSDB -Xv -L$(NOVAS_HOME)/share/PLI/IUS/LINUX64/boot/ -Xv +ncloadpli1=$(NOVAS_HOME)/share/PLI/IUS/LINUX64/boot/debpli.so:debpli_boot
SIM_OPTS  =  -sim

rtl :
	$(BSC_COMP) $(VER_OPTS) -bdir ./ -p +:$(srcdir) -g $(top_module)

sim :
	$(BSC_COMP) $(SIM_OPTS) -bdir ./ -p +:$(srcdir):$(verdir)/ -g $(top_module) $(reldir)/$(top_module).bsv

%_v: $(srcdir)/%.bsv mkdirs
	$(BSC_COMP) $(BSC_OPTS) $(VER_OPTS) $<

%_tb: $(srcdir)/%.bsv mkdirs
	$(BSC_COMP) $(BSC_OPTS) $(SIM_OPTS) -D $(subst _tb,,TEST_BENCH_$@) $<
	$(BSC_COMP) $(BSC_OPTS) $(SIM_OPTS) -e mkTb $(objdir)/*.ba $(dpidir)/*.c

%_vtb: $(srcdir)/%.bsv mkdirs
	$(BSC_COMP) $(BSC_OPTS) $(VER_OPTS) -D $(subst _vtb,,TEST_BENCH_$@) $<
	$(BSC_COMP) $(BSC_OPTS) $(VER_OPTS) $(FSDB_OPTS) -D BSV_TIMESCALE=1ns/1ns -e mkTb $(verdir)/*.v $(libfile) $(objdir)/*.ba $(dpidir)/*.c

%_c: $(srcdir)/%.bsv mkdirs
	$(BSC_COMP) $(BSC_OPTS) $(SIM_OPTS) $<

mkdirs :
	@mkdir -p $(objdir) $(verdir) $(simdir)

#--------------------------------------------------------------------
# Files build rules
#--------------------------------------------------------------------
%bo : $(srcdir)/%bsv mkdirs
	@mkdir -p ver
	$(BSC_COMP) $(BSC_OPTS) $(VER_OPTS) $<





#--------------------------------------------------------------------
# Default make target
#--------------------------------------------------------------------

all : rtl

#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------

clean :
	$(RM) -rf $(junk) *~ \#* *.cxx *.ba *.o *.h *.bi *.bo *.log *.so
	$(RM) -rf $(exedir) $(objdir) $(simdir) $(verdir) csrc
	$(RM) -rf a.out* INCA_libs*
	$(RM) -rf novas.rc *.fsdb *.vcd
