#=======================================================================
# x265-hw Makefile for bsc-compile
#-----------------------------------------------------------------------
# Copyright (c) 2015, Min Chen
# All rights reserved.
#

default : all

topdir  = ..

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

# Library components

bsvclibsrcs = 

# Bluespec sources

top_module = mkTb

srcdir = $(topdir)/src
verdir = $(topdir)/src_ver
cppdir = $(topdir)/src_cpp

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

BSC_COMP = bsc
BSC_OPTS = -u -show-module-use -verilog -aggressive-conditions \
           -relax-method-earliness -relax-method-urgency -v

VER_OPTS =  +RTS -K100000000 --RTS -u -v -verilog -aggressive-conditions
SIM_OPTS =  +RTS -K100000000 --RTS -u -v -sim -aggressive-conditions

rtl : 
	$(BSC_COMP) $(VER_OPTS) -bdir ./ -p +:$(srcdir) -g $(top_module)

sim : 
	$(BSC_COMP) $(SIM_OPTS) -bdir ./ -p +:$(srcdir):$(verdir)/ -g $(top_module) $(reldir)/$(top_module).bsv


#--------------------------------------------------------------------
# Default make target
#--------------------------------------------------------------------

all : rtl

#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------

clean :
	rm -rf $(junk) *~ \#* *.cxx *.ba *.o *.h *.bi *.bo
 
