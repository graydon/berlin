# $Id: Makefile,v 1.9 2000/09/23 19:14:54 stefan Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
# http://www.berlin-consortium.org
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
# MA 02139, USA.

SHELL	= /bin/sh

top	= ../..
cpath	= $(top)/config
ipath	= $(top)/include
lpath	= $(top)/lib
mpath	= $(top)/modules
dpath	= dep
opath	= obj
gpath	= gdb
ppath	= prf
rpath	= $(lpath)

include $(cpath)/local.mk
include $(cpath)/Warsaw/local.mk  # for ORBLIBS

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(ORBCPPFLAGS) $(GL_INCLUDES)
CXXFLAGS= -pthread -Wall -Dprofile $(OPTFLAGS)
LDFLAGS	= -L$(lpath)
#LIBS	= -lWarsaw -lBerlin -lPrague
LIBS	= $(ORBLIBS)

# there are an awful lot of identical rules in here; so let's combine them
# (and get dependencies to work better as well)
# This is a little klugy, but, for now, is an improvement
MISC    = get-nameservice-ior
MISCDEP := $(foreach target,$(MISC),$(dpath)/$(target).d)

TARGET	=  demo

.PHONY: demo clean distclean warsaw-modules

world:		$(TARGET)

demo: nsref.ior warsaw-modules

nsref.ior: get-nameservice-ior
	./get-nameservice-ior > nsref.ior

warsaw-modules: Warsaw/build-stamp
Warsaw/build-stamp: Warsaw.idl
	rm -rf Warsaw impls
	@echo Building skeletons
	idl2perl -combine -out Warsaw Warsaw.idl
	mv Warsaw/Warsaw.pm Warsaw/Warsaw.pm.back
	echo 'use Warsaw::_file_;' > Warsaw/Warsaw.pm
	cat Warsaw/Warsaw.pm.back >> Warsaw/Warsaw.pm
	@echo Building implementation templates
	idl2perl -out impls -impl Warsaw.idl
	touch Warsaw/build-stamp

Warsaw.idl: gen-warsaw-idl.pl $(wildcard $(top)/idl/Warsaw/*.idl)
	./gen-warsaw-idl.pl $(top)/idl/Warsaw/*.idl

ldlibpath = LD_LIBRARY_PATH=$(lpath)

$(MISC): %: $(opath)/%.o
		$(ldlibpath) $(CXX) $(LDFLAGS) -o $@ $< $(LIBS)

install:	world
		for i in $(TARGET); do \
		  install -m755 $$i $(bindir); \
		done

clean:	
		rm -f *.o $(opath)/*.o $(dpath)/*.d *~ bb.out gmon.out
		rm -f nsref.ior Warsaw.idl
		rm -rf Warsaw impls

distclean:	clean
		rm -f $(MISC)
		rm -rf $(opath) $(gpath) $(ppath) $(dpath)

# These need a special rule, because they generally have no .hh file
$(MISCDEP): $(dpath)/%.d: %.cc
		@echo making dependencies for $<
		-@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) $(DEPFLAGS) $(CPPFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o : /g" > $@'		
$(dpath)/%.d:	%.cc %.hh
		@echo making dependencies for $<
		-@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) $(DEPFLAGS) $(CPPFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	%.cc
		-@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(MISCDEP)
endif 
endif
