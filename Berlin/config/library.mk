# $Id: library.mk,v 1.4 1999/09/07 21:33:26 gray Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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

# this file defines common rules to build Berlin plugins.
# It requires the following variables to be set within the
# including Makefile:
#
# top:   	Berlin's root directory
# hpath: 	the directory containing the corresponding headers
#        	which sources depend on
# SRC:   	the list of all source files to be compiled and linked
#        	into the library
# TARGET:	the name of the library to be generated
#
# CPPFLAGS:	preprocessor flags
# CXXFLAGS:	compiler flags
# LDFLAGS:	linker flags
# LIBS:		libraries to be linked to this library
#
#

ipath	= $(top)/include
lpath	= $(top)/lib
mpath   = $(top)/modules
dpath	= dep
opath	= obj
gpath	= gdb
ppath	= prf
rpath	= $(lpath)

DEP	= $(patsubst %.cc, $(dpath)/%.d, $(SRC))
OBJ	= $(patsubst %.cc, $(opath)/%.o, $(SRC))
GDB	= $(patsubst %.cc, $(gpath)/%.o, $(SRC))
PRF	= $(patsubst %.cc, $(ppath)/%.o, $(SRC))

$(TARGET):	$(OBJ)
		$(CXX) $(LDFLAGS) -Wl,-rpath $(rpath) -o $@ $(OBJ) $(LIBS)
		strip --strip-unneeded $@

$(dpath)/%.d:	%.cc $(hpath)/%.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CPPFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o $(gpath)\/$*\\.o $(ppath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(SOFLAGS) -c $< -o $@
$(gpath)/%.o:	%.cc
		@if [ ! -d $(gpath) ]; then mkdir $(gpath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@
$(ppath)/%.o:	%.cc
		@if [ ! -d $(ppath) ]; then mkdir $(ppath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) $(PRFFLAGS) -c $< -o $@

clean:	
		rm -f $(opath)/*.o $(gpath)/*.o $(ppath)/*.o $(dpath)/*.d
		rm -f *~ $(hpath)/*~

distclean:	clean
		rm -f $(TARGET)
		rm -rf $(opath) $(gpath) $(ppath) $(dpath)

install:
		(if ! test -d $(libdir); \
		  then install -d $(libdir); \
		fi)
		install -m755 $(TARGET) $(libdir)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(DEP)
endif 
endif

