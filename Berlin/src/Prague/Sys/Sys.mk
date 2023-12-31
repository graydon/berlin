# $Id: Sys.mk,v 1.12 1999/10/14 03:32:56 gray Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
# http://www.berlin-consortium.org
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
# MA 02139, USA.

SYS_SRC	= logbuf.cc regex.cc User.cc Stopwatch.cc Profiler.cc Time.cc \
	  File.cc Directory.cc Path.cc GetOpt.cc DataTypeManager.cc \
	  DLL.cc MMap.cc SHM.cc Thread.cc Timer.cc Signal.cc Fork.cc

SYS_DEP	= $(patsubst %.cc, $(dpath)/%.d, $(SYS_SRC))
SYS_OBJ	= $(patsubst %.cc, $(opath)/%.o, $(SYS_SRC))
SYS_GDB	= $(patsubst %.cc, $(gpath)/%.o, $(SYS_SRC))
SYS_PRF	= $(patsubst %.cc, $(ppath)/%.o, $(SYS_SRC))

$(dpath)/%.d:	Sys/%.cc $(ipath)/Prague/Sys/%.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o $(gpath)\/$*\\.o $(ppath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	Sys/%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@
$(gpath)/%.o:	Sys/%.cc
		@if [ ! -d $(gpath) ]; then mkdir $(gpath); fi
		$(CXX) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@
$(ppath)/%.o:	Sys/%.cc
		@if [ ! -d $(ppath) ]; then mkdir $(ppath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) $(PRFFLAGS) -c $< -o $@

$(opath)/DataTypeManager.o:	Sys/DataTypeManager.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -ftemplate-depth-23 -c $< -o $@

clean:		sysclean
sysclean:
		rm -f Sys/*~
		rm -f $(ipath)/Prague/Sys/*~

ifneq ($(MAKECMDGOALS),config)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(SYS_DEP)
endif 
endif 
endif 
