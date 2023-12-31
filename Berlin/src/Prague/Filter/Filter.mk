# $Id: Filter.mk,v 1.4 1999/08/26 13:55:41 gray Exp $
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

FLT_SRC	= gzbuf.cc# xdrbuf.cc #bzbuf.cc 

FLT_DEP = $(patsubst %.cc, $(dpath)/%.d, $(FLT_SRC))
FLT_OBJ	= $(patsubst %.cc, $(opath)/%.o, $(FLT_SRC))
FLT_GDB	= $(patsubst %.cc, $(gpath)/%.o, $(FLT_SRC))
FLT_PRF	= $(patsubst %.cc, $(ppath)/%.o, $(FLT_SRC))

$(dpath)/%.d:	Filter/%.cc $(ipath)/Prague/Filter/%.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o $(gpath)\/$*\\.o $(ppath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	Filter/%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@
$(gpath)/%.o:	Filter/%.cc
		@if [ ! -d $(gpath) ]; then mkdir $(gpath); fi
		$(CXX) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@
$(ppath)/%.o:	Filter/%.cc
		@if [ ! -d $(ppath) ]; then mkdir $(ppath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) $(PRFFLAGS) -c $< -o $@

clean:		filterclean
filterclean:
		rm -f Filter/*~
		rm -f $(ipath)/Prague/Filter/*~

ifneq ($(MAKECMDGOALS),config) 
ifneq ($(MAKECMDGOALS),clean) 
ifneq ($(MAKECMDGOALS),distclean) 
-include $(FLT_DEP)
endif 
endif 
endif 
