# $Id: Unicode.mk,v 1.6 1999/11/03 21:55:20 gray Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999 Tobias Hunger <Tobias_Hunger@gmx.de> 
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

UNI_SRC	 = Unistring.cc Unichar.cc Dictionary.cc 

PLUGINS      = 0000-007F=Basic_Latin.so
PLUGINTARGET = ../../modules/unicode
PLUGINFLAGS  = -shared -fpic 

UNI_DEP  = $(patsubst %.cc, $(dpath)/%.d, $(UNI_SRC))
UNI_OBJ	 = $(patsubst %.cc, $(opath)/%.o, $(UNI_SRC))
UNI_GDB	 = $(patsubst %.cc, $(gpath)/%.o, $(UNI_SRC))
UNI_PRF	 = $(patsubst %.cc, $(ppath)/%.o, $(UNI_SRC))

PLUGINSRC       = $(PLUGINS:%.so=Unicode/Blocks/%.cc)
PLUGINLIBS      = $(PLUGINS:%.so=$(PLUGINTARGET)/%.so) 

$(dpath)/%.d:	Unicode/%.cc $(ipath)/Prague/Unicode/%.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o $(gpath)\/$*\\.o $(ppath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	Unicode/%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@
$(gpath)/%.o:	Unicode/%.cc
		@if [ ! -d $(gpath) ]; then mkdir $(gpath); fi
		$(CXX) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@
$(ppath)/%.o:	Unicode/%.cc
		@if [ ! -d $(ppath) ]; then mkdir $(ppath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) $(PRFFLAGS) -c $< -o $@

clean:		uniclean
uniclean:
		rm -f Unicode/*~
		rm -f $(ipath)/Prague/Unicode/*~
		rm -f $(PLUGINTARGET)/*.so

ifneq ($(MAKECMDGOALS),config) 
ifneq ($(MAKECMDGOALS),clean) 
ifneq ($(MAKECMDGOALS),distclean) 
-include $(UNI_DEP)
endif 
endif 
endif 
