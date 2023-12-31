# $Id: IPC.mk,v 1.8 1999/11/17 02:03:18 stefan Exp $
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

IPC_SRC	= ipcbuf.cc pipebuf.cc sockbuf.cc ptybuf.cc mmapbuf.cc \
	  Agent.cc Dispatcher.cc Coprocess.cc PipeAgent.cc TTYAgent.cc

IPC_DEP	= $(patsubst %.cc, $(dpath)/%.d, $(IPC_SRC))
IPC_OBJ	= $(patsubst %.cc, $(opath)/%.o, $(IPC_SRC))
IPC_GDB	= $(patsubst %.cc, $(gpath)/%.o, $(IPC_SRC))
IPC_PRF	= $(patsubst %.cc, $(ppath)/%.o, $(IPC_SRC))

$(dpath)/%.d:	IPC/%.cc $(ipath)/Prague/IPC/%.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o $(gpath)\/$*\\.o $(ppath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	IPC/%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@ 
$(gpath)/%.o:	IPC/%.cc
		@if [ ! -d $(gpath) ]; then mkdir $(gpath); fi
		$(CXX) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@
$(ppath)/%.o:	IPC/%.cc
		@if [ ! -d $(ppath) ]; then mkdir $(ppath); fi
		$(CXX) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) $(PRFFLAGS) -c $< -o $@

clean:		ipcclean
ipcclean:
		rm -f IPC/*~
		rm -f $(ipath)/Prague/IPC/*~

ifneq ($(MAKECMDGOALS),config) 
ifneq ($(MAKECMDGOALS),clean) 
ifneq ($(MAKECMDGOALS),distclean) 
-include $(IPC_DEP)
endif 
endif 
endif 
