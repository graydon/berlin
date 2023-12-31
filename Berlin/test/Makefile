# $Id: Makefile,v 1.45 1999/11/26 18:29:41 graydon Exp $
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

SHELL	= /bin/sh

top	= ..
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

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(ORBCPPFLAGS) $(GL_INCLUDES)
CXXFLAGS= -pthread -Wall -Dprofile $(OPTFLAGS)
LDFLAGS	= -L$(lpath) -L$(mpath)
LIBS	= -lWarsaw -lBerlin -lDrawing -lPrague

DEMO =	  Application.cc LayoutDemo.cc TextDemo.cc RasterDemo.cc TransformDemo.cc LogoDemo.cc \
	  demo.cc
DEP	= $(patsubst %.cc, $(dpath)/%.d, $(DEMO))
OBJ	= $(patsubst %.cc, $(opath)/%.o, $(DEMO))

TARGET	=  server demo

world:		$(TARGET)

ldlibpath = LD_LIBRARY_PATH=$(lpath)


server:		server.o
		$(ldlibpath) $(CXX) $(LDFLAGS) -o $@ $< $(LIBS)  

demo:		$(OBJ)
		$(ldlibpath) $(CXX) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

install:	world
		for i in server demo; do \
		  install -m755 $$i $(bindir); \
		done

clean:	
		rm -f *.o $(opath)/*.o $(dpath)/*.d *~

distclean:	clean
		rm -f $(TARGET)
		rm -rf $(opath) $(gpath) $(ppath) $(dpath)

$(dpath)%.d:	%.cc %.hh
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec '$(CXX) -MM $(CPPFLAGS) $< \
		| sed "s/$*\\.o[ :]*/$(dpath)\/$*\\.d $(opath)\/$*\\.o : /g" > $@'
$(opath)/%.o:	%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(DEP)
endif 
endif
