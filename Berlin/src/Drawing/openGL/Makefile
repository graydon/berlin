# $Id: Makefile,v 1.18 1999/11/26 18:29:40 graydon Exp $
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

# This is a mess. sorry dudes, it had to be done for the install target.
# --Aaron

SHELL	= /bin/sh
subdirs = gltt

top	= ../../..
cpath	= $(top)/config
hpath	= $(top)/include/Drawing/openGL

ipath	= $(top)/include
lpath	= $(top)/lib
mpath   = $(top)/modules
dpath	= dep
opath	= obj
gpath	= gdb
ppath	= prf
rpath	= $(lpath)

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(GL_INCLUDES) $(ORBCPPFLAGS)
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lPrague -lBerlin $(GL_LIBS) -lGLtt -lttf

SRC	= Pointer.cc GLRaster.cc GLFont.cc GLUnifont.cc GLDrawable.cc GLPencil.cc GLDrawingKit.cc

TARGET	= $(lpath)/libDrawing.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Warsaw $(ipath)/Berlin $(ipath)/Drawing/openGL

DEP	= $(patsubst %.cc, $(dpath)/%.d, $(SRC))
OBJ	= $(patsubst %.cc, $(opath)/%.o, $(SRC))
GDB	= $(patsubst %.cc, $(gpath)/%.o, $(SRC))
PRF	= $(patsubst %.cc, $(ppath)/%.o, $(SRC))

all: subs $(TARGET)

subs:
	for i in ${subdirs}; do \
	  make -C $$i; \
	done

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
		for i in ${subdirs}; do \
		  make -C $$i clean; \
		done

distclean:	clean
		rm -f $(TARGET)
		rm -rf $(opath) $(gpath) $(ppath) $(dpath)
		for i in ${subdirs}; do \
		  make -C $$i distclean; \
		done

install:
		for i in ${subdirs}; do \
		  $(MAKE) -C $$i install; \
		done
		install -m644 $(TARGET) $(libdir)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(DEP)
endif 
endif
