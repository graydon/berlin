# $Id: Makefile,v 1.40 1999/11/29 01:20:25 tobias Exp $
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

cpath	= ../../config
ipath	= ../../include
lpath	= ../../lib
dpath	= dep
opath	= obj
rpath	= $(lpath)

include $(cpath)/local.mk

CPPFLAGS= $(SYSCPPFLAGS) $(ORBCPPFLAGS) -I. -I..
CXXFLAGS+= $(SOFLAGS)# $(ORBOPTFLAGS)
LDFLAGS	= -shared -rdynamic
LIBS	+= $(ORBLIBS)

DYNINT	= Types.idl Message.idl Event.idl Style.idl
STATINT	= ClientContext.idl ServerContext.idl Naming.idl \
	  Drawable.idl Pencil.idl \
	  Transform.idl Region.idl Allocation.idl Traversal.idl PickTraversal.idl DrawTraversal.idl Graphic.idl Screen.idl \
	  Focus.idl \
	  Figure.idl Image.idl Transformator.idl \
	  Grid.idl Viewport.idl Stage.idl \
	  Command.idl \
	  Subject.idl Controller.idl View.idl \
	  Telltale.idl BoundedValue.idl BoundedRange.idl TextBuffer.idl StreamBuffer.idl \
	  Button.idl \
	  Metrics.idl Text.idl \
	  Raster.idl \
	  Desktop.idl Window.idl 
LCINT	= LifeCycle.idl Cloneable.idl DrawingKit.idl FigureKit.idl LayoutKit.idl CommandKit.idl \
	  WidgetKit.idl TextKit.idl ImageKit.idl DesktopKit.idl

INT	= $(DYNINT) $(STATINT) $(LCINT)

DEP	= $(patsubst %.idl, $(dpath)/%.d, $(INT))

DYNSRC	= $(patsubst %.idl, %DynSK.cc, $(DYNINT))
STATSRC	= $(patsubst %.idl, %SK.cc, $(DYNINT)) $(patsubst %.idl, %SK.cc, $(STATINT))
LCSRC	= $(patsubst %.idl, %SK.cc, $(LCINT))

UTIL	= Warsaw.cc Unicode.cc

SRC	= $(DYNSRC) $(STATSRC) $(LCSRC) $(UTIL)

DYNHDR	= $(patsubst %.idl, %.hh, $(DYNINT))
STATHDR	= $(patsubst %.idl, %.hh, $(STATINT))
LCHDR	= $(patsubst %.idl, %.hh, $(LCINT))
HDR	= $(DYNHDR) $(STATHDR) $(LCHDR)

OBJ	= $(patsubst %.cc, $(opath)/%.o, $(SRC))
TARGET	= $(lpath)/libWarsaw.so

vpath %.d $(dpath)

world:	$(TARGET)

$(TARGET):	$(OBJ)
		$(CXX) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)
		strip --strip-unneeded $@

$(dpath)/%.d:	%.idl
		@echo making dependencies for $<
		@if [ ! -d $(dpath) ]; then mkdir $(dpath); fi
		@$(SHELL) -ec "$(CXX) -x c++ -M $(CPPFLAGS) $(CXXFLAGS) $< \
		| perl -e 'undef \$$/; while(<>)\
		    {\
	    	      (/^(.*)\\.idl\\.o:((.|\\n)*)/);\
		      \$$file = \$$1;\
		      \$$dep = \$$2;\
		      print \"\$$1\", \"SK.cc \", \$$file, \"DynSK.cc \",\
			    \$$file, \".hh $(dpath)\/\", \$$file, \".d:\", \$$dep;\
		      \$$file .= \"SK\";\
		      \$$dep =~ s/\\.idl/\\.hh/g;\
		      print \"$(opath)\/\", \$$file, \".o\", \": \", \$$file, \".cc\", \$$dep ;\
		    }' > $@"
$(STATHDR): %.hh:		%.idl
		$(IDL) $(IDLFLAGS) $<
$(DYNHDR): %.hh:	%.idl
		$(IDL) $(IDLFLAGS) $(DYNIDLFLAGS) $<
$(LCHDR): %.hh:	%.idl
		$(IDL) $(IDLFLAGS) $(LCIDLFLAGS) $<
$(STATSRC): %SK.cc:		%.idl
		$(IDL) $(IDLFLAGS) $<
$(DYNSRC): %DynSK.cc:	%.idl
		$(IDL) $(IDLFLAGS) $(DYNIDLFLAGS) $<
$(LCSRC): %SK.cc:	%.idl
		$(IDL) $(IDLFLAGS) $(LCIDLFLAGS) $<
$(opath)/%.o:	%.cc
		@if [ ! -d $(opath) ]; then mkdir $(opath); fi
		$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean:	
		rm -f $(opath)/*.o $(dpath)/*.d
		rm -f $(DYNSRC) $(STATSRC) $(LCSRC) $(DYNHDR) $(STATHDR) $(LCHDR) *~

distclean:	clean
		rm -f $(TARGET)
		rm -rf $(opath) $(dpath)

install:
		install -m755 $(TARGET) $(libdir)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean) 
-include $(DEP)
endif 
endif 

