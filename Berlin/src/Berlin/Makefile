# $Id: Makefile,v 1.19 1999/11/26 18:29:40 graydon Exp $
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

top	= ../..
cpath	= $(top)/config
hpath	= $(top)/include/Berlin

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(ORBCPPFLAGS) $(GGI_INCLUDES)
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lPrague $(GGI_LIBS) #$(ORBLIBS)

SRC 	= Logger.cc GenericFactoryImpl.cc FactoryFinderImpl.cc CloneableImpl.cc \
	  RegionImpl.cc TransformImpl.cc TraversalImpl.cc AllocationImpl.cc GraphicImpl.cc \
	  MonoGraphic.cc PolyGraphic.cc Allocator.cc Requestor.cc DebugGraphic.cc \
	  DrawTraversalImpl.cc PickTraversalImpl.cc \
	  ControllerImpl.cc DesktopImpl.cc ScreenImpl.cc ScreenManager.cc EventManager.cc FocusImpl.cc NameUtil.cc \
	  SubjectImpl.cc ServerContextImpl.cc ClientContextImpl.cc ServerContextManagerImpl.cc

TARGET	= $(lpath)/libBerlin.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Warsaw $(ipath)/Berlin

include $(cpath)/library.mk

