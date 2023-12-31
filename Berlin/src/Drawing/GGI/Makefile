# $Id: Makefile,v 1.2 2000/01/16 07:58:36 graydon Exp $
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
hpath	= $(top)/include/Drawing/GGI

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(GGI_INCLUDES) $(ORBCPPFLAGS)
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lPrague -lBerlin -lggi2d $(GGI_LIBS) -lFT #-lttf

#GGIFont.cc GGIUnifont.cc

SRC	= GGIDrawingKit.cc GGIUnifont.cc 

#GGIRaster.cc GGITessellator.cc GGIVectorFont.cc GGIOutlineFont.cc GGIBitmapFont.cc GGIPixmapFont.cc \


TARGET	= $(mpath)/libGGIDrawing.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Warsaw $(ipath)/Berlin $(ipath)/Drawing/openGGI

include $(cpath)/module.mk
