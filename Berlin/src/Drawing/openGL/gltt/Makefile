# $Id: Makefile,v 1.6 1999/11/26 18:29:40 graydon Exp $
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

top	= ../../../..
cpath	= $(top)/config
hpath	= $(top)/include/Drawing/openGL/gltt

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw $(ORBINCLUDES) $(GL_INCLUDES)
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lPrague -lBerlin $(GL_LIBS) $(ORBLIBS)

SRC	= BottomText.cc FTGlyphVectorizer.cc GLTTOutlineFont.cc \
	  FTBitmapFont.cc FTInstance.cc GLTTPixmapFont.cc \
	  FTEngine.cc FTPixmapFont.cc \
	  FTFace.cc GLTTBitmapFont.cc \
	  FTFont.cc GLTTFont.cc \
	  FTGlyph.cc GLTTGlyphPolygonizer.cc \
	  FTGlyphBitmap.cc GLTTGlyphPolygonizerHandler.cc \
	  FTGlyphPixmap.cc GLTTGlyphTriangulator.cc 

TARGET	= $(lpath)/libGLtt.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Drawing/openGL $(ipath)/Drawing/openGL/gltt

include $(cpath)/library.mk

