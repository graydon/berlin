# $Id: Makefile,v 1.3 2000/05/03 19:22:10 stefan Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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

top	= ../../..
cpath	= $(top)/config
hpath	= $(top)/include/Drawing/FT

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) 
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lttf

SRC	= Engine.cc Face.cc Instance.cc Glyph.cc Outline.cc Bitmap.cc Pixmap.cc \
	  Font.cc BitmapFont.cc PixmapFont.cc

TARGET	= $(lpath)/libFT.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Drawing/FT

include $(cpath)/library.mk
