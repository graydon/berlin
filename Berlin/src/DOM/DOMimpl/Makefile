# $Id: Makefile,v 1.2 1999/08/26 13:55:41 gray Exp $
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

top	= ../../..
cpath	= $(top)/config
hpath	= $(top)/include/Berlin

include $(cpath)/local.mk

# Compilation and linking flags.
CPPFLAGS= -I$(ipath) -I$(ipath)/Warsaw
LDFLAGS	= -shared -rdynamic -L$(lpath)
LIBS	= -lPrague #$(ORBLIBS)

SRC = 	  domString.cc domText_impl.cc domAttrib_impl.cc testIt.cc \
          domElement_impl.cc domNode_impl.cc domNodeIterator_impl.cc \
          domAttributeList_impl.cc domDocument_impl.cc

TARGET	= $(mpath)/libDOM.so

vpath %.d $(dpath)
vpath %.hh  $(ipath)/Warsaw $(ipath)/Berlin

include $(cpath)/library.mk
