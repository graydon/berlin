# $Id: local.mk.in,v 1.5 1999/11/26 18:29:40 graydon Exp $
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

# Places to install libraries, binaries, etc.
prefix		= /usr/local
bindir		= $$DESTDIR$(prefix)/bin
libdir		= $$DESTDIR$(prefix)/lib
includedir	= $$DESTDIR$(prefix)/include
moddir		= $$DESTDIR$(prefix)/lib/Berlin
confdir		= $$DESTDIR/etc

# Various programs used in compilation and running of the programs.

IDL		= /usr/local/bin/omniidl2
CXX		= g++
AR		= ar
YACC		= bison -y
FLEX		= flex
INSTALL		= /usr/bin/install -c
LD		= ld
RANLIB		= ranlib

# Compilation and linking flags.
IDLFLAGS	= 
LCIDLFLAGS	= -l
DYNIDLFLAGS	= -a
ORBFLAGS	= __OMNIORB2__
ORBCPPFLAGS	= -D__x86__ -D__linux__ -D__OSVERSION__=2 
ORBOPTFLAGS	= -fno-default-inline -fomit-frame-pointer -fnonnull-objects
ORBLIBS		=  -lomniORB2 -lomnithread -ltcpwrapGK -lomniLC

GGI_INCLUDES	= 
GGI_LIBS	=  -lggi

GL_INCLUDES	=  
GL_LIBS		=  -lggi  -lGL -lGLU

OPTFLAGS	= -O3
SOFLAGS		= -fpic
GDBFLAGS	= -ggdb -DGDB
PROFILEFLAGS	= -pg -Winline 
CFLAGS	   	= -pthread -Wall -Dprofile $(OPTFLAGS)
CXXFLAGS   	= -pthread -Wall -Dprofile $(OPTFLAGS)
LDFLAGS    	=
LIBS		= -lpthread
