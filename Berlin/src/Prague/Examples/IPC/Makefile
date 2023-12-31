# Generated automatically from Makefile.in by configure.
# $Id: Makefile,v 1.3 1999/11/16 02:15:20 stefan Exp $
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
prefix      = /usr/local
bindir      = $(prefix)/bin
libdir      = $(prefix)/lib
includedir  = $(prefix)/include

SHELL 	    = /bin/sh
# The C and C++ compilers to use.
CC          = gcc
CXX         = g++

# Various programs used in compilation and running of the programs.

AR	    = ar
YACC	    = bison -y
FLEX	    = flex
INSTALL	    = /usr/bin/install -c
LD	    = ld
MAKEINFO    = makeinfo
RANLIB      = ranlib
TEXI2DVI    = texi2dvi
TEXI2HTML   = texi2html
TEX         = 
DVIPS       = 

# Compilation and linking flags.
CFLAGS	   	= -Wall -I../../../include -pthread
CXXFLAGS   	= -Wall -I../../../../include -pthread
LDFLAGS    	= -L../../../../lib
OPTFLAGS	= -O3
SOFLAGS		= -fpic
GDBFLAGS	= -ggdb -DGDB
PROFILEFLAGS	= -pg -Winline 

ipath		= ../../../../include
lpath		= ../../../../lib
dpath		= dep
opath		= obj
gpath		= gdb
ppath		= prf
rpath		= $(lpath)

LIBS		= -lPrague -ldl -lz -lpthread

ALL        =   MMapServer MMapClient echo PipeAgent TTYAgent

world:		$(ALL)

%.o:		%.cc
		$(CXX) $(CXXFLAGS) $(GDBFLAGS) -c $< -o $@

MMapServer:	MMapServer.o
		$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

MMapClient:	MMapClient.o
		$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

echo:		echo.o
		$(CXX) $(LDFLAGS) -o $@ $^

PipeAgent:	echo PipeAgent.o
		$(CXX) $(LDFLAGS) -ggdb -o $@ PipeAgent.o $(LIBS)

TTYAgent:	TTYAgent.o
		$(CXX) $(LDFLAGS) -ggdb -o $@ $^ $(LIBS)

clean:	
		$(RM) *.o *~

distclean:	clean
		$(RM) $(ALL)

