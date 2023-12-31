# Generated automatically from Makefile.in by configure.
# $Id: Makefile.in,v 1.3 2001/01/09 21:35:10 tobias Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 1999, 2000 Tobias Hunger <tobias@berlin-consortium.org> 
# Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

SHELL		:= /bin/sh

top_srcdir	:= /home/stefan/Berlin/Berlin
srcdir		:= $(top_srcdir)/src/Babylon/blocks
builddir	:= ../../..
cdir		:= $(builddir)/config
hdir		:= $(top_srcdir)/include

define compile
@echo compiling $(@F)
@$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@
endef

define link
@echo linking $(@F)
@$(CXX) $(LDFLAGS) -o $@ $< $(LIBS) && strip --strip-unneeded $@
endef

include $(cdir)/local.mk

CPPFLAGS	+=  -D_REENTRANT
CXXFLAGS	+= $(SO_CXXFLAGS)
LDFLAGS		+= $(SO_LDFLAGS)
LIBS		+=  -lPrague -ldl -lpthread

# Which Unicode modules to build
UNICODE_BLOCKS	= complete

# General section
SRC	= undef.cc \
	  0-7F.cc \
	  80-FF.cc \
	  100-17F.cc \
	  180-24F.cc

ifeq ($(UNICODE_BLOCKS),complete)
SRC	+= 250-2AF.cc \
	   2B0-2FF.cc \
	   300-36F.cc \
           370-3FF.cc \
           400-4FF.cc \
           530-58F.cc \
           590-5FF.cc \
           600-6FF.cc \
           700-74F.cc \
           780-7BF.cc \
           900-97F.cc \
           980-9FF.cc \
           A00-A7F.cc \
           A80-AFF.cc \
           B00-B7F.cc \
           B80-BFF.cc \
           C00-C7F.cc \
           C80-CFF.cc \
	   D00-D7F.cc \
           D80-DFF.cc \
           E00-E7F.cc \
           E80-EFF.cc \
           F00-FFF.cc \
           1000-109F.cc \
           10A0-10FF.cc \
           1100-11FF.cc \
           1200-137F.cc \
           13A0-13FF.cc \
           1400-167F.cc \
           1680-169F.cc \
           16A0-16FF.cc \
           1780-17FF.cc \
           1800-18AF.cc \
           1E00-1EFF.cc \
           1F00-1FFF.cc \
           2000-206F.cc \
           2070-209F.cc \
           20A0-20CF.cc \
           20D0-20FF.cc \
           2100-214F.cc \
           2150-218F.cc \
           2190-21FF.cc \
           2200-22FF.cc \
           2300-23FF.cc \
           2400-243F.cc \
           2440-245F.cc \
           2460-24FF.cc \
           2500-257F.cc \
           2580-259F.cc \
           25A0-25FF.cc \
           2600-26FF.cc \
           2700-27BF.cc \
           2800-28FF.cc \
           2E80-2EFF.cc \
           2F00-2FDF.cc \
           2FF0-2FFF.cc \
           3000-303F.cc \
           3040-309F.cc \
           30A0-30FF.cc \
           3100-312F.cc \
           3130-318F.cc \
           3190-319F.cc \
           31A0-31BF.cc \
           3200-32FF.cc \
           3300-33FF.cc \
           3400-4DB5.cc \
           4E00-9FFF.cc \
           A000-A48F.cc \
           A490-A4CF.cc \
           AC00-D7A3.cc \
           D800-DB7F.cc \
           DB80-DBFF.cc \
           DC00-DFFF.cc \
           E000-F8FF.cc \
           F900-FAFF.cc \
           FB00-FB4F.cc \
           FB50-FDFF.cc \
           FE20-FE2F.cc \
           FE30-FE4F.cc \
           FE50-FE6F.cc \
           FE70-FEFE.cc \
           FEFF-FEFF.cc \
           FF00-FFEF.cc \
           FFF0-FFFD.cc \
           10300-1032F.cc \
	   10330-1034F.cc \
           10400-1044F.cc \
           1D000-1D0FF.cc \
	   1D100-1D1FF.cc \
           1D400-1D7FF.cc \
           20000-2A6D6.cc \
           2F800-2FA1F.cc \
           E0000-E007F.cc \
	   F0000-FFFFD.cc \
	   100000-10FFFD.cc
endif

OBJ	= $(patsubst %.cc, %.o, $(SRC))
TARGET	= $(patsubst %.cc, $(mdir)/Babylon/%.so, $(SRC))

all:	$(TARGET)

clean:		
	rm -f $(TARGET)

distclean:	clean

maintainer-clean: distclean

%.o:	$(srcdir)/%.cc
	$(compile)

$(mdir)/Babylon/%.so:	%.o
	$(link)
