# Generated automatically from Makefile.in by configure.
# $Id: Makefile,v 1.13 2001/04/10 15:09:46 tobias Exp $
#
# This source file is a part of the Berlin Project.
# Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
VERSION		:= 0.2.2

top_srcdir	:= /home/hunger/c/berlin/Berlin
srcdir		:= $(top_srcdir)/clients/C++
builddir	:= ../..
cdir		:= $(builddir)/config
hdir		:= $(top_srcdir)/include

define makedep
@echo generating dependencies for $(<F)
$(SHELL) -ec '$(CXX) $(DEPFLAGS) $(CPPFLAGS) $< | sed "s/$*\\.o[ :]*/$*\\.d $*\\.o : /g" > $@'
endef

define compile
@echo compiling $(@F)
$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPTFLAGS) $(SOFLAGS) -c $< -o $@
endef

include $(cdir)/local.mk

CPPFLAGS	+=  -pthread -pthread
LIBS		+= -lBerlin -lWarsaw -lBabylon -lPrague  -lomnithread -ltcpwrapGK -lomniDynamic3 -lomniORB3 -pthread -ldl -pthread

TARGETS		:= demo canvas ggiwrapper pinyin_demo # trafo -- doesn't work here. Anyone?
SRC		:= Application.cc LayoutDemo.cc TextDemo.cc \
		   EditTextDemo.cc RasterDemo.cc TransformDemo.cc \
		   ColorDemo.cc LogoDemo.cc FocusDemo.cc \
		   ViewportDemo.cc DocDemo.cc TermDemo.cc demo.cc
OBJ		:= $(patsubst %.cc, %.o, $(SRC))

TARGETS		:= $(bdir)/demo $(bdir)/canvas $(bdir)/ggiwrapper

all:	$(TARGETS)

$(bdir)/demo:	$(OBJ)
	@echo linking $(@F)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

$(bdir)/canvas:	canvas.o
	@echo linking $(@F)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

$(bdir)/ggiwrapper:	ggiwrapper.o
	@echo linking $(@F)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

$(bdir)/pinyin_demo: pinyin_demo.o TextConvertor.o
	@echo linking $(@F)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

install:
		$(INSTALL) -m755 $(TARGETS) $(bindir)

clean:	
		rm -f $(TARGETS)
		rm -f *.o

distclean:	clean
		rm -f Makefile $(cdir)/Clients-C++/subdirs.mk \
			$(cdir)/Clients-C++/config.log $(cdir)/Clients-C++/config.status

maintainer-clean: distclean
		rm -f $(top_srcdir)/config/Clients-C++/configure $(top_srcdir)/config/Clients-C++/aclocal.m4

%.o:	$(srcdir)/%.cc
	@echo compiling $(@F)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
