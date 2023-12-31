# $Id: Makefile,v 1.6 2000/05/17 16:21:34 skinnemo Exp $
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

SHELL	= /bin/sh

top	= ../..
cpath	= $(top)/config

ifeq (,$(findstring $(MAKECMDGOALS), clean distclean))
include $(top)/config/local.mk
endif

define makeconf
echo Running configure $$CONFIGURE_OPTS...
./configure $$CONFIGURE_OPTS
endef

.PHONY: config clean distclean dist
.FORCE:

world:	local.mk

config: .FORCE configure
	@$(makeconf)

local.mk: configure local.mk.in
	@$(makeconf)

configure: configure.in aclocal.m4
	@echo Running autoconf...
	@autoconf

aclocal.m4: $(cpath)/macros/*.m4
	@echo Running aclocal...
	@(aclocal -I $(cpath)/macros || cat $(cpath)/macros/*.m4 > aclocal.m4)

clean:	
	rm -f *~ local.mk config.log config.status

distclean:	clean
	rm -f configure config.cache config.param

ifeq ($(MAKECMDGOALS), dist)
include dist.mk

dist:	distclean
	@echo "Generating Berlin-Clients-Perl-$(major).$(minor).$(patch).tar.gz"
	@cat $(cpath)/archive > archive
	@for file in $(archive); do echo Berlin/$$file >> archive; done
	@(cd $(top)/.. && tar -czf Berlin-Clients-Perl-$(major).$(minor).$(patch).tar.gz \
	  --files-from Berlin/config/Clients-Perl/archive --exclude='CVS')
	@rm archive

endif
