# $Id: Makefile,v 1.7 1999/09/19 21:09:28 gray Exp $
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

subdirs	= include/Warsaw src test
# doc

.PHONY:	config test

world:	all

config::
	@cd config; ./configure $$CONFIGURE_OPTS; cd ../src/Prague/config; \
	./configure $$CONFIGURE_OPTS
# Added shell env var $$CONFIGURE_OPTS

all:
	@if [ -f include/Warsaw/config.hh ]; then \
	  for dir in ${subdirs}; do \
	    (cd $$dir && $(MAKE)) \
	    || case "$(MFLAGS)" in *k*) fail=yes;; *) exit 1;; esac; \
	  done && test -z "$$fail"; \
	else \
	  echo -e "Please run \"make config\" first!\n" \
	  "\nIf you need to provide arguments to the configure scripts use:"\
	  "\n    cd config"\
	  "\n    ./configure --whatever-you-need"\
	  "\n    cd .."\
	  "\nThen run make again."; \
	fi

clean:
	/bin/rm -f *~
	@for dir in ${subdirs}; do \
	  (cd $$dir && $(MAKE) clean) \
	  || case "$(MFLAGS)" in *k*) fail=yes;; *) exit 1;; esac; \
	done && test -z "$$fail"

distclean:
	/bin/rm -f $(cpath)/config.cache $(cpath)/config.log
	@for dir in ${subdirs}; do \
	  (cd $$dir && $(MAKE) distclean) \
	  || case "$(MFLAGS)" in *k*) fail=yes;; *) exit 1;; esac; \
	done && test -z "$$fail"

install: all
	@for dir in ${subdirs}; do \
	  (cd $$dir && $(MAKE) install); \
	done

debs:
	dpkg-buildpackage -rfakeroot -uc -us
