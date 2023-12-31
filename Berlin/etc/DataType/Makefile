# $Id: Makefile,v 1.1 1999/07/19 18:14:57 gray Exp $
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

top	= ../..
cpath	= $(top)/config

include $(cpath)/local.mk

SRC	= Header \
	  compressed.rule encoded.rule image.rule audio.rule music.rule \
	  text.rule video.rule world.rule font.rule archive.rule \
	  game.rule executable.rule script.rule database.rule \
	  application.rule pgp.rule Trailer

TARGET	= datatypes

world:	$(TARGET)

$(TARGET):	$(SRC)
		cat < $^ > $@
