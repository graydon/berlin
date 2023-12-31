# $Id: Prague.mk,v 1.4 1999/07/23 17:08:37 gray Exp $
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

include Sys/Sys.mk
include IPC/IPC.mk
include Filter/Filter.mk
include Network/Network.mk
include Unicode/Unicode.mk

vpath %.hh  $(ipath)/Prague/Sys $(ipath)/Prague/IPC $(ipath)/Prague/Filter $(ipath)/Prague/Network $(ipath)/Prague/Unicode

SRC = $(SYS_SRC) $(IPC_SRC) $(FLT_SRC) $(NTW_SRC) $(UNI_SRC)
OBJ = $(SYS_OBJ) $(IPC_OBJ) $(FLT_OBJ) $(NTW_OBJ) $(UNI_OBJ)
GDB = $(SYS_GDB) $(IPC_GDB) $(FLT_GDB) $(NTW_GDB) $(UNI_GDB)
DEP = $(SYS_DEP) $(IPC_DEP) $(FLT_DEP) $(NTW_DEP) $(UNI_DEP)
YACC = $(SYS_YACC) $(IPC_YAC) $(FLT_YACC) $(NTW_YACC) $(UNI_YACC)
