//
// $Id: Plugin.hh,v 1.1 1999/04/11 08:11:45 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//

#ifndef __PLUGIN__
#define __PLUGIN__

#include <omniORB2/CORBA.h>
#include "Berlin/CloneableImpl.hh"

// this is the measely plugin interface
extern "C" {
  CloneableImpl *getPlugin();
  char *getName();
}

#define EXPORT_PLUGIN(impl,name) \
  CloneableImpl *getPlugin() {return new impl();} char *getName() {return name;}

#endif
