//
// $Id: NameUtil.hh,v 1.1 1999/03/28 09:13:04 gray Exp $
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

#ifndef _NameUtil_hh
#define _NameUtil_hh

#include <omniORB2/Naming.hh>

// this is a file with general name-service utilities in it. 

class lookupFailureException {};

const CosNaming::Name charPtrToName(char *ch);
void bindObjectToName(CORBA::ORB_ptr, CORBA::Object_ptr, char *ch);
CORBA::Object_ptr lookup(CORBA::ORB_ptr, char *ch)  throw (lookupFailureException);

#endif
