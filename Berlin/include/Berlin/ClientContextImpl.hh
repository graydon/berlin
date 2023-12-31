//
// $Id: ClientContextImpl.hh,v 1.10 1999/09/30 17:23:33 gray Exp $
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

#ifndef _ClientContextImpl_hh
#define _ClientContextImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/ClientContext.hh"
#include "Prague/Sys/Thread.hh"
#include "Prague/Sys/User.hh"


// This is a handle to a client application that the display server holds. It
// provides the display server with enough mechanisms to check to see if the
// client is alive and to determine the client's security rights. It is
// instantiated within the client address space.

class ClientContextImpl : implements(ClientContext)
{
public:
  ClientContextImpl();
  
  Unistring *userName();
  CORBA::Boolean ping();
protected:
  Prague::User *user;
};

#endif
