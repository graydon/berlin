/*$Id: Warsaw.cc,v 1.3 2000/07/21 19:25:05 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * http://www.berlin-consortium.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

#include "Warsaw/Warsaw.hh"
#include "Warsaw/Server.hh"

Warsaw::Warsaw(int argc, char **argv)
  : orb(CORBA::ORB_init(argc, argv, "omniORB3")),
    client(new ClientContextImpl)
{
  CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
  PortableServer::POA_var poa = PortableServer::POA::_narrow(obj);
  PortableServer::POAManager_var pman = poa->the_POAManager();
  pman->activate();

  CosNaming::NamingContext_var context = resolve_init<CosNaming::NamingContext>(orb, "NameService");
  Server_var s = resolve_name<Server>(context, Server::_PD_repoId);
  server = s->newServerContext(ClientContext_var(client->_this()));
}

Warsaw::~Warsaw()
{
//  client->_dispose();
}
