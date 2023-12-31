/*$Id: client.hh,v 1.1 2001/04/20 14:57:11 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Tobias Hunger <Tobias@berlin-consortium.org>
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

#ifndef _Client_hh
#define _Client_hh

#include <Warsaw/config.hh>
#include <omniORB3/CORBA.h>
#include <omniORB3/poa.h>
#include <Warsaw/resolve.hh>
#include <Warsaw/exception.hh>
#include <Warsaw/ClientContextImpl.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Window.hh>
#include <Warsaw/Trigger.hh>
#include <Warsaw/Unicode.hh>
#include <Warsaw/Controller.hh>

#include <Berlin/CommandImpl.hh>
#include <Berlin/ControllerImpl.hh>

class Berlin_Server {
public:
    Berlin_Server(int argc, char** argv) {
	// CORBA initialization
	orb = CORBA::ORB_init(argc, argv, "omniORB3");
	name = resolve_init<CosNaming::NamingContext>(orb, "NameService");
	poa = resolve_init<PortableServer::POA>(orb, "RootPOA");
	poa_manager = poa->the_POAManager();
	poa_manager->activate();
	
	// Berlin initialization
	client = new ClientContextImpl;
	server = resolve_name<Warsaw::Server>(name, "IDL:Warsaw/Server:1.0");
	server_context =
	    server->create_server_context(Warsaw::ClientContext_var(client->_this()));
    }
 
    template<class T>
    typename T::_ptr_type get_kit(const char *name,
				  const Warsaw::Kit::PropertySeq &props = 0) {
	return resolve_kit<T>(server_context, name, props);
    }

    CORBA::ORB_ptr get_ORB() { return orb; }
    CosNaming::NamingContext_ptr get_naming_context() { return name; }
    PortableServer::POA_ptr get_POA() { return poa; }
    PortableServer::POAManager_ptr get_POAManager() { return poa_manager; }

    ClientContextImpl * get_client_context() { return client; }
    Warsaw::Server_ptr get_server() { return server; }
    Warsaw::ServerContext_ptr get_server_context() { return server_context;}

    Warsaw::ServerContext_var operator() () { return server_context; }

    ~Berlin_Server() { delete client; };
private:
    Berlin_Server() {};
    Berlin_Server(const Berlin_Server &) {};

    // CORBA
    CORBA::ORB_var orb;
    CosNaming::NamingContext_var name;
    PortableServer::POA_var poa;
    PortableServer::POAManager_var poa_manager;

    // BERLIN
    ClientContextImpl * client;
    Warsaw::Server_var server;
    Warsaw::ServerContext_var server_context;
};

#define REGISTER_KIT(server,name,kit,version) Warsaw::##kit##_var name = server##.get_kit<Warsaw::##kit>("IDL:Warsaw/" #kit ":" #version)

#endif // Client_hh
