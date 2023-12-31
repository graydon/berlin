/*$Id: ServerContextImpl.cc,v 1.28 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org>
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

#include <Warsaw/config.hh>
#include <Warsaw/ClientContext.hh>
#include "Berlin/ServerContextImpl.hh"
#include "Berlin/ServerImpl.hh"
#include "Berlin/KitImpl.hh"
#include "Berlin/Logger.hh"
#include <Prague/Sys/Plugin.hh>
#include <Prague/Sys/Directory.hh>
#include <Prague/Sys/Tracer.hh>
#include <strstream>

using namespace Prague;
using namespace Warsaw;

unsigned long ServerContextImpl::_counter = 0;

// this is the thing which holds all the references to things a client allocates
// (for garbage collection), does security checks for new allocation, and
// occasionally pings the client to make sure it's still there.

ServerContextImpl::ServerContextImpl(ServerImpl *s, ClientContext_ptr c)
    : _server(s), _client(ClientContext::_duplicate(c))
{
  Trace trace("ServerContextImpl::ServerContextImpl");
//   PortableServer::POA_var root = _default_POA();
//   policies.length(1);
//   policies[0] = root->create_implicit_activation_policy(PortableServer::NO_IMPLICIT_ACTIVATION);
}

ServerContextImpl::~ServerContextImpl()
{
  Trace trace("ServerContextImpl::~ServerContextImpl");
  for (klist_t::iterator i = _kits.begin(); i != _kits.end(); ++i)
    (*i).second->deactivate();
}

ClientContext_ptr ServerContextImpl::client() { return ClientContext::_duplicate(_client);}

// will fill the rest of these in as needs arise. They should do something reasonably obvious.

void ServerContextImpl::set_singleton(const char *name, CORBA::Object_ptr singleton)
  throw (SecurityException, SingletonFailureException)
{
  _server->set_singleton(name, singleton);
}

void ServerContextImpl::remove_singleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  _server->remove_singleton(name);
}

CORBA::Object_ptr ServerContextImpl::get_singleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  return _server->get_singleton(name);
}

// this method should eventually do some kind of checking on the
// ClientContext to make sure they are allowed to construct objects on
// this server.

Kit_ptr ServerContextImpl::resolve(const char *type, const Kit::PropertySeq &properties)    
  throw (SecurityException, CreationFailureException)
{
  /*
   * look for loaded kits first
   */
  klist_t::iterator k1 = _kits.lower_bound(type), k2 = _kits.upper_bound(type);
  for (klist_t::iterator i = k1; i != k2; ++i)
    if ((*i).second->supports(properties))
      {
	(*i).second->increment();
	return (*i).second->_this();
      }
  /*
   * now try the factories
   */
  PortableServer::POA_var root = _default_POA();
  std::ostrstream oss;
  oss << type << '#' << _counter++ << std::ends;
  char *name = oss.str();
  PortableServer::POAManager_var manager = root->the_POAManager();
  PortableServer::POA_var poa = root->create_POA(name, manager, _policies);
//   PortableServer::POA_var poa = PortableServer::POA::_duplicate(root);
  delete [] name;
  KitImpl *kit = _server->create(type, properties, poa);
  if (!kit) throw CreationFailureException();
  kit->bind(ServerContext_var(_this()));
  kit->increment();
  _kits.insert(klist_t::value_type(type, kit));
  return kit->_this();
}

// this will nuke all allocated objects if the client has died, and then tell the caller 
// (Server) who will most likely destroy this ServerContext.

bool ServerContextImpl::ping()
{
  Trace trace("ServerContextImpl::ping");
  Prague::Guard<Mutex> guard(_mutex);
  bool alive = true;
  if (CORBA::is_nil(_client)) alive = false;
  else
    try { _client->ping();}
    catch (...) { alive = false;}
  return alive;
}
