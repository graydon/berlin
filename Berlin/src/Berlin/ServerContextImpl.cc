/*$Id: ServerContextImpl.cc,v 1.14 1999/09/30 17:23:33 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
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

#include "Berlin/GenericFactoryImpl.hh"
#include "Berlin/ClientContextImpl.hh"
#include "Berlin/ServerContextManagerImpl.hh"
#include "Berlin/Logger.hh"

using namespace Prague;

// this is the thing which holds all the references to things a client allocates
// (for garbage collection), does security checks for new allocation, and
// occasionally pings the client to make sure it's still there.

ServerContextImpl::ServerContextImpl(ServerContextManagerImpl *m, CosLifeCycle::FactoryFinder_ptr ff, ClientContext_ptr c)
  : manager(m), ffinder(ff), cContext(ClientContext::_duplicate(c)) 
{}

CosLifeCycle::FactoryFinder_ptr ServerContextImpl::factoryFinder()
{
  return CosLifeCycle::FactoryFinder::_duplicate(ffinder);
}

ClientContext_ptr ServerContextImpl::client()
{
  return ClientContext::_duplicate(cContext);
}

// will fill the rest of these in as needs arise. They should do something reasonably obvious.

void ServerContextImpl::setSingleton(const char *name, CORBA::Object_ptr singleton) 
  throw (SecurityException, SingletonFailureException)
{
  manager->setSingleton(name, singleton);
}

void ServerContextImpl::delSingleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  manager->delSingleton(name);
}

CORBA::Object_ptr ServerContextImpl::getSingleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  return manager->getSingleton(name);
}

// this method should eventually do some kind of checking on the
// ClientContext to make sure they are allowed to construct objects on
// this server.

CORBA::Object_ptr ServerContextImpl::create(const char *interfaceName)    
  throw (SecurityException, CreationFailureException)
{
  CosLifeCycle::Key key;
  key.length(1);
  key[0].id   =  interfaceName; 
  key[0].kind = (const char*) "Object"; 

  CosLifeCycle::Criteria criteria;
  
  CosLifeCycle::Factories *factories = ffinder->find_factories(key);
  CosLifeCycle::GenericFactory_var factory = CosLifeCycle::GenericFactory::_narrow((*factories)[0]);

  if (CORBA::is_nil(factory)) throw CreationFailureException();
  CORBA::Object_var object = factory->create_object(key, criteria);

  Cloneable_var cloneable = Cloneable::_narrow(object);
  if (!CORBA::is_nil(cloneable)) cloneable->bind(ServerContext_var(_this()));
  // save a reference for future destroying purposes  
  MutexGuard guard(mutex);
  objects.push_back(object);
  return CORBA::Object::_duplicate(object);
}

// this will nuke all allocated objects if the client has died, and then tell the caller 
// (ServerContextManager) who will most likely destroy this ServerContext.

bool ServerContextImpl::ping()
{
  MutexGuard guard (mutex);
  bool alive = true;
  try
    {
      if (CORBA::is_nil(cContext)) alive = false;
      else alive = cContext->ping();
    }
  catch (...)
    {
      alive = false;
    }
  if (!alive)
    {
      for(olist_t::iterator i = objects.begin(); i != objects.end(); i++)
	{
	  CosLifeCycle::LifeCycleObject_var p = CosLifeCycle::LifeCycleObject::_narrow(*i);
	  if (!CORBA::is_nil(p)) p->remove();
	}
    }
  return alive;
}



