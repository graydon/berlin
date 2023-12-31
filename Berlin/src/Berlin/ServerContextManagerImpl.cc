/*$Id: ServerContextManagerImpl.cc,v 1.13 1999/11/06 20:23:08 stefan Exp $
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
#include "Berlin/ServerContextManagerImpl.hh"
#include "Berlin/ServerContextImpl.hh"
#include <unistd.h>

using namespace Prague;

ServerContextManagerImpl::ServerContextManagerImpl(GenericFactoryImpl *factory)
//. this is the 1 object you *can* find through nameservice. since nameservice has
//. no pretense of having security, we take over and handle security manually
//. once you've found the sessionManager. It allocates sessions. It's pretty simple.
  : thread(&ServerContextManagerImpl::run, this)
{
  ffinder = FactoryFinderImpl::Instance(factory);
}

ServerContext_ptr ServerContextManagerImpl::newServerContext(ClientContext_ptr c)
  throw (SecurityException)
{
  MutexGuard guard (mutex);
  ServerContextImpl *sc = new ServerContextImpl(this, CosLifeCycle::FactoryFinder_var(ffinder->_this()), c);
  sc->_obj_is_ready(_boa());
  contexts.push_back(sc);
  return sc->_this();
}

void ServerContextManagerImpl::setSingleton(const char *name, CORBA::Object_ptr singleton) 
  throw (SecurityException, SingletonFailureException)
{
  MutexGuard guard (mutex);
  singletons[name] = singleton;
}

void ServerContextManagerImpl::delSingleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  MutexGuard guard (mutex);
  map<string, CORBA::Object_var>::iterator p = singletons.find(name);
  if (p != singletons.end())
    {
      Cloneable_var cloneable = Cloneable::_narrow(p->second);
      if (!CORBA::is_nil(cloneable)) cloneable->remove();
      singletons.erase(p);
    }
}

CORBA::Object_ptr ServerContextManagerImpl::getSingleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  MutexGuard guard (mutex);
  map<string, CORBA::Object_var>::iterator p = singletons.find(name);
  if (p != singletons.end()) return CORBA::Object::_duplicate(p->second);
  throw SingletonFailureException();
}

void ServerContextManagerImpl::start()
{
  thread.start();
}

void ServerContextManagerImpl::ping()
{
  MutexGuard guard (mutex);
  clist_t tmp;
  for (clist_t::iterator i = contexts.begin(); i != contexts.end(); i++)
    {
      if ((*i)->ping()) tmp.push_back(*i);	    
      else (*i)->_dispose();
    }
  contexts = tmp;
};

void *ServerContextManagerImpl::run(void *X)
{
  ServerContextManagerImpl *manager = reinterpret_cast<ServerContextManagerImpl *>(X);
  while (true)
    {
      sleep(1);
      manager->ping();
    }
  return 0;
}

