/*$Id: ServerContextImpl.hh,v 1.13 1999/11/20 12:08:52 aaronv Exp $
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
#ifndef _ServerContextImpl_hh
#define _ServerContextImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/ServerContext.hh"
#include "Prague/Sys/Thread.hh"
#include <vector>
#include <string>
#include <map>

class ServerContextManagerImpl;

class ServerContextImpl : implements(ServerContext)
// this is an encapsulated "entry point" which a client uses to manufacture
// new objects, look up singletons, look up the scene root, etc.
{
  typedef vector<CORBA::Object_var> olist_t;

 public:  
  ServerContextImpl(ServerContextManagerImpl *,
                    CosLifeCycle::FactoryFinder_ptr,
                    ClientContext_ptr);

  ClientContext_ptr client();
  CosLifeCycle::FactoryFinder_ptr factoryFinder();
  CORBA::Object_ptr create(const char *)
    throw (SecurityException, CreationFailureException);
  void setSingleton(const char *, CORBA::Object_ptr) 
    throw (SecurityException, SingletonFailureException);
  void delSingleton(const char *) 
    throw (SecurityException, SingletonFailureException);
  CORBA::Object_ptr getSingleton(const char *) 
    throw (SecurityException, SingletonFailureException);
  bool ping();
 protected:
  ServerContextManagerImpl *manager;
  CosLifeCycle::FactoryFinder_var ffinder;
  ClientContext_var cContext;
  olist_t objects;
  Prague::Mutex mutex;
};

#endif
