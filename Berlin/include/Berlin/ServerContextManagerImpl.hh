/*$Id: ServerContextManagerImpl.hh,v 1.11 1999/09/30 17:23:33 gray Exp $
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
#ifndef _ServerContextManagerImpl_hh
#define _ServerContextManagerImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/ServerContext.hh"
#include "Warsaw/Graphic.hh"
#include "Berlin/FactoryFinderImpl.hh"
#include "Berlin/GenericFactoryImpl.hh"
#include "Prague/Sys/Thread.hh"

class ServerContextManagerImpl : implements(ServerContextManager)
//. the ServerContextManager just hands out new ServerContexts to
//. people who are connecting.  it might want to do some checking on
//. the incoming ClientContext's credentials, but at the moment it doesn't.
{
  typedef vector<ServerContextImpl *> clist_t;
public:  
  ServerContextManagerImpl(GenericFactoryImpl *);
  ServerContext_ptr newServerContext(ClientContext_ptr c) throw (SecurityException);
  void setSingleton(const char *, CORBA::Object_ptr)
    throw (SecurityException, SingletonFailureException);
  void delSingleton(const char *)
    throw (SecurityException, SingletonFailureException);
  CORBA::Object_ptr getSingleton(const char *) 
    throw (SecurityException, SingletonFailureException);
  void ping();
  void start();
protected:
  static void *run(void *);
  FactoryFinderImpl *ffinder;
  map<string, CORBA::Object_var> singletons;
  clist_t contexts;
  Prague::Thread thread;
  Prague::Mutex mutex;
};

#endif
