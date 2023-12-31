/*$Id: ServerContextImpl.hh,v 1.20 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _ServerContextImpl_hh
#define _ServerContextImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Server.hh>
#include <Berlin/KitImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <multimap.h>

class ServerImpl;

//. this is an encapsulated "entry point" which a client uses to manufacture
//. new objects, look up singletons, look up the scene root, etc.
class ServerContextImpl : public virtual POA_Warsaw::ServerContext,
			  public virtual PortableServer::RefCountServantBase
{
  typedef std::multimap<std::string, KitImpl *> klist_t;
 public:
  ServerContextImpl(ServerImpl *, Warsaw::ClientContext_ptr);
  ~ServerContextImpl();
  Warsaw::ClientContext_ptr client();
  Warsaw::Kit_ptr resolve(const char *, const Warsaw::Kit::PropertySeq &)
    throw (Warsaw::SecurityException, Warsaw::CreationFailureException);
  void set_singleton(const char *, CORBA::Object_ptr) 
    throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
  void remove_singleton(const char *) 
    throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
  CORBA::Object_ptr get_singleton(const char *) 
    throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
  bool ping();
 private:
  static unsigned long      _counter;
  ServerImpl               *_server;
  Warsaw::ClientContext_var _client;
  klist_t                   _kits;
  CORBA::PolicyList         _policies;
  Prague::Mutex             _mutex;
};

#endif
