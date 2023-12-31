/*$Id: ServerImpl.cc,v 1.10 2001/04/18 06:07:27 stefan Exp $
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
#include "Berlin/ImplVar.hh"
#include "Berlin/ServerImpl.hh"
#include "Berlin/ServerContextImpl.hh"
#include "Berlin/Logger.hh"
#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Directory.hh>

using namespace Prague;
using namespace Warsaw;

ServerImpl *ServerImpl::_server = 0;
static Mutex mutex;

ServerImpl *ServerImpl::instance()
{
  Prague::Guard<Mutex> guard(mutex);
  if (!_server) _server = new ServerImpl();
  return _server;
}

ServerImpl::ServerImpl() : _thread(&ServerImpl::run, this) {}
//. this is the 1 object you *can* find through nameservice. since nameservice has
//. no pretense of having security, we take over and handle security manually
//. once you've found the server. It allocates contexts. It's pretty simple.

//. This is called by the client to get it's server context!
ServerContext_ptr ServerImpl::create_server_context(ClientContext_ptr c) throw (SecurityException)
{
  Trace trace("ServerImpl::create_server_context");
  Prague::Guard<Mutex> guard (_mutex);
  Impl_var<ServerContextImpl> sc(new ServerContextImpl(this, c));
  _contexts.push_back(sc);
  return sc._retn()->_this();
}

void ServerImpl::set_singleton(const char *name, CORBA::Object_ptr singleton) 
  throw (SecurityException, SingletonFailureException)
{
  Prague::Guard<Mutex> guard (_mutex);
  _singletons[name] = singleton;
}

void ServerImpl::remove_singleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  Prague::Guard<Mutex> guard (_mutex);
  smap_t::iterator p = _singletons.find(name);
  if (p != _singletons.end()) _singletons.erase(p);
}

CORBA::Object_ptr ServerImpl::get_singleton(const char *name) 
  throw (SecurityException, SingletonFailureException)
{
  Prague::Guard<Mutex> guard (_mutex);
  smap_t::iterator p = _singletons.find(name);
  if (p != _singletons.end()) return CORBA::Object::_duplicate(p->second);
  throw SingletonFailureException();
}

void ServerImpl::start()
{
  _thread.start();
}

void ServerImpl::stop()
{
  Trace trace("ServerImpl::stop");
  Prague::Guard<Mutex> guard (_mutex);
  for (clist_t::iterator i = _contexts.begin(); i != _contexts.end(); i++)
    ServerImpl::destroy_context(*i);
  _contexts.clear();
  exit(0);
}

void ServerImpl::ping()
{
  Trace trace("ServerImpl::ping");
  Prague::Guard<Mutex> guard (_mutex);
  clist_t tmp;
  for (clist_t::iterator i = _contexts.begin(); i != _contexts.end(); i++)
    if ((*i)->ping())
	tmp.push_back(*i);
    else
	destroy_context(*i);
  _contexts = tmp;
};

void ServerImpl::scan(const std::string &name)
{
  Prague::Guard<Mutex> guard(_mutex);
  /*
   * load all files in <name> according to the regexp '\\.so'
   */
  Directory directory(name, Directory::alpha, "\\.so");
  Logger::log(Logger::loader) << "ServerImpl: scanning plugin dir " << name << std::endl;
  for (Directory::iterator i = directory.begin(); i != directory.end(); i++)
    {
      Plugin<KitFactory> *plugin = new Plugin<KitFactory>((*i)->long_name());
      /*
       * if this is not a plugin, skip over it
       */
      if (!*plugin)
	{
	  Logger::log(Logger::loader) << (*i)->name() << " not loadable " << plugin->error() << std::endl;
	  delete plugin;
	  continue;
	}
      _factories.insert(fmap_t::value_type((*plugin)->type(), plugin));
      Logger::log(Logger::loader) << "ServerImpl: loaded factory for " << (*plugin)->type() << " from " << (*i)->name() << std::endl;
    }
}

void ServerImpl::clear()
//. hope you know what you are doing if you call this...
{
  Prague::Guard<Mutex> guard(_mutex);
  for (fmap_t::iterator i = _factories.begin(); i != _factories.end(); i++) delete (*i).second;
  _factories.clear();
}

ServerImpl::FactoryList ServerImpl::list()
{
  FactoryList fl;
  for (fmap_t::iterator i = _factories.begin(); i != _factories.end(); ++i)
    fl.insert(FactoryList::value_type((*i).first, (*(*i).second)->properties()));
  return fl;
}

KitImpl *ServerImpl::create(const char *type, const Kit::PropertySeq &properties, PortableServer::POA_ptr poa)
{
  Trace trace("ServerImpl::create");
  fmap_t::iterator f1 = _factories.lower_bound(type), f2 = _factories.upper_bound(type);
  for (fmap_t::iterator i = f1; i != f2; i++)
    if ((*(*i).second)->supports(properties))
      return (*(*i).second)->create(properties, poa);
  return 0;
}

void *ServerImpl::run(void *X)
{
  ServerImpl *server = reinterpret_cast<ServerImpl *>(X);
  while (true)
    {
      Thread::delay(1000);
      server->ping();
    }
  return 0;
}

void ServerImpl::destroy_context(ServerContextImpl *context)
{
  Trace trace("ServerImpl::destroy_context");
  PortableServer::POA_var poa = context->_default_POA();
  PortableServer::ObjectId *oid = poa->servant_to_id(context);
  try { poa->deactivate_object(*oid);}
  /*
   * should this output go to Logger::log(Logger::corba) ?
   * -stefan
   */
  catch (const CORBA::OBJECT_NOT_EXIST &) { cerr << "caught CORBA::OBJECT_NOT_EXIST while cleaning up ServerContext" << endl;}
  catch (const CORBA::COMM_FAILURE &) { cerr << "caught CORBA::COMM_FAILURE while cleaning up ServerContext" << endl;}
  catch(...) { cerr << "caught unknown exception while cleaning up ServerContext" << endl;}
  delete oid;
}
