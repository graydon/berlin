/*$Id: ServerImpl.hh,v 1.8 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _ServerImpl_hh
#define _ServerImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/Graphic.hh>
#include <Berlin/KitImpl.hh>
#include <Berlin/KitFactory.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Plugin.hh>
#include <vector>
#include <string>
#include <map>
#include <multimap.h>

class ServerContextImpl;

//. the Server just hands out new ServerContexts to
//. people who are connecting.  it might want to do some checking on
//. the incoming ClientContext's credentials, but at the moment it doesn't.
class ServerImpl : public virtual POA_Warsaw::Server,
                   public virtual PortableServer::RefCountServantBase
{
    friend class ServerContextImpl;
    typedef std::vector<ServerContextImpl *> clist_t;
    typedef std::multimap<std::string, Prague::Plugin<KitFactory> *> fmap_t;
    typedef std::multimap<std::string, KitImpl *> kmap_t;
    typedef std::map<std::string, CORBA::Object_var> smap_t;
public:
    // Singelton pattern: This makes sure that only one ServerImpl is around.
    static ServerImpl *instance();
    typedef multimap<std::string, Warsaw::Kit::PropertySeq_var> FactoryList;
    virtual Warsaw::ServerContext_ptr create_server_context(Warsaw::ClientContext_ptr c) throw (Warsaw::SecurityException);
    virtual void set_singleton(const char *, CORBA::Object_ptr)
	throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
    virtual void remove_singleton(const char *)
	throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
    virtual CORBA::Object_ptr get_singleton(const char *) 
	throw (Warsaw::SecurityException, Warsaw::SingletonFailureException);
    
    //. Finds the requested Kit and loads it.
    template <class K>
    typename K::_ptr_type resolve(const char *, const Warsaw::Kit::PropertySeq &, PortableServer::POA_ptr);
    //. Starts the server.
    void start();
    //. Stops the server.
    void stop();
    //. Pings all ServerContexts and destroys them if necessary.
    void ping();
    // This builds the plugin table
    void scan(const std::string &);
    //. Deletes all known Kits.
    void clear();
    FactoryList list();
protected:
    //. Finds the requested kit and loads it. This is a helper function only.
    //. Use resolve,
    KitImpl *create(const char *, const Warsaw::Kit::PropertySeq &, PortableServer::POA_ptr);
private:
    //. Sets up run() to run every secound.
    ServerImpl();
    //. Called every second to ping the clients.
    static void *run(void *);
    //. Destroys a given Servercontext (and with that the client's resources
    //. in this server).
    static void destroy_context(ServerContextImpl *);
    smap_t  _singletons;
    clist_t _contexts;
    fmap_t  _factories;
    kmap_t  _kits;
    Prague::Thread _thread;
    Prague::Mutex _mutex;
    static ServerImpl *_server;
};

template <class K>
typename K::_ptr_type ServerImpl::resolve(const char *type, const Warsaw::Kit::PropertySeq &properties, PortableServer::POA_ptr poa)
{
    KitImpl *kit = create(type, properties, poa);
    if (!kit) return K::_nil();
    typename K::_var_type reference;
    try
      {
	reference = K::_narrow(Warsaw::Kit_var(kit->_this()));
      }
    catch (CORBA::Exception &e)
      {
	std::cerr << "Cannot narrow reference: " << e << std::endl;
	return K::_nil();
      }
    if (CORBA::is_nil(reference))
      {
	std::cerr << "Reference has incorrect type" << std::endl;
	return K::_nil();
      }
    _kits.insert(kmap_t::value_type(type, kit));
    return reference._retn();
}

#endif
