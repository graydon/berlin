/*$Id: GenericFactoryImpl.hh,v 1.8 1999/09/30 17:23:33 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _GenericFactoryImpl_hh
#define _GenericFactoryImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/LifeCycle.hh>
#include <Berlin/CloneableImpl.hh>
#include <Berlin/ServerContextImpl.hh>
#include <Berlin/ClientContextImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <string>
#include <map>

class noSuchProtoException {};
class noSuchPluginException {};
class SeverContextImpl;
class Plugin;


struct keyComp
//. this is a comparator for our lookup table
{
  bool operator()(const CosLifeCycle::Key &a, const CosLifeCycle::Key &b);
};

class GenericFactoryImpl : lcimplementsscoped(CosLifeCycle,GenericFactory) 
//. what used to be the "plugin loader" is now, in line with corba lifecycle service,
//. an implementation of GenericFactory. The idea with this factory is that it looks
//. up objects in dynamically loadable modules, on the disk, and tries to find one which
//. claims to be the correct key
{  
  typedef map<CosLifeCycle::Key, Plugin *, keyComp> plist_t;
public:
  
  GenericFactoryImpl();
  ~GenericFactoryImpl();
  
  // stuff declared in IDL
  virtual CORBA::Boolean  supports(const CosLifeCycle::Key &);
  virtual CORBA::Object_ptr create_object(const CosLifeCycle::Key &, const CosLifeCycle::Criteria &);
  
  // this builds the plugin table
  void scan(const char *);
  void clear();
protected:
  
  // this is a simple helper function to make it easier to find the
  // LifeCycyleInfo object in criteria
  omniLifeCycleInfo_ptr extractLifeCycleFromCriteria(const CosLifeCycle::Criteria &);
  
  // this does the call into the function pointer
  CloneableImpl *loadPlugin(const CosLifeCycle::Key &) throw (noSuchPluginException);
  
  // lock the whole thing during a load.
  plist_t plugins;
  Prague::Mutex mutex;
};

#endif
