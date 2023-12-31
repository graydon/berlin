/*$Id: GenericFactoryImpl.cc,v 1.12 1999/10/19 21:07:52 gray Exp $
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
#include "Berlin/GenericFactoryImpl.hh"
#include "Berlin/CloneableImpl.hh"
#include "Berlin/Logger.hh"
#include "Prague/Sys/DLL.hh"
#include "Prague/Sys/Directory.hh"

// genericFactory is a dynamic linking facility conforming to the
// corba COS Lifecycle specification. It produces objects based on
// their interface names. It also handles assigning lifecycle info
// tags to the objects, so that they may migrate from one server to
// another.

using namespace Prague;

class Plugin : private DLL
{
public:
  Plugin(const string &name) : DLL(name), id(0), loader(0)
    {
      char *(*getName)() = (char *(*)()) resolve("getName");
      if (!getName)
	{
	  Logger::log(Logger::loader) << "Plugin " << DLL::name()
				      << " failed to load (reason: " << DLL::error() << ')' << endl;
	  return;
	}
      id = getName();
      loader = (CloneableImpl *(*)()) resolve("getPlugin");
    }
  ~Plugin() {}
  bool ok() { return loader != 0;}
  const char *name() { return id;}
  CloneableImpl *load() { return loader ? loader() : 0;}
private:
  const char *id;
  CloneableImpl *(*loader)();
};


GenericFactoryImpl::GenericFactoryImpl() {}
GenericFactoryImpl::~GenericFactoryImpl() { clear();}

// this is the method your average factory-finder is going to call from
// god knows where on the network. Its job it to make the object 
// which identifies itself by "key". If "criteria" contains an omniLifecycleObject, we 
// assume the new object is being made in preparation for a move, so we do
// a _set_lifecycle on the new object.


CORBA::Object_ptr
GenericFactoryImpl::create_object(const CosLifeCycle::Key &key, const CosLifeCycle::Criteria &criteria)
{  
  CloneableImpl *cloneable = 0;
  try
    {    
      MutexGuard guard(mutex);
      cloneable = loadPlugin(key);
    }
  catch (noSuchPluginException e)
    {
      throw CosLifeCycle::CannotMeetCriteria();
    }
  if (!cloneable)
    Logger::log(Logger::loader) << "GenericFactoryImpl: loaded NULL pointer in response to " << key[0].id << '\n';
  CORBA::Object_var object;
  // see if we are doing a lifecycle migration here
  omniLifeCycleInfo_ptr li = extractLifeCycleFromCriteria(criteria);

  // no lifeCycleInfo detected
  if (CORBA::is_nil(li))
    {
      Logger::log(Logger::loader) << "GenericFactoryImpl: not doing lifecycle copy for " << key[0].id << '\n';
      cloneable->_obj_is_ready(_boa());
      object = cloneable->_this();
    
      if (CORBA::is_nil(object))
	Logger::log(Logger::loader) << "GenericFactoryImpl: returning a nil reference for " << key[0].id << '\n';
      // lifeCycleInfo was found!
    }
  else 
    {
      Logger::log(Logger::loader) << "GenericFactoryImpl: doing lifecycle copy for " << key[0].id << '\n';
      cloneable->_set_lifecycle(li);
      cloneable->_obj_is_ready(_boa());
      object = cloneable->_this();
    }
  return CORBA::Object::_duplicate(object);
}


// this is just a helper function to make the above code a little more readable
// perhaps someone who knows the Any interface a little better can advise me on whether
// this is the proper way to do it?
omniLifeCycleInfo_ptr
GenericFactoryImpl::extractLifeCycleFromCriteria(const CosLifeCycle::Criteria &criteria)
{
  omniLifeCycleInfo_ptr li = omniLifeCycleInfo::_nil();
  for(unsigned int i = 0; i < criteria.length(); i++)
    if (strcmp(criteria[i].name, "NP_omniLifeCycleInfo") == 0)
      if(criteria[i].value >>= li) break;
  return li;
}


// this method does the dirty work of getting the new C++ object off the
// disk and connected to the BOA. It's not pretty, but that's all it does.
CloneableImpl *GenericFactoryImpl::loadPlugin(const CosLifeCycle::Key &key)
  throw (noSuchPluginException)
{
  plist_t::iterator p = plugins.find(key);
  if (p == plugins.end())
    {
      // naughty boy, you should have called supports() first!
      throw noSuchPluginException();
    }
  else return (*p).second->load(); 
}


// you can call this is you're curious about creation support in the factory.
CORBA::Boolean GenericFactoryImpl::supports (const CosLifeCycle::Key &key)
{
  plist_t::iterator p = plugins.find(key);
  if (p == plugins.end())
    {
      Logger::log(Logger::loader) << "GenericFactoryImpl does not support " << key[0].id << '\n';
      Logger::log(Logger::loader) << "GenericFactoryImpl interface listing follows: \n";
      for(p = plugins.begin(); p != plugins.end(); p++)
	Logger::log(Logger::loader) << p->first[0].id << '\n';
      return false;
    }
  else return true;
}


// init builds the table of function pointers from which we construct new objects.
// if you ever want to re-scan the directory for new plugins, well, we could use stat
// records or something, or you could just call init again. That would probably do OK.

void GenericFactoryImpl::scan(const char *name)
{
  MutexGuard guard(mutex);
  /*
   * load all files in <name> according to the regexp '\\.so'
   */
  Directory directory(name, Directory::alpha, "\\.so");
  Logger::log(Logger::loader) << "GenericFactoryImpl: scanning plugin dir " << name << endl;
  for (Directory::iterator i = directory.begin(); i != directory.end(); i++)
    {
      Plugin *plugin = new Plugin((*i)->longName());
      /*
       * if this is not a plugin, skip over it
       */
      if (!plugin->ok())
	{
	  delete plugin;
	  continue;
	}
      CosLifeCycle::Key prototype;
      prototype.length(1);
      prototype[0].id   = (const char*) plugin->name();    // string copied
      prototype[0].kind = (const char*) "Object";          // string copied    
      
      plist_t::iterator p = plugins.find(prototype);
      if (p != plugins.end())
	Logger::log(Logger::loader) << "GenericFactoryImpl warning: multiple definitions for plugin "
				    << plugin->name() << endl;
      
      // stash the function pointer for loading new object in the future
      plugins[prototype] = plugin;
      Logger::log(Logger::loader) << "GenericFactoryImpl loaded plugin " << plugin->name() << endl;
    }
}

void GenericFactoryImpl::clear()
{
  MutexGuard guard(mutex);
  for (plist_t::iterator i = plugins.begin(); i != plugins.end(); i++) delete (*i).second;
  plugins.erase(plugins.begin(), plugins.end());
}

bool keyComp::operator()(const CosLifeCycle::Key &a, const CosLifeCycle::Key &b)
{
  // Blast C-style strings! see pg 468 of Stroustrup 3rd ed.
  return( strcmp(a[0].id, b[0].id) < 0);
};
