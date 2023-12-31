//
// $Id: FactoryFinderImpl.cc,v 1.6 1999/09/30 17:23:33 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//
#include "Berlin/FactoryFinderImpl.hh"
#include "Berlin/GenericFactoryImpl.hh"

// this is the simplest factoryFinder I could think of. It's just a
// placeholder for the time being until we come up with a better
// factory naming scheme built on hostnames or something.

using namespace Prague;

CosLifeCycle::Factories * FactoryFinderImpl::find_factories ( const CosLifeCycle::Key &key)
{
  CosLifeCycle::GenericFactory_var fact = CosLifeCycle::GenericFactory::_narrow((*factories)[0]);
  if ((!CORBA::is_nil(fact)) && (fact->supports(key))) return factories;
  else throw CosLifeCycle::NoFactory();
}


FactoryFinderImpl::FactoryFinderImpl(CosLifeCycle::GenericFactory_ptr factory)
{
  factories = new CosLifeCycle::Factories();  
  factories->length(1);
  (*factories)[0] = factory;
}

//These two global variables are for storing the
//static member data in FactoryFinderImpl...
Mutex FactoryFinderImpl::mutex;
FactoryFinderImpl *FactoryFinderImpl::instance;

// this takes care of singleton work.
FactoryFinderImpl *FactoryFinderImpl::Instance(GenericFactoryImpl *factory)
{
  MutexGuard guard(mutex);
  if (!instance)
    {
      instance = new FactoryFinderImpl(CosLifeCycle::GenericFactory_var(factory->_this()));
      instance->_obj_is_ready(factory->_boa());
    } 
  return instance;
}

// this is the dreaded singleton-which-might-explode method.
FactoryFinderImpl *FactoryFinderImpl::Instance() throw (unInitializedGenericFactoryException)
{
  MutexGuard guard(mutex);
  if (!instance) throw unInitializedGenericFactoryException();
  return instance;
}


// never call this ctor. You're asking for trouble. None of the operations will work.
// that's why it's private for chrissake!
FactoryFinderImpl::FactoryFinderImpl() {}


