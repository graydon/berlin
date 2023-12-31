/*$Id: CloneableImpl.cc,v 1.8 1999/09/10 20:57:37 gray Exp $
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
#include "Warsaw/config.hh"
#include "Berlin/CloneableImpl.hh"

// Cloneable is a subclass of LifeCycleObject with some handy methods
// tacked on the side, a "copyStateToOther" pure virtual it forces
// derived classes to implement, an implementation of the
// LifeCycleObject operations in terms of copyStateToOther, and a
// session-management system which is rudimentary and probably will be
// replaced some day in the future when we know more.

void CloneableImpl::bind(ServerContext_ptr sc)
{
  context = ServerContext::_duplicate(sc);
}


// copy produces a new clone on the machine found by the FactoryFinder "there", which 
// in most cases will actually be "here". But anyway, it copies state. That's what's important!

CosLifeCycle::LifeCycleObject_ptr CloneableImpl::copy(CosLifeCycle::FactoryFinder_ptr there,
						      const CosLifeCycle::Criteria &the_criteria)
{
  CosLifeCycle::Key newObjectName;
  newObjectName.length(1);
  newObjectName[0].id   = (const char*) this->NP_IRRepositoryId();; 
  newObjectName[0].kind = (const char*) "Object"; 
  
  CosLifeCycle::Factories *factories = there->find_factories(newObjectName);
  CosLifeCycle::GenericFactory_var theFactory =  CosLifeCycle::GenericFactory::_narrow((*factories)[0]);

  if (CORBA::is_nil(theFactory)) throw CosLifeCycle::NotCopyable();
  CORBA::Object_var newObject = theFactory->create_object(newObjectName, the_criteria);
  
  // do Cloneable-specific things to the new object
  Cloneable_var newClone = Cloneable::_narrow(newObject);
  if (!CORBA::is_nil(newClone))
    {
      // !!!FIXME!!! note: hot-swap is BROKEN at the moment, precisely because of this.
      // we need to resolve how we're gonna do this.
      //
      //     this->copyStateToOther(newClone);
      //     newClone->mySession(this->mySession());
      return Cloneable::_duplicate(newClone);
    }
  else
    {
      CosLifeCycle::LifeCycleObject_var newLifeCycle = CosLifeCycle::LifeCycleObject::_narrow(newObject);
      if (!CORBA::is_nil(newLifeCycle)) return CosLifeCycle::LifeCycleObject::_duplicate(newLifeCycle);
      else throw CosLifeCycle::NotCopyable();      // well, we tried!
    }
  return CosLifeCycle::LifeCycleObject::_nil();
}


// move is just like copy, only it (a) tells the underlying ORB to forward all requests to the
// new copy, and (b) destroys itself once it's made the copy, effectively migrating the object.
// also, it doesn't actually return anything. It's a "go there" method.

void  CloneableImpl::move(CosLifeCycle::FactoryFinder_ptr there, const CosLifeCycle::Criteria &the_criteria)
{
  // do some thread-coordinating magic
  omniLC::ThreadLC zz(this);

  // insert the current lifecycle object into the criteria list
  unsigned long int i = the_criteria.length();

  // the_criteria is const - duplicate and add.
  CosLifeCycle::Criteria our_criteria;
  our_criteria.length(i);
  for (unsigned long int j = 0; j < i; j++)
    {
      our_criteria[j].name = the_criteria[j].name;
      our_criteria[j].value = the_criteria[j].value;
    }

  our_criteria.length(i+1); 
  our_criteria[i].name = "NP_omniLifeCycleInfo";
  (::CORBA::Any &)(our_criteria[i].value) <<= this->_get_lifecycle();

  // make normal copy on remote host
  CORBA::Object_ptr newObject = this->copy(there, our_criteria);
  
  // tell omni we're through
  _move(newObject);
  CORBA::BOA::getBOA()->dispose(this);
}


// clone produces a copy of an object on the same server as we're currently running.
// it's just a chorthand for an otherwise tedious call.
Cloneable_ptr CloneableImpl::clone()
{
  CosLifeCycle::Criteria crit;
  //   CORBA::Object_var newObj = this->copy(this->mySession()->myFactoryFinder(), crit);
  CORBA::Object_var newObj;
  return Cloneable::_narrow(newObj);
}


// reload is shorthand, again, for in-place hot swapping of an object with a new clone.
// you use it when you're reloading a plugin from disk. obj->reload();
void CloneableImpl::reload()
{
  CosLifeCycle::Criteria crit;
  //   this->move(this->mySession()->myFactoryFinder(), crit);
}


// this is the simplest possible "blow self up" method.
void CloneableImpl::remove ()
{
  CORBA::BOA::getBOA()->dispose(this);
}

// garbage collection to be implemented here.
void CloneableImpl::reference() {};
void CloneableImpl::forget() {};


// these are just getters and setters for the current session
// session_ptr CloneableImpl::mySession() {
//   _sessionMutex.lock();
//   session_ptr temp = _mySession;
//   _sessionMutex.unlock();
//   return temp; 
// }

// void CloneableImpl::mySession(session_ptr s) {
//   _sessionMutex.lock();
//   _mySession = s;
//   _sessionMutex.unlock();
// }

// by default, a clone is unmanaged. children of Cloneable
// are going to have to register their thread- and graphic-ness
void CloneableImpl::registerWithMyManagers() {}
