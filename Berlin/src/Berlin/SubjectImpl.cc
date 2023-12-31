/*$Id: SubjectImpl.cc,v 1.11 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#include "Berlin/SubjectImpl.hh"
#include "Berlin/Logger.hh"

using namespace Prague;

SubjectImpl::SubjectImpl() : blocked(false) {}

void SubjectImpl::attach(Observer_ptr o)
{
  SectionLog section("SubjectImpl::attach");
  MutexGuard guard(observerMutex);
  observers.push_back(Observer::_duplicate(o));
}

void SubjectImpl::detach(Observer_ptr o)
{
  SectionLog section("SubjectImpl::detach");
  MutexGuard guard(observerMutex);
  observers.remove(o);
}


void SubjectImpl::block(CORBA::Boolean b)
{
  MutexGuard guard(myMutex);
  blocked = b;
}

void SubjectImpl::notify() {
    this->notify(CORBA::Any());
}

void SubjectImpl::notify(const CORBA::Any &whatChanged)
{
  SectionLog section("SubjectImpl::notify");
  MutexGuard guard(myMutex);
  if (!blocked)
    {
      MutexGuard guard(observerMutex);
      Subject_var me = _this();
      for(list<Observer_var>::iterator i = observers.begin(); i != observers.end(); i++)
	(*i)->update(me,whatChanged);
    }
}
