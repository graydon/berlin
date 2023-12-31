/*$Id: SubjectImpl.cc,v 1.20 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/Tracer.hh>
#include "Berlin/SubjectImpl.hh"
#include <algorithm>
#include <functional>

using namespace Prague;
using namespace Warsaw;

SubjectImpl::SubjectImpl() : _blocked(false) {}

void SubjectImpl::attach(Observer_ptr observer)
{
  Trace trace("SubjectImpl::attach");
  Prague::Guard<Mutex> guard(_observerMutex);
  _observers.push_back(Warsaw::Observer::_duplicate(observer));
}

struct Id_eq : public std::unary_function<Warsaw::Identifiable_ptr, bool>
{
  Id_eq(Warsaw::Identifiable_ptr i) : id(i) {}
  bool operator()(const Warsaw::Identifiable_ptr i) const { return id->is_identical(i);}
  Warsaw::Identifiable_ptr id;
};

void SubjectImpl::detach(Observer_ptr observer)
{
  Trace trace("SubjectImpl::detach");
  Prague::Guard<Mutex> guard(_observerMutex);
  _observers.erase(find_if(_observers.begin(), _observers.end(), Id_eq(observer)));
}


void SubjectImpl::block(CORBA::Boolean blocked)
{
  Prague::Guard<Mutex> guard(_mutex);
  _blocked = blocked;
}

void SubjectImpl::notify()
{
  this->notify(CORBA::Any());
}

void SubjectImpl::notify(const CORBA::Any &change)
{
  Trace trace("SubjectImpl::notify");
  Prague::Guard<Mutex> guard(_mutex);
  if (!_blocked)
    {
      Prague::Guard<Mutex> guard(_observerMutex);
      for(olist_t::iterator i = _observers.begin(); i != _observers.end(); ++i)
	try { (*i)->update(change);}
        catch (const CORBA::OBJECT_NOT_EXIST &) { *i = Observer::_nil();}
	catch (const CORBA::COMM_FAILURE &) { *i = Observer::_nil();}
    }
}
