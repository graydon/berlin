/*$Id: SelectionImpl.cc,v 1.5 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Controller.hh>
#include <Berlin/ObserverImpl.hh>
#include "Command/SelectionImpl.hh"

using namespace Prague;
using namespace Warsaw;

/*
 * a little glue class to notify the Selection if the 'toggled'
 * flag in the Telltale changed state.
 */
class SelectionImpl::Observer : public ObserverImpl
{
public:
  Observer(SelectionImpl *, Telltale_ptr, Tag);
  ~Observer();
  Tag id() const { return t;}
  bool toggled() { return cached;}
  void update(const CORBA::Any &);
private:
  SelectionImpl *selection;
  RefCount_var<Warsaw::Telltale> item;
  bool cached;
  Tag t;
};

bool SelectionImpl::Id_eq::operator()(const SelectionImpl::Observer *o) const { return o->id() == id;}

SelectionImpl::Observer::Observer(SelectionImpl *s, Telltale_ptr i, Tag tt)
  : selection(s),
    item(RefCount_var<Warsaw::Telltale>::increment(i)),
    cached(item->test(Warsaw::Controller::toggled)),
    t(tt)
{
}

SelectionImpl::Observer::~Observer()
{
  Trace trace("SelectionImpl::Observer::~Observer");
  item->detach(Observer_var(_this()));
  selection->remove_observer(t);
}

void SelectionImpl::Observer::update(const CORBA::Any &any)
{
  bool toggled = item->test(Warsaw::Controller::toggled);
  if (toggled == cached) return; // not for us...
  cached = toggled;
  selection->update(t, toggled);
}

SelectionImpl::SelectionImpl(Warsaw::Selection::Policy p, TelltaleConstraint_ptr c)
  : policy(p), constraint(RefCount_var<TelltaleConstraint>::increment(c))
{
  Trace trace("SelectionImpl::SelectionImpl");
}

SelectionImpl::~SelectionImpl()
{
  Trace trace("SelectionImpl::~SelectionImpl");
//   for (list_t::iterator i = items.begin(); i != items.end(); i++)
//     try { (*i)->deactivate();}
//     catch (CORBA::OBJECT_NOT_EXIST &) {}
}

Warsaw::Selection::Policy SelectionImpl::type() { return policy;}
void SelectionImpl::type(Warsaw::Selection::Policy) {}

Tag SelectionImpl::add(Telltale_ptr t)
{
  Trace trace("SelectionImpl::add");
  Prague::Guard<Mutex> guard(mutex);
  Tag id = uniqueId();
  Observer *observer = new Observer(this, t, id);
//   activate(observer);
  t->attach(Observer_var(observer->_this()));
  if (!CORBA::is_nil(constraint)) constraint->add(t);
  items.push_back(observer);
  return id;
}

void SelectionImpl::remove(Tag t)
{
  Trace trace("SelectionImpl::remove");
  Prague::Guard<Mutex> guard(mutex);
  size_t i = id_to_index(t);
  if (i < items.size())
    {
      //       if (!CORBA::is_nil(constraint)) constraint->remove(t);
      items[i]->destroy();
      items.erase(items.begin() + i);
    }
}

Selection::Items *SelectionImpl::toggled()
{
  Trace trace("SelectionImpl::toggled");
  Prague::Guard<Mutex> guard(mutex);
  Warsaw::Selection::Items_var ret = new Warsaw::Selection::Items;
  for (list_t::iterator i = items.begin(); i != items.end(); i++)
    if ((*i)->toggled())
      {
	ret->length(ret->length() + 1);
	ret[ret->length() - 1] = (*i)->id();
      }
  return ret._retn();
}

void SelectionImpl::update(Tag t, bool toggled)
{
  Trace trace("SelectionImpl::update");
  CORBA::Any any;
  Warsaw::Selection::Item item;
  item.id = t;
  item.toggled = toggled;
  any <<= item;
  notify(any);
}

void SelectionImpl::remove_observer(Tag t)
{
  Trace trace("SelectionImpl::remove_observer");
  Prague::Guard<Mutex> guard(mutex);
  size_t i = id_to_index(t);
  if (i < items.size()) items.erase(items.begin() + i);
}

Tag SelectionImpl::uniqueId()
{
  Tag id;
  for (id = 0;
       std::find_if(items.begin(), items.end(), Id_eq(id)) != items.end();
       id++);
      return id;
}

CORBA::Long SelectionImpl::id_to_index(Tag id)
{
  return std::find_if(items.begin(), items.end(), Id_eq(id)) - items.begin();
}
