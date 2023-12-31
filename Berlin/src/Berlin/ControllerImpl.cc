/*$Id: ControllerImpl.cc,v 1.6 1999/11/06 20:23:08 stefan Exp $
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

#include "Berlin/ControllerImpl.hh"
#include "Warsaw/Transform.hh"
#include "Warsaw/Region.hh"
#include "Warsaw/PickTraversal.hh"
#include "Warsaw/Event.hh"
#include "Berlin/Logger.hh"

using namespace Prague;

ControllerImpl::ControllerImpl(bool t) : flags(0L), grabbed(false), transparent(t) {}
void ControllerImpl::pick(PickTraversal_ptr traversal)
{
  SectionLog section("ControllerImpl::pick");
  if (grabbed || traversal->intersectsAllocation())
    {
      traversal->enterController(Controller_var(_this()));
      MonoGraphic::traverse(traversal);
      if (!transparent && !traversal->picked()) traversal->hit();
      traversal->leaveController();
    }
}

Controller_ptr ControllerImpl::parentController() { MutexGuard guard(mutex); return Controller::_duplicate(parent);}

void ControllerImpl::appendController(Controller_ptr c)
{
  if (!CORBA::is_nil(Controller_var(c->parentController()))) return;
  MutexGuard guard(mutex);
  controllers.push_back(Controller::_duplicate(c));
  c->setParentController(Controller_var(_this()));
}

void ControllerImpl::prependController(Controller_ptr c)
{
  if (!CORBA::is_nil(Controller_var(c->parentController()))) return;
  MutexGuard guard(mutex);
  controllers.insert(controllers.begin(), Controller::_duplicate(c));
  c->setParentController(Controller_var(_this()));
}

void ControllerImpl::insertController(Controller_ptr m, Controller_ptr c)
{
  if (!CORBA::is_nil(Controller_var(c->parentController()))) return;
  if (CORBA::is_nil(m)) prependController(c);
  else
    {
      MutexGuard guard(mutex);
      clist_t::iterator i = controllers.begin();
      while (i != controllers.end() && !m->_is_equivalent(*i)) i++;
      if (i != controllers.end())
	{
	  controllers.insert(i, Controller::_duplicate(c));
	  c->setParentController(Controller_var(_this()));
	}
    }
}

void ControllerImpl::replaceController(Controller_ptr m, Controller_ptr c)
{
  if (CORBA::is_nil(m) || !CORBA::is_nil(Controller_var(c->parentController()))) return;
  if (CORBA::is_nil(c)) removeController(m);
  else
    {
      MutexGuard guard(mutex);
      clist_t::iterator i = controllers.begin();
      while (i != controllers.end() && !m->_is_equivalent(*i)) i++;
      if (i != controllers.end())
	{
	  *i = Controller::_duplicate(c);
	  c->setParentController(Controller_var(_this()));
	}
    }
}

void ControllerImpl::removeController(Controller_ptr c)
{
  if (CORBA::is_nil(c) || !CORBA::is_nil(Controller_var(c->parentController()))) return;
  MutexGuard guard(mutex);
  clist_t::iterator i = controllers.begin();
  while (i != controllers.end() && !c->_is_equivalent(*i)) i++;
  if (i != controllers.end())
    {
      c->setParentController(Controller::_nil());
      controllers.erase(i);
    }
}

void ControllerImpl::setParentController(Controller_ptr c)
{
  MutexGuard guard(mutex);
  parent = Controller::_duplicate(c);
}

void ControllerImpl::requestFocus(Controller_ptr c)
{
  SectionLog section("ControllerImpl::requestFocus");  
  Controller_var parent = parentController();
  if (CORBA::is_nil(parent)) return;
  if (!c->_is_equivalent(Controller_var(_this())) &&
      !test(Telltale::active))
    parent->requestFocus(Controller_var(_this()));
  parent->requestFocus(c);
}

CORBA::Boolean ControllerImpl::receiveFocus(Focus_ptr f)
{
  SectionLog section("ControllerImpl::receiveFocus");  
  set(Telltale::active);
  return true;
}

void ControllerImpl::loseFocus(Focus_ptr)
{
  SectionLog section("ControllerImpl::loseFocus");
  clear(Telltale::active);
}

void ControllerImpl::set(Telltale::Flag f)
{
  SectionLog section("ControllerImpl::set");
  if (!CORBA::is_nil(myConstraint)) myConstraint->trymodify(Telltale_var(_this()), f, true);
  else modify(f, true);
}

void ControllerImpl::clear(Telltale::Flag f)
{
  SectionLog section("ControllerImpl::clear");
  if (!CORBA::is_nil(myConstraint)) myConstraint->trymodify(Telltale_var(_this()), f, false);
  else modify(f, false);
}

CORBA::Boolean ControllerImpl::test(Telltale::Flag f)
{
  MutexGuard guard(mutex);
  return flags & (1 << f);
}

void ControllerImpl::modify(Telltale::Flag f, CORBA::Boolean on)
{
  unsigned long fs = 1 << f;
  unsigned long nf = on ? flags | fs : flags & ~fs;
  {
    MutexGuard guard(mutex);
    if (nf == flags) return;
    else flags = nf;
  }
  CORBA::Any any;
  notify(any);
}

void ControllerImpl::constraint(TelltaleConstraint_ptr c)
{
  MutexGuard guard(mutex);
  myConstraint = c;
}

TelltaleConstraint_ptr ControllerImpl::constraint()
{
  MutexGuard guard(mutex);
  return TelltaleConstraint::_duplicate(myConstraint);
}

CORBA::Boolean ControllerImpl::handle(PickTraversal_ptr traversal, const CORBA::Any &any)
{
  SectionLog section("ControllerImpl::handle");
  Event::Pointer *pointer;
  if (any >>= pointer) return handlePositionalEvent(traversal, pointer);
  /* else key event ? */
  else return false;
}

bool ControllerImpl::handlePositionalEvent(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  switch (pointer->whatHappened)
    {
    case Event::press: press(traversal, pointer); break;
    case Event::release: release(traversal, pointer); break;
    case Event::hold:
      if (test(Telltale::toggle)) drag(traversal, pointer);
      else move(traversal, pointer);
      break;
    default: other(traversal, pointer); break;
    }
  return true;
}

bool ControllerImpl::inside(PickTraversal_ptr traversal)
  //. default implementation: use bounding box
{
  return traversal->intersectsAllocation();
}

void ControllerImpl::move(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::press(PickTraversal_ptr traversal, const Event::Pointer *)
{
  grab(traversal);
  set(Telltale::toggle);
}

void ControllerImpl::drag(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::release(PickTraversal_ptr traversal, const Event::Pointer *)
{
  clear(Telltale::toggle);
  ungrab(traversal);
}

void ControllerImpl::doubleClick(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::keyPress(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::keyRelease(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::other(PickTraversal_ptr, const Event::Pointer *)
{
}

void ControllerImpl::grab(PickTraversal_ptr traversal)
{
  traversal->grab();
  grabbed = true;
}

void ControllerImpl::ungrab(PickTraversal_ptr traversal)
{
  traversal->ungrab();
  grabbed = false;
}
