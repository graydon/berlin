/*$Id: PickTraversalImpl.cc,v 1.30 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include <Warsaw/IO.hh>
#include "Berlin/PickTraversalImpl.hh"
#include "Berlin/PositionalFocus.hh"

using namespace Prague;
using namespace Warsaw;

PickTraversalImpl::PickTraversalImpl(Graphic_ptr g, Region_ptr r, Transform_ptr t, PositionalFocus *f)
  : TraversalImpl(g, r, t),
    _focus(f),
    _cursor(0)
{
  Trace trace("PickTraversalImpl::PickTraversalImpl");
  __this = POA_Warsaw::PickTraversal::_this();
}

PickTraversalImpl::PickTraversalImpl(const PickTraversalImpl &traversal)
  : TraversalImpl(traversal),
    _controllers(traversal._controllers),
    _positions(traversal._positions),
    _focus(traversal._focus),
    _cursor(traversal._positions.back() - 1)
{
  __this = POA_Warsaw::PickTraversal::_this();
}

PickTraversalImpl::~PickTraversalImpl() {}

PickTraversalImpl &PickTraversalImpl::operator = (const PickTraversalImpl &traversal)
{
  Trace trace("PickTraversalImpl::operator =");
  TraversalImpl::operator = (traversal);
  _controllers = traversal._controllers;
  _positions = traversal._positions;
  _focus = traversal._focus;
  // the current graphic after a pick isn't the top most graphic in the trail
  // but the top most controller, as it's the controller which will receive the event...
  _cursor = traversal._positions.back() - 1;
  return *this;
}

PickTraversal_ptr PickTraversalImpl::_this()
{
  return Warsaw::PickTraversal::_duplicate(__this);
}

Region_ptr PickTraversalImpl::current_allocation()
{
  Trace trace("PickTraversalImpl::current_allocation");
  return Region::_duplicate(get_allocation(_cursor));
}

Transform_ptr PickTraversalImpl::current_transformation() 
{
  Trace trace("PickTraversalImpl::current_transformation");
  return get_transformation(_cursor)->_this();
}

Graphic_ptr PickTraversalImpl::current_graphic()
{
  Trace trace("PickTraversalImpl::current_graphic");
  return Graphic::_duplicate(get_graphic(_cursor));
}

void PickTraversalImpl::traverse_child(Graphic_ptr child, Tag tag, Region_ptr region, Transform_ptr transform)
{
  Trace trace("PickTraversalImpl::traverse_child");
  if (CORBA::is_nil(region)) region = Region_var(current_allocation());
  Lease_var<TransformImpl> cumulative(Provider<TransformImpl>::provide());
  *cumulative = *get_transformation(_cursor);
  if (!CORBA::is_nil(transform)) cumulative->premultiply(transform);
  push(child, tag, region, cumulative);
  _cursor++;
  try { child->traverse(__this);}
  catch (...) { _cursor--; pop(); throw;}
  _cursor--;
  pop(); 
}

void PickTraversalImpl::visit(Warsaw::Graphic_ptr g) { g->pick(__this);}
Warsaw::Traversal::order PickTraversalImpl::direction() { return Warsaw::Traversal::down;}
CORBA::Boolean PickTraversalImpl::ok() { return !picked();}

CORBA::Boolean PickTraversalImpl::intersects_allocation()
{
  Region_var region = current_allocation();
  return intersects_region(region);
}

void PickTraversalImpl::enter_controller(Controller_ptr c)
{
  Trace trace("PickTraversal::enter_controller");
  _controllers.push_back(Controller::_duplicate(c));
  _positions.push_back(size());
}

void PickTraversalImpl::leave_controller()
{
  Trace trace("PickTraversal::leave_controller");
  _controllers.pop_back();
  _positions.pop_back();
}

Focus_ptr PickTraversalImpl::get_focus() { return _focus ? _focus->_this() : Focus::_nil();}
CORBA::Boolean PickTraversalImpl::forward()
{
  if (_cursor + 1 < size()) { ++_cursor; return true;}
  return false;
}

CORBA::Boolean PickTraversalImpl::backward()
{
  if (_cursor > _positions.back()) { --_cursor; return true;}
  return false;
}
