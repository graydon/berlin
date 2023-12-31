/*$Id: TraversalImpl.cc,v 1.39 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Warsaw/Allocation.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/IO.hh>
#include "Berlin/Provider.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/RefCountVar.hh"
#include "Berlin/TraversalImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/RegionImpl.hh"

using namespace Prague;
using namespace Warsaw;

TraversalImpl::TraversalImpl(Graphic_ptr g, Region_ptr r, Transform_ptr t)
{
  Trace trace("TraversalImpl::TraversalImpl");
  Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
  transform->copy(t);
  push(g, 0, r, transform._retn());
}

TraversalImpl::TraversalImpl(const TraversalImpl &traversal)
  : _stack(traversal.size())
{
  Trace trace("TraversalImpl::TraversalImpl(const TraversalImpl &)");
  // explicitely copy the stack so we are the owner and can delete it in the destructor
  stack_t::iterator i = _stack.begin();
  stack_t::const_iterator j = traversal._stack.begin();
  for (; i != _stack.end(); ++i, ++j)
    {
      (*i).graphic = Warsaw::Graphic::_duplicate((*j).graphic);
      (*i).id      = (*j).id;
      (*i).allocation = Warsaw::Region::_duplicate((*j).allocation);
      (*i).transformation = Provider<TransformImpl>::provide();
      *(*i).transformation = *(*j).transformation;
    };
}

TraversalImpl::~TraversalImpl()
{
  clear();
}

TraversalImpl &TraversalImpl::operator = (const TraversalImpl &traversal)
{
  Trace trace("TraversalImpl::operator = (const TraversalImpl &)");
  clear();
  // explicitely copy the stack so we are the owner and can delete it in the destructor
  _stack.resize(traversal._stack.size());
  stack_t::iterator i = _stack.begin();
  stack_t::const_iterator j = traversal._stack.begin();
  for (; i != _stack.end(); ++i, ++j)
    {
      (*i).graphic = Warsaw::Graphic::_duplicate((*j).graphic);
      (*i).id      = (*j).id;
      (*i).allocation = Warsaw::Region::_duplicate((*j).allocation);
      (*i).transformation = Provider<TransformImpl>::provide();
      *(*i).transformation = *(*j).transformation;
    };
}

Region_ptr TraversalImpl::current_allocation()
{
  Trace trace("TraversalImpl::current_allocation");
  return Region::_duplicate(_stack.back().allocation);
}

Transform_ptr TraversalImpl::current_transformation() 
{
  Trace trace("TraversalImpl::current_transformation");
  return _stack.back().transformation->_this();
}

Graphic_ptr TraversalImpl::current_graphic()
{
  Trace trace("TraversalImpl::current_graphic");
  return Graphic::_duplicate(_stack.back().graphic);
}

CORBA::Boolean TraversalImpl::bounds(Vertex &lower, Vertex &upper, Vertex &origin) 
{
  Trace trace("TraversalImpl::bounds");
  bool b = false;
  Region_ptr allocation = _stack.back().allocation;
  if (!CORBA::is_nil(allocation))
    {
      allocation->bounds(lower, upper);
      allocation->origin(origin);
      b = true;
    }
  return b;
}

void TraversalImpl::push(Graphic_ptr g, Tag id, Region_ptr r, TransformImpl *t)
{
  Trace trace("TraversalImpl::push");
  _stack.push_back(State(g, id, r, t));
}

void TraversalImpl::pop()
{
  Trace trace("TraversalImpl::pop");
  _stack.erase(_stack.end() - 1);
}

void TraversalImpl::update()
{
  Trace trace("TraversalImpl::update");
  if (_stack.size() == 1) return;
  stack_t::iterator parent = _stack.begin();
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy((*parent).allocation);
  Lease_var<TransformImpl> transformation(Provider<TransformImpl>::provide());
  *transformation = *(*parent).transformation;
  Allocation::Info info;
  info.allocation = allocation->_this();
  info.transformation = transformation->_this();
  for (stack_t::iterator child = parent + 1; child != _stack.end(); ++parent, ++child)
    {
      // recompute the allocation info for the child, given the (just updated)
      // allocation for the parent
      (*parent).graphic->allocate((*child).id, info);
      (*child).allocation->copy(info.allocation);
      *(*child).transformation = *transformation;
    }
}

void TraversalImpl::clear()
{
  // this assumes we own all items
  for (stack_t::iterator i = _stack.begin(); i != _stack.end(); ++i)
    {
      CORBA::release((*i).graphic);
      CORBA::release((*i).allocation);
      Provider<TransformImpl>::adopt((*i).transformation);
    };
}
