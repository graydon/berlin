/*$Id: TraversalImpl.cc,v 1.23 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
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
#include "Berlin/TraversalImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"
#include "Warsaw/Allocation.hh"
#include "Warsaw/Graphic.hh"
#include "Warsaw/Region.hh"

TraversalImpl::TraversalImpl(Graphic_ptr g, Region_ptr r, Transform_ptr t)
{
  Impl_var<TransformImpl> transform(new TransformImpl);
  transform->copy(t);
  push(g, 0, r, transform.release());
}

TraversalImpl::TraversalImpl(const TraversalImpl &t)
{
  for (stack_t::const_iterator i = t.stack.begin(); i != t.stack.end(); i++)
    {
      State state;
      state.graphic = Graphic::_duplicate((*i).graphic);
      state.tag = (*i).tag;
      state.allocation = Region::_duplicate((*i).allocation);
      Impl_var<TransformImpl> transform(new TransformImpl);
      transform->copy((*i).transformation);
      state.transformation = transform.release();
      stack.push_back(state);
    }
}

TraversalImpl::~TraversalImpl()
{
  for (stack_t::const_iterator i = stack.begin(); i != stack.end(); i++)
    (*i).transformation->_dispose();
}

Region_ptr TraversalImpl::allocation()
{
  return Region::_duplicate(stack.back().allocation);
}

Transform_ptr TraversalImpl::transformation() 
{
  return stack.back().transformation->_this();
}

CORBA::Boolean TraversalImpl::bounds(Vertex &lower, Vertex &upper, Vertex &origin) 
{
  bool b = false;
  State &state = stack.back();
  Region_ptr r = state.allocation;
  if (!CORBA::is_nil(r))
    {
      r->bounds(lower, upper);
      r->origin(origin);
      b = true;
    }
  return b;
}

void TraversalImpl::traverseChild(Graphic_ptr child, Tag tag, Region_ptr region, Transform_ptr t)
{
  SectionLog section("TraversalImpl::traverseChild");
  if (CORBA::is_nil(region)) region = Region_var(allocation());
  Impl_var<TransformImpl> cumulative(new TransformImpl);
  cumulative->copy(Transform_var(transformation()));
  if (!CORBA::is_nil(t)) cumulative->premultiply(t);
#if 1
  push(child, tag, region, cumulative.release());
  child->traverse(Traversal_var(_this()));
  pop();
#else
  Allocation::Info_var info = new Allocation::Info;
  info->allocation = Region::_duplicate(region);
  info->transformation = cumulative->_this();
  Impl_var<RegionImpl> ext(new RegionImpl);
  child->extension(info, Region_var(ext->_this()));
  if (intersectsRegion(Region_var(ext->_this())))
      {
	push(child, tag, region, cumulative);
	child->traverse(Traversal_var(_this()));
	pop();
      }
#endif
}

void TraversalImpl::push(Graphic_ptr g, Tag tag, Region_ptr r, TransformImpl *t)
{
  SectionLog section("TraversalImpl::push");
  State state;
  state.graphic = Graphic::_duplicate(g);
  state.tag = tag;
  state.allocation = Region::_duplicate(r);
  state.transformation = t;
  stack.push_back(state);
}

void TraversalImpl::pop()
{
  SectionLog section("TraversalImpl::pop");
  State &state = *stack.rbegin();
  state.transformation->_dispose();
  stack.erase(stack.end() - 1);
}

void TraversalImpl::update()
{
  SectionLog section("TraversalImpl::update");
  if (stack.size() == 1) return;
  stack_t::iterator parent = stack.begin();
  Impl_var<RegionImpl> allocation(new RegionImpl((*parent).allocation));
  Impl_var<TransformImpl> transformation(new TransformImpl);
  transformation->copy((*parent).transformation);
  Allocation::Info info;
  info.allocation = allocation->_this();
  info.transformation = transformation->_this();
  for (stack_t::iterator child = parent + 1; child != stack.end(); parent++, child++)
    {
      (*parent).graphic->allocate((*child).tag, info);
      (*child).allocation->copy(info.allocation);
      (*child).transformation->copy(info.transformation);
//       (*child).transformation->copy((*parent).transformation);
//       (*child).transformation->premultiply(info.transformation);
    }
}
