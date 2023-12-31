/*$Id: Allocator.cc,v 1.13 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
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
#include "Warsaw/config.hh"
#include "Warsaw/Traversal.hh"
#include "Warsaw/Screen.hh"
#include "Berlin/Allocator.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/AllocationImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"

Allocator::Allocator()
  : requested(false),
    natural(new RegionImpl),
    extension(new RegionImpl)
{
}

Allocator::~Allocator()
{ 
}

void Allocator::request(Requisition &r)
{
  updateRequisition();
  r = requisition;
}

void Allocator::traverse(Traversal_ptr traversal)
{
  updateRequisition();
  Region_var allocation = traversal->allocation();
  if (!CORBA::is_nil(allocation))
    traversal->traverseChild(Graphic_var(body()), 0, allocation, Transform_var(Transform::_nil()));
  else
    traversal->traverseChild(Graphic_var(body()), 0, Region_var(natural->_this()), Transform_var(Transform::_nil()));
}

void Allocator::needResize()
{
  Impl_var<AllocationImpl> allocation(new AllocationImpl);
  allocations(Allocation_var(allocation->_this()));
  Impl_var<RegionImpl> region(new RegionImpl);
  if (extension->valid) region->copy(Region_var(extension->_this()));
  requested = false;
  updateRequisition();
  if (extension->valid) region->mergeUnion(Region_var(extension->_this()));
  if (region->valid) needDamage(region, allocation);
}

void Allocator::allocate(Tag, const Allocation::Info &i)
{
  updateRequisition();
  i.allocation->copy(Region_var(natural->_this()));
}

static void naturalAllocation(Graphic::Requisition &r, RegionImpl &natural)
{
  if (r.x.defined)
    {
      natural.xalign = r.x.align;
      natural.lower.x = -r.x.align * r.x.natural;
      natural.upper.x = natural.lower.x + r.x.natural;
      natural.valid = true;
    }
  if (r.y.defined)
    {
      natural.yalign = r.y.align;
      natural.lower.y = -r.y.align * r.y.natural;
      natural.upper.y = natural.lower.y + r.y.natural;
      natural.valid = true;
    }
  if (r.z.defined)
    {
      natural.zalign = r.z.align;
      natural.lower.z = -r.z.align * r.z.natural;
      natural.upper.z = natural.lower.z + r.z.natural;
      natural.valid = true;
    }
}

void Allocator::updateRequisition()
{
  SectionLog section("Allocator::updateRequisition");
  if (!requested)
    {
      Graphic::Requisition r;
      GraphicImpl::initRequisition(r);
      MonoGraphic::request(r);
      requisition = r;
      ::naturalAllocation(requisition, *natural);
      requested = r.x.defined && r.y.defined; // && r.z.defined;
      extension->valid = false;
      Allocation::Info info;
      MonoGraphic::extension(info, Region_var(extension->_this()));
    }
}

void Allocator::needDamage(RegionImpl *e, Allocation_ptr allocation)
{
  Impl_var<RegionImpl> region(new RegionImpl);
  for (long i = 0; i < allocation->size(); i++)
    {
      Allocation::Info_var info = allocation->get(i);
      region->copy(Region_var(e->_this()));
      region->applyTransform(info->transformation);
      info->root->damage(Region_var(region->_this()));
    }
}

TransformAllocator::TransformAllocator(Alignment xp, Alignment yp, Alignment zp, Alignment xc, Alignment yc, Alignment zc)
  : xparent(xp), yparent(yp), zparent(zp), xchild(xc), ychild(yc), zchild(zc)
{}

TransformAllocator::~TransformAllocator() {}

void TransformAllocator::request(Requisition &r)
{
  Allocator::request(r);
  Coord fil = Coord(1000000.0);
  Coord zero = Coord(0.0);
  r.x.maximum = fil;
  r.x.minimum = zero;
  r.y.maximum = fil;
  r.y.minimum = zero;
  r.z.maximum = fil;
  r.z.minimum = zero;
  requisition.x.maximum = fil;
  requisition.x.minimum = zero;
  requisition.y.maximum = fil;
  requisition.y.minimum = zero;
  requisition.z.maximum = fil;
  requisition.z.minimum = zero;
}

void TransformAllocator::allocate(Tag t, const Allocation::Info &i)
{
  Vertex lower, upper, delta;
  Impl_var<TransformImpl> tx(new TransformImpl);
  Allocator::allocate(t, i);
  i.allocation->bounds(lower, upper);
  computeDelta(lower, upper, delta);
  tx->translate(delta);
  i.transformation->premultiply(Transform_var(tx->_this()));
  i.allocation->copy(natural);
}

void TransformAllocator::traverse(Traversal_ptr traversal)
{
  Impl_var<TransformImpl> tx(new TransformImpl);
  updateRequisition();
  Vertex lower, upper, v;
  traversal->bounds(lower, upper, v);
  computeDelta(lower, upper, v);
  tx->translate(v);
  traversal->traverseChild(Graphic_var(body()), 0, Region_var(natural->_this()), Transform_var(tx->_this()));
}

void TransformAllocator::computeDelta(const Vertex &lower, const Vertex &upper, Vertex &delta)
{
  delta.x = (lower.x - natural->lower.x + xparent * (upper.x - lower.x) -
	     xchild * (natural->upper.x - natural->lower.x));
  delta.y = (lower.y - natural->lower.y + yparent * (upper.y - lower.y) -
	     ychild * (natural->upper.y - natural->lower.y));
  delta.z = (lower.z - natural->lower.z + zparent * (upper.z - lower.z) -
	     zchild * (natural->upper.z - natural->lower.z));
}
