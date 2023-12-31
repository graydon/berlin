/*$Id: Allocator.cc,v 1.25 2001/04/24 03:27:37 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Warsaw/config.hh>
#include <Warsaw/Traversal.hh>
#include <Warsaw/Screen.hh>
#include "Berlin/Allocator.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/AllocationImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Provider.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

Allocator::Allocator()
  : _requested(false),
    _natural(new RegionImpl),
    _extension(new RegionImpl)
{
}

Allocator::~Allocator()
{ 
}

void Allocator::request(Warsaw::Graphic::Requisition &r)
{
  Trace trace("Allocator::request");
  cache_requisition();
  r = _requisition;
}

void Allocator::traverse(Traversal_ptr traversal)
{
  Trace trace("Allocator::traverse");
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  Region_var allocation = traversal->current_allocation();
  try
    {
      if (!CORBA::is_nil(allocation))
	traversal->traverse_child(child, 0, allocation, Transform::_nil());
      else
	traversal->traverse_child(child, 0, Region_var(_natural->_this()), Transform::_nil());
    }
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}

void Allocator::need_resize()
{
  Trace trace("Allocator::need_resize");
  Lease_var<AllocationImpl> allocation(Provider<AllocationImpl>::provide());
  allocation->clear();
  allocations(Allocation_var(allocation->_this()));
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  region->clear();
  if (_extension->valid) region->copy(Region_var(_extension->_this()));
  _requested = false;
  _allocated = false;
  cache_requisition();
  cache_allocation();
  if (_extension->valid) region->merge_union(Region_var(_extension->_this()));
  if (region->valid) need_damage(region, Allocation_var(allocation->_this()));
  MonoGraphic::need_resize();
}

void Allocator::allocate(Tag, const Allocation::Info &i)
{
  Trace trace("Allocator::allocate");
//   updateRequisition();
//  i.allocation->copy(Region_var(natural->_this()));
}

void Allocator::natural_allocation(const Warsaw::Graphic::Requisition &r, RegionImpl &natural)
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

void Allocator::cache_requisition()
{
  Trace trace("Allocator::cache_requisition");
  if (!_requested)
    {
      Warsaw::Graphic::Requisition r;
      GraphicImpl::init_requisition(r);
      MonoGraphic::request(r);
      _requisition = r;
      _requested = true;//r.x.defined && r.y.defined && r.z.defined;
    }
}

void Allocator::cache_allocation()
{
  Trace trace("Allocator::cache_allocation");
  cache_requisition();
  if (!_allocated)
    {
      natural_allocation(_requisition, *_natural);
      _extension->valid = false;
      Lease_var<RegionImpl> tmp(Provider<RegionImpl>::provide());
      tmp->clear();
      Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
      transform->load_identity();
      Allocation::Info info;
      info.allocation = tmp->_this();
      info.transformation = transform->_this();
      MonoGraphic::extension(info, Region_var(_extension->_this()));
      _allocated = true;
    }
}

void Allocator::need_damage(RegionImpl *e, Allocation_ptr allocation)
{
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  region->clear();

  for (long i = 0; i < allocation->size(); i++)
    {
      Allocation::Info_var info = allocation->get(i);
      region->copy(Region_var(e->_this()));
      region->apply_transform(info->transformation);
      info->root->damage(Region_var(region->_this()));
    }
}

TransformAllocator::TransformAllocator(Alignment xp, Alignment yp, Alignment zp, Alignment xc, Alignment yc, Alignment zc)
  : _xparent(xp), _yparent(yp), _zparent(zp), _xchild(xc), _ychild(yc), _zchild(zc)
{}

TransformAllocator::~TransformAllocator() {}

void TransformAllocator::request(Warsaw::Graphic::Requisition &r)
{
  Trace trace("TransformAllocator::request");
  Allocator::request(r);
  Coord fill = 1000000.;
  Coord zero = 0.;
  r.x.maximum = fill;
  r.x.minimum = zero;
  r.y.maximum = fill;
  r.y.minimum = zero;
  r.z.maximum = fill;
  r.z.minimum = zero;
//   requisition.x.maximum = fil;
//   requisition.x.minimum = zero;
//   requisition.y.maximum = fil;
//   requisition.y.minimum = zero;
//   requisition.z.maximum = fil;
//   requisition.z.minimum = zero;
}

void TransformAllocator::allocate(Tag t, const Allocation::Info &i)
{
  Trace trace("TransformAllocator::allocate");
  Vertex lower, upper, delta;
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  Allocator::allocate(t, i);
  i.allocation->bounds(lower, upper);
  compute_delta(lower, upper, delta);
  tx->translate(delta);
  i.transformation->premultiply(Transform_var(tx->_this()));
  i.allocation->copy(Region_var(_natural->_this()));
}

void TransformAllocator::traverse(Traversal_ptr traversal)
{
  Trace trace("TransformAllocator::traverse");
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  cache_allocation();
  Vertex lower, upper, v;
  traversal->bounds(lower, upper, v);
  compute_delta(lower, upper, v);
  tx->translate(v);
  try
    {
      traversal->traverse_child(child, 0, Region_var(_natural->_this()), Transform_var(tx->_this()));
    }
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}

void TransformAllocator::compute_delta(const Vertex &lower, const Vertex &upper, Vertex &delta)
{
  delta.x = (lower.x - _natural->lower.x + _xparent * (upper.x - lower.x) -
	     _xchild * (_natural->upper.x - _natural->lower.x));
  delta.y = (lower.y - _natural->lower.y + _yparent * (upper.y - lower.y) -
	     _ychild * (_natural->upper.y - _natural->lower.y));
  delta.z = (lower.z - _natural->lower.z + _zparent * (upper.z - lower.z) -
	     _zchild * (_natural->upper.z - _natural->lower.z));
}
