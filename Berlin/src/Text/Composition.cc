/*$Id: Composition.cc,v 1.8 2000/11/14 21:36:37 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include "Text/Composition.hh"
#include "Text/Compositor.hh"
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Traversal.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/Provider.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Tracer.hh>
#include <algorithm>

using namespace Prague;
using namespace Warsaw;

Composition::Composition(DrawingKit_ptr dk, Compositor *c)
  : canonicalDK(DrawingKit::_duplicate(dk)),
    compositor(c),
    requested(false)
{
  Trace trace("Composition::Composition");
}

Composition::~Composition() {}

void Composition::request(Warsaw::Graphic::Requisition &r)
{
  Trace trace("Composition::request");
  if (!requested)
    {
      GraphicImpl::default_requisition(requisition);
      GraphicImpl::init_requisition(requisition);
      long n = num_children();
      if (n > 0)
	{
	  Warsaw::Graphic::Requisition *r = children_requests();
	  compositor->request(n, r, canonicalDK, requisition);
	  deallocate_requisitions(r);
	}
      requested = true;
    }
  r = requisition;
}

void Composition::extension(const Allocation::Info &info, Region_ptr region)
{
  Trace trace("Composition::extension");  
  long n = num_children();
  if (n > 0)
    {
      Allocation::Info child;
      Vertex origin, previous, delta;
      previous.x = previous.y = previous.z = 0;

      Lease_var<TransformImpl> child_tx(Provider<TransformImpl>::provide());
      Lease_var<TransformImpl> tmp_tx(Provider<TransformImpl>::provide());
      tmp_tx->load_identity();
      child_tx->load_identity();
      
      child.transformation = child_tx->_this();
      child.transformation->copy(info.transformation);
      RegionImpl **result = children_allocations(info.allocation);

      for (long i = 0; i < n; i++)
	{
	  result[i]->normalize(origin);
	  delta = origin - previous;
	  tmp_tx->translate(delta);
	  child.allocation = result[i]->_this();
	  child.transformation->premultiply(Transform_var(tmp_tx->_this()));
	  child_extension(i, child, region);
	  previous = origin;
	}
      for (CORBA::Long i = 0; i != n; ++i) Provider<RegionImpl>::adopt(result[i]);
      delete [] result;
    }
}

void Composition::traverse(Traversal_ptr traversal)
{
  Trace trace("Composition::traverse");
  if (num_children())
    {
      /*
       * this cull test is not accurate, it assumes that the children
       * don't draw outside the box' allocation.
       * the alternative - using extension - is expensive...
       *              -stefan
       */
      if (!traversal->intersects_allocation()) return;
      Region_var given = traversal->current_allocation();
      RegionImpl **result = children_allocations(given);
      CORBA::Long size = num_children();
      CORBA::Long begin, end, incr;

      Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
      tx->load_identity();

      if (traversal->direction() == Traversal::up)
	{
	  begin = 0;
	  end = size;
	  incr = 1;
	}
      else
	{
	  begin = size - 1;
	  end = -1;
	  incr = -1;
	}
      for (CORBA::Long i = begin; i != end && traversal->ok(); i += incr)
	{
	  Graphic_var child = _children [i].peer;
	  if (CORBA::is_nil(child)) continue;
	  Vertex origin;
	  result[i]->normalize(origin);
	  tx->load_identity();
	  /*
	   * ok, so we stipulate that Boxes lay out their children 
	   * only translating them -stefan
	   */
	  tx->translate(origin);
	  try { traversal->traverse_child (child, _children[i].localId, Region_var(result[i]->_this()), Transform_var(tx->_this()));}
	  catch (const CORBA::OBJECT_NOT_EXIST &) { _children [i].peer = Warsaw::Graphic::_nil();}
	  catch (const CORBA::COMM_FAILURE &) { _children [i].peer = Warsaw::Graphic::_nil ();}
	}
      for (CORBA::Long i = 0; i != size; ++i) Provider<RegionImpl>::adopt(result[i]);
      delete [] result;
    }
}

void Composition::need_resize()
{
  requested = false;
  PolyGraphic::need_resize();
}

void Composition::need_resize(Tag)
{
  need_resize();
}

void Composition::allocate(Tag tag, const Allocation::Info &info)
{
  Trace trace("Composition::allocate");
  RegionImpl **result = children_allocations(info.allocation);
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  CORBA::Long idx = child_id_to_index(tag);
  result[idx]->normalize(Transform_var(tx->_this()));
  info.transformation->premultiply(Transform_var(tx->_this()));
  info.allocation->copy(Region_var(result[idx]->_this()));
  CORBA::Long children = num_children();
  for (CORBA::Long i = 0; i != children; ++i) Provider<RegionImpl>::adopt(result[i]);
  delete [] result;
}

RegionImpl **Composition::children_allocations(Region_ptr allocation)
{
  Trace trace("Composition::children_allocations");
  CORBA::Long children = num_children();
  Warsaw::Graphic::Requisition *childrenRequisitions = children_requests();
    
  // cache integrated form of children requisitions
  if (!requested)
    {
      GraphicImpl::init_requisition(requisition);
      compositor->request(children, childrenRequisitions, canonicalDK, requisition);
      requested = true;
    }
  // build region array for children
  RegionImpl **childrenRegions = new RegionImpl *[children];

  for (CORBA::Long i = 0; i < children; i++)
    {
      childrenRegions[i] = Provider<RegionImpl>::provide();
      childrenRegions[i]->valid = true;
    }
  // fill in children regions which are reasonable matches for the given requesitions
  compositor->allocate(children, childrenRequisitions, canonicalDK, allocation, childrenRegions);
  deallocate_requisitions(childrenRequisitions);
  return childrenRegions;
}
