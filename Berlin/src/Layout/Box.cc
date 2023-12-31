/*$Id: Box.cc,v 1.32 2000/11/14 21:36:37 stefan Exp $
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
#include <Warsaw/config.hh>
#include <Warsaw/Traversal.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/Provider.hh>
#include <Berlin/TransformImpl.hh>
#include <Prague/Sys/Tracer.hh>
#include <iostream>
#include "Layout/Box.hh"
#include "Layout/LayoutManager.hh"
#include "Layout/Placement.hh"

using namespace Prague;
using namespace Warsaw;

Box::Box(LayoutManager *l)
  : layout(l),
    requested(false)
{
}

Box::~Box() { delete layout;}

void Box::request(Warsaw::Graphic::Requisition &r)
{
  Trace trace("Box::request");
  if (!requested)
    {
      GraphicImpl::default_requisition(requisition);
      GraphicImpl::init_requisition(requisition);
      long n = num_children();
      if (n > 0)
	{
	  Warsaw::Graphic::Requisition *r = children_requests();
	  layout->request(n, r, requisition);
	  deallocate_requisitions(r);
	}
      requested = true;
    }
  r = requisition;
}

void Box::extension(const Allocation::Info &info, Region_ptr region)
{
  Trace trace("Box::extension");  
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
      LayoutManager::Allocations result = children_allocations(info.allocation);
      for (long i = 0; i < n; i++)
	{
	  result[i]->normalize(origin);
	  delta = origin - previous;
// 	  tmp_tx->loadIdentity();
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

void Box::traverse(Traversal_ptr traversal)
{
  Trace trace("Box::traverse");
  if (num_children())
    {
      Region_var given = traversal->current_allocation();
      if (!CORBA::is_nil(given))
 	{
	  /*
	   * this cull test is not accurate, it assumes that the children
	   * don't draw outside the box' allocation.
	   * the alternative - using extension - is expensive...
	   *              -stefan
	   */
	  if (traversal->intersects_allocation())
	    traverse_with_allocation(traversal, given);
	}
      else traverse_without_allocation(traversal);
    }
}

void Box::need_resize()
{
  requested = false;
  PolyGraphic::need_resize();
}

void Box::need_resize(Tag)
{
  need_resize();
}

/*
 * this is a method called (but left empty in the superclass) in
 * PolyGraphic::allocate. it is called after a particular child has been
 * located in the child list. It is supposed to "finish off" providing the
 * allocation info for the given child 
 */
void Box::allocate(Tag tag, const Allocation::Info &info)
{
  /*
   * fetch requested (presumably allocated) child regions
   */
  CORBA::Long n = num_children();
  LayoutManager::Allocations result = children_allocations(info.allocation);
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();

  /*
   * copy transformation and region into allocation
   */
  CORBA::Long idx = child_id_to_index(tag);
  result[idx]->normalize(Transform_var(tx->_this()));
  info.transformation->premultiply(Transform_var(tx->_this()));
  info.allocation->copy(Region_var(result[idx]->_this()));
  for (CORBA::Long i = 0; i != n; ++i) Provider<RegionImpl>::adopt(result[i]);
  delete [] result;
}


/*
 * this is called from Box::allocate to resolve the layout of the box's
 * children by (a) asking the children how big they want to be, then (b)
 * delegating the actual allocation to the current layoutManager. It also caches
 * the children's requests so that the real layout (at draw time) will happen
 * faster. 
 */
LayoutManager::Allocations Box::children_allocations(Region_ptr allocation)
{
  Trace trace("Box::children_allocations");
  CORBA::Long children = num_children();
  Warsaw::Graphic::Requisition *childrenRequisitions = children_requests();
    
  // cache integrated form of children requisitions
  if (!requested)
    {
      GraphicImpl::init_requisition(requisition);
      layout->request(children, childrenRequisitions, requisition);
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
  layout->allocate(children, childrenRequisitions, allocation, childrenRegions);
  deallocate_requisitions(childrenRequisitions);
  return childrenRegions;
}

void Box::traverse_with_allocation(Traversal_ptr t, Region_ptr r)
{
  Trace trace("Box::traverse_with_allocation");
  LayoutManager::Allocations result;
  result = children_allocations(r);
  CORBA::Long size = num_children();
  CORBA::Long begin, end, incr;

  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();

  if (t->direction() == Traversal::up)
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
  for (CORBA::Long i = begin; i != end; i += incr)
    {
      if (CORBA::is_nil(_children[i].peer)) continue;
      Vertex origin;
      result[i]->normalize(origin);
      tx->load_identity();
      /*
       * ok, so we stipulate that Boxes lay out their children 
       * only translating them -stefan
       */
      tx->translate(origin);
      try { t->traverse_child(_children[i].peer, _children[i].localId, Region_var(result[i]->_this()), Transform_var(tx->_this()));}
      catch (const CORBA::OBJECT_NOT_EXIST &) { _children [i].peer = Warsaw::Graphic::_nil ();}
      catch (const CORBA::COMM_FAILURE &) { _children [i].peer = Warsaw::Graphic::_nil ();}
      if (!t->ok()) break;
    }
  for (CORBA::Long i = 0; i != size; ++i) Provider<RegionImpl>::adopt(result[i]);
  delete [] result;
}

void Box::traverse_without_allocation(Traversal_ptr t)
{
  Trace trace("Box::traverse_without_allocation");
  if (t->direction() == Traversal::up)
    {
      for (glist_t::iterator i = _children.begin(); i != _children.end() && t->ok(); i++)
	{
	  if (CORBA::is_nil(i->peer)) continue;
	  try { t->traverse_child (i->peer, i->localId, Region::_nil (), Transform::_nil ());}
	  catch (const CORBA::OBJECT_NOT_EXIST &) { i->peer = Warsaw::Graphic::_nil ();}
	  catch (const CORBA::COMM_FAILURE &) { i->peer = Warsaw::Graphic::_nil ();}
	}
    }
  else
    {
      for (glist_t::reverse_iterator i = _children.rbegin(); i != _children.rend() && t->ok(); i++)
	{
	  if (CORBA::is_nil (i->peer)) continue;
	  try { t->traverse_child (i->peer, i->localId, Region::_nil(), Transform::_nil());}
	  catch (const CORBA::OBJECT_NOT_EXIST &) { i->peer = Warsaw::Graphic::_nil();}
	  catch (const CORBA::COMM_FAILURE &) { i->peer = Warsaw::Graphic::_nil();}
	}
    }
}

BoxAlignElements::BoxAlignElements(LayoutManager *layout, Axis a, Alignment align)
  : Box(layout), axis(a), alignment(align) {}
BoxAlignElements::~BoxAlignElements() {}

void BoxAlignElements::append_graphic(Graphic_ptr g)
{
  Placement *placement = new Placement(new LayoutCenter(axis, alignment));
  placement->body(g);
  Box::append_graphic(Graphic_var(placement->_this()));
}

void BoxAlignElements::prepend_graphic(Graphic_ptr g)
{
  Placement *placement = new Placement(new LayoutCenter(axis, alignment));
  placement->body(g);
  Box::prepend_graphic(Graphic_var(placement->_this()));
}
