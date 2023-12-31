/*$Id: Box.cc,v 1.18 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#include "Layout/Box.hh"
#include "Layout/LayoutManager.hh"
#include "Layout/Placement.hh"
#include "Berlin/TransformImpl.hh"
#include "Warsaw/Traversal.hh"
#include "Berlin/Logger.hh"
#include "Berlin/ImplVar.hh"
#include <iostream>

Box::Box(LayoutManager *l)
{
  layout = l;
  requested = false;
}

Box::~Box() { delete layout;}

void Box::request(Requisition &r)
{
  SectionLog section("Box::request");
  if (!requested)
    {
      GraphicImpl::defaultRequisition(requisition);
      GraphicImpl::initRequisition(requisition);
      long n = numChildren();
      if (n > 0)
	{
	  Graphic::Requisition *r = childrenRequests();
	  layout->request(n, r, requisition);
	  deallocateRequisitions(r);
	}
      requested = true;
    }
  r = requisition;
}

void Box::extension(const Allocation::Info &info, Region_ptr region)
{
  SectionLog section("Box::extension");  
  long n = numChildren();
  if (n > 0)
    {
      Allocation::Info child;
      Vertex origin, previous, delta;
      previous.x = previous.y = previous.z = 0;

      Impl_var<TransformImpl> child_tx(new TransformImpl);
      Impl_var<TransformImpl> tmp_tx(new TransformImpl);
      
      child.transformation = child_tx->_this();
      child.transformation->copy(info.transformation);
      RegionImpl **result = childrenAllocations(info.allocation);

      for (long i = 0; i < n; i++)
	{
	  result[i]->normalize(origin);
	  delta = origin - previous;
// 	  tmp_tx->loadIdentity();
	  tmp_tx->translate(delta);
	  child.allocation = result[i]->_this();
	  child.transformation->premultiply(Transform_var(tmp_tx->_this()));
	  childExtension(i, child, region);
	  previous = origin;
	}
      for (long i = 0; i < n; i++) result[i]->_dispose();
      delete [] result;
    }
}

void Box::traverse(Traversal_ptr traversal)
{
  SectionLog section("Box::traverse");
  if (numChildren())
    {
      Region_var given = traversal->allocation();
      if (!CORBA::is_nil(given))
 	{
	  /*
	   * this cull test is not accurate, it assumes that the children
	   * don't draw outside the box' allocation.
	   * the alternative - using extension - is expensive...
	   *              -stefan
	   */
	  if (traversal->intersectsAllocation())
	    traverseWithAllocation(traversal, given);
	}
      else traverseWithoutAllocation(traversal);
    }
}

void Box::needResize()
{
  requested = false;
  PolyGraphic::needResize();
}

void Box::needResize(Tag)
{
  needResize();
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
  RegionImpl **result = childrenAllocations(info.allocation);
  Impl_var<TransformImpl> tx(new TransformImpl);
  /*
   * copy transformation and region into allocation
   */
  CORBA::Long idx = index(tag);
  result[idx]->normalize(tx);
  info.transformation->premultiply(Transform_var(tx->_this()));
  info.allocation->copy(Region_var(result[idx]->_this()));
  CORBA::Long children = numChildren();
  for (CORBA::Long i = 0; i < children; i++) result[i]->_dispose();
  delete [] result;
}


/*
 * this is called from Box::allocate to resolve the layout of the box's
 * children by (a) asking the children how big they want to be, then (b)
 * delegating the actual allocation to the current layoutManager. It also caches
 * the children's requests so that the real layout (at draw time) will happen
 * faster. 
 */
RegionImpl **Box::childrenAllocations(Region_ptr allocation)
{
  SectionLog section("Box::childrenAllocations");
  CORBA::Long children = numChildren();
  Graphic::Requisition *childrenRequisitions = childrenRequests(); // first defined  in PolyGraphic.cc
    
  // cache integrated form of children requisitions
  if (!requested)
    {
      GraphicImpl::initRequisition(requisition);
      layout->request(children, childrenRequisitions, requisition);
      requested = true;
    }
  // build region array for children
  RegionImpl **childrenRegions = new RegionImpl *[children];
  for (CORBA::Long i = 0; i < children; i++)
    {
      childrenRegions[i] = new RegionImpl;
      childrenRegions[i]->_obj_is_ready(_boa());
      childrenRegions[i]->valid = true;
    }
  // fill in children regions which are reasonable matches for the given requesitions
  layout->allocate(children, childrenRequisitions, allocation, childrenRegions);
  deallocateRequisitions(childrenRequisitions);
  return childrenRegions;
}

void Box::traverseWithAllocation(Traversal_ptr t, Region_ptr r)
{
  SectionLog section("Box::traverseWithAllocation");
  RegionImpl **result = childrenAllocations(r);
  CORBA::Long size = numChildren();
  CORBA::Long begin, end, incr;
  Impl_var<TransformImpl> tx(new TransformImpl);
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
      Vertex origin;
      result[i]->normalize(origin);
      tx->loadIdentity();
      /*
       * ok, so we stipulate that Boxes lay out their children 
       * only translating them -stefan
       */
      tx->translate(origin);
      t->traverseChild(children[i].first, children[i].second, Region_var(result[i]->_this()), Transform_var(tx->_this()));
      if (!t->ok()) break;
    }
  for (long i = 0; i < size; i++) result[i]->_dispose();
  delete [] result;
}

void Box::traverseWithoutAllocation(Traversal_ptr t)
{
  SectionLog section("Box::traverseWithoutAllocation");
  if (t->direction() == Traversal::up)
    for (clist_t::iterator i = children.begin(); i != children.end(); i++)
      {
	t->traverseChild((*i).first, (*i).second, Region_var(Region::_nil()), Transform_var(Transform::_nil()));
	if (!t->ok()) break;
      }
  else
    for (clist_t::reverse_iterator i = children.rbegin(); i != children.rend(); i++)
      {
	t->traverseChild((*i).first, (*i).second, Region_var(Region::_nil()), Transform_var(Transform::_nil()));
	if (!t->ok()) break;
      }    
}

BoxAlignElements::BoxAlignElements(LayoutManager *layout, Axis a, Alignment align)
  : Box(layout), axis(a), alignment(align) {}
BoxAlignElements::~BoxAlignElements() {}

void BoxAlignElements::append(Graphic_ptr g)
{
  Placement *placement = new Placement(new LayoutCenter(axis, alignment));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  Box::append(Graphic_var(placement->_this()));
}

void BoxAlignElements::prepend(Graphic_ptr g)
{
  Placement *placement = new Placement(new LayoutCenter(axis, alignment));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  Box::prepend(Graphic_var(placement->_this()));
}
