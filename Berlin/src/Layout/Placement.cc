/*$Id: Placement.cc,v 1.9 1999/11/06 20:23:08 stefan Exp $
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
#include "Berlin/TraversalImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Layout/Placement.hh"
#include "Layout/LayoutManager.hh"

Placement::Placement(LayoutManager *l)
{
  layout = l;
  region = new RegionImpl;
  region->_obj_is_ready(CORBA::BOA::getBOA());
}

Placement::~Placement()
{
  region->_dispose();
  delete layout;
}

void Placement::request(Requisition &r)
{
  MonoGraphic::request(r);
  layout->request(0, 0, r);
}

void Placement::traverse(Traversal_ptr traversal)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  /*
   * cheap and dirty cull test -stefan
   */
//   if (!traversal->intersectsAllocation()) return;
  Region_var allocation = Region::_duplicate(traversal->allocation());
  if (!CORBA::is_nil(allocation))
    {
      Impl_var<RegionImpl> result(new RegionImpl(allocation));
      Graphic::Requisition r;
      GraphicImpl::initRequisition(r);
      MonoGraphic::request(r);
      RegionImpl *tmp = result.get();
      layout->allocate(1, &r, allocation, &tmp);
      Impl_var<TransformImpl> tx(new TransformImpl);
      result->normalize(tx);
      traversal->traverseChild(child, 0, Region_var(result->_this()), Transform_var(tx->_this()));
    }
  else MonoGraphic::traverse(traversal);
}

void Placement::allocate(Tag, const Allocation::Info &a)
{
  region->copy(a.allocation);
  Graphic::Requisition r;
  GraphicImpl::initRequisition(r);
  MonoGraphic::request(r);
  layout->allocate(1, &r, a.allocation, &region);

  Impl_var<TransformImpl> tx(new TransformImpl);
  region->normalize(tx);
  a.transformation->premultiply(Transform_var(tx->_this()));
  a.allocation->copy(Region_var(region->_this()));
}

LayoutLayer::LayoutLayer(Graphic_ptr between, Graphic_ptr under, Graphic_ptr over)
{
  body(between);
  under = Graphic::_duplicate(under);
  over = Graphic::_duplicate(over);
}

LayoutLayer::~LayoutLayer()
{
}

void LayoutLayer::traverse(Traversal_ptr t)
{
  if (!CORBA::is_nil(under)) under->traverse(t);
  MonoGraphic::traverse(t);
  if (!CORBA::is_nil(over)) over->traverse(t);
}
