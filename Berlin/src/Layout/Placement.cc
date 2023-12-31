/*$Id: Placement.cc,v 1.17 2000/11/14 21:36:37 stefan Exp $
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
#include <Berlin/TraversalImpl.hh>
#include <Berlin/Provider.hh>
#include <Berlin/ImplVar.hh>
#include "Layout/Placement.hh"
#include "Layout/LayoutManager.hh"

using namespace Warsaw;

Placement::Placement(LayoutManager *l)
{
  layout = l;
  region = new RegionImpl;
}

Placement::~Placement()
{
  delete layout;
}

void Placement::request(Warsaw::Graphic::Requisition::Requisition &r)
{
  MonoGraphic::request(r);
  layout->request(0, 0, r);
}

void Placement::traverse(Traversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  if (!CORBA::is_nil(allocation))
    {
      Warsaw::Graphic::Requisition r;
      GraphicImpl::init_requisition(r);
      MonoGraphic::request(r);
      Graphic_var child = body();
      if (CORBA::is_nil(child)) return;
      Lease_var<RegionImpl> result(Provider<RegionImpl>::provide());
      result->copy(allocation);
      RegionImpl *tmp = static_cast<RegionImpl *>(result);
      layout->allocate(1, &r, allocation, &tmp);
      Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
      tx->load_identity();
      result->normalize(Transform_var(tx->_this()));
      try { traversal->traverse_child (child, 0, Region_var(result->_this()), Transform_var(tx->_this()));}
      catch (const CORBA::OBJECT_NOT_EXIST &) { body (Warsaw::Graphic::_nil());}
      catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
    }
  else MonoGraphic::traverse(traversal);
}

void Placement::allocate(Tag, const Allocation::Info &a)
{
  region->copy(a.allocation);
  Warsaw::Graphic::Requisition r;
  GraphicImpl::init_requisition(r);
  MonoGraphic::request(r);
  RegionImpl *cast = region;
  layout->allocate(1, &r, a.allocation, &cast);
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  region->normalize(Transform_var(tx->_this()));
  a.transformation->premultiply(Transform_var(tx->_this()));
  a.allocation->copy(Region_var(region->_this()));
}

LayoutLayer::LayoutLayer(Graphic_ptr between, Graphic_ptr under, Graphic_ptr over)
{
  body(between);
  under = Warsaw::Graphic::_duplicate(under);
  over = Warsaw::Graphic::_duplicate(over);
}

LayoutLayer::~LayoutLayer()
{
}

void LayoutLayer::traverse(Traversal_ptr t)
{
  if (!CORBA::is_nil(under))
    try { under->traverse (t);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { under = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { under = Warsaw::Graphic::_nil();}
  MonoGraphic::traverse(t);
  if (!CORBA::is_nil(over))
    try { over->traverse (t);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { over = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { over = Warsaw::Graphic::_nil();}
}
