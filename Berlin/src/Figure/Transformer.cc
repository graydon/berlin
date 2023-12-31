/*$Id: Transformer.cc,v 1.14 2001/04/24 05:04:49 stefan Exp $
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
#include <Warsaw/Region.hh>
#include <Warsaw/Traversal.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/IO.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/Provider.hh>
#include <Berlin/RegionImpl.hh>
#include "Figure/Transformer.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

Transformer::Transformer() : transform(new TransformImpl) {}
Transformer::~Transformer() {}
Transform_ptr Transformer::transformation() { return transform->_this();}

void Transformer::request(Warsaw::Graphic::Requisition &requisition)
{
  Trace trace("Transformer::request");
  Allocator::request(requisition);
  GraphicImpl::transform_request(requisition, Transform_var(transform->_this()));
}

void Transformer::traverse(Traversal_ptr traversal)
{
  Trace trace("Transformer::traverse");
  if (transform->identity())
    {
      Allocator::traverse(traversal);
    }
  else
    {
      Warsaw::Graphic::Requisition r;
      GraphicImpl::init_requisition(r);
      Allocator::request(r);
      Graphic_var child = body();
      if (CORBA::is_nil(child))	return;
      Lease_var<RegionImpl> rr(Provider<RegionImpl>::provide());
      rr->copy(Region_var(traversal->current_allocation()));
      Vertex delta = GraphicImpl::transform_allocate(*rr, r, Transform_var(transform->_this()));
      Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
      tx->copy(Transform_var(transform->_this()));
      try { traversal->traverse_child (child, 0, Region_var(rr->_this()), Transform_var(tx->_this()));}
      catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
      catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
    }
}

void Transformer::allocate(Tag, const Allocation::Info &info)
{
  Trace trace("Transformer::allocate");
  if (!transform->identity())
    {
      if (!CORBA::is_nil(info.allocation))
	{
	  Lease_var<RegionImpl> rr(Provider<RegionImpl>::provide());
	  rr->copy(info.allocation);
	  Warsaw::Graphic::Requisition r;
	  GraphicImpl::init_requisition(r);
	  Allocator::request(r);
	  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
	  tx->load_identity();
	  Vertex delta = GraphicImpl::transform_allocate(*rr, r, Transform_var(transform->_this()));
 	  tx->copy(Transform_var(transform->_this()));
	  info.transformation->premultiply(Transform_var(tx->_this()));
	  info.allocation->copy(Region_var(rr->_this()));
        }
      else
	{
	  info.transformation->premultiply(Transform_var(transform->_this()));
	  Allocator::allocate(0, info);
        }
    }
  else Allocator::allocate(0, info);
}
