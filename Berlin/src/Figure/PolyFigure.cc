/*$Id: PolyFigure.cc,v 1.12 2000/11/14 21:36:36 stefan Exp $
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

#include "Figure/PolyFigure.hh"
#include <Warsaw/config.hh>
#include <Warsaw/Traversal.hh>
#include <Warsaw/IO.hh>
#include <Warsaw/PickTraversal.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

PolyFigure::PolyFigure()
  : _tx(new TransformImpl),
    _bbox(new RegionImpl)
{
}

PolyFigure::PolyFigure(const PolyFigure &pf)
  : _tx(new TransformImpl),
    _bbox(new RegionImpl)
{
  _bbox->valid = pf._bbox->valid;
  if (_bbox->valid) _bbox->copy(Region_var(pf._bbox->_this()));
}

PolyFigure::~PolyFigure()
{
}

void PolyFigure::update_bbox()
{
  if (!_bbox->valid)
    {
      CORBA::ULong n = num_children();
      if (n > 0)
	{
	  Allocation::Info info;
	  for (CORBA::ULong i = 0; i < n; i++)
	    _children[i].peer->extension(info, Region_var(_bbox->_this()));
	}
    }
}

void PolyFigure::allocate(Tag, const Allocation::Info &info)
{
  // undefine the allocation...how ? -Stefan
//   info.allocation->;
  info.transformation->premultiply(Transform_var(_tx->_this()));
}

void PolyFigure::request(Warsaw::Graphic::Requisition &r)
{
  GraphicImpl::init_requisition(r);
  Impl_var<RegionImpl> region(new RegionImpl);
  update_bbox();
  if (_bbox->valid)
    {
      region->copy(Region_var(_bbox->_this()));
      region->apply_transform(Transform_var(_tx->_this()));
      Coord x_lead = -region->lower.x, x_trail = region->upper.x;
      Coord y_lead = -region->lower.y, y_trail = region->upper.y;
      GraphicImpl::require_lead_trail(r.x, x_lead, x_lead, x_lead, x_trail, x_trail, x_trail);
      GraphicImpl::require_lead_trail(r.y, y_lead, y_lead, y_lead, y_trail, y_trail, y_trail);
    }
}

// If given transform is nil, PolyFigure::extension considers this a flag
// meaning "ok to calculate an imprecise extension" and thus bounding box
// can be used.
    
void PolyFigure::extension(const Allocation::Info &info, Region_ptr region)
{
  Impl_var<RegionImpl> tmp(new RegionImpl);
  update_bbox();
  if (_bbox->valid)
    {
      Impl_var<TransformImpl> transformation(new TransformImpl);
      if (!CORBA::is_nil(info.transformation)) transformation->copy(info.transformation);
      transformation->premultiply(Transform_var(_tx->_this()));
      tmp->copy(Region_var(_bbox->_this()));
      tmp->apply_transform(Transform_var(transformation->_this()));
      region->merge_union(Region_var(tmp->_this()));
    }
}

/*
 * FIXME !!!: we currently just add a figure specific cull test
 *            and then call PolyGraphic::traverse, we need to push
 *            the figure's trafo as well ! -stefan
 */
void PolyFigure::traverse(Traversal_ptr traversal)
{
  Trace trace("PolyFigure::traverse");
  update_bbox();
  if (!_bbox->valid) return;
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  region->copy(Region_var(_bbox->_this()));
  region->apply_transform(Transform_var(_tx->_this()));
  if (!traversal->intersects_region(Region_var(region->_this()))) return;
  CORBA::Long n = num_children();
  for (CORBA::Long i = 0; i != n && traversal->ok(); i++)
    {
      Graphic_var child = _children[i].peer;
      if (CORBA::is_nil(child)) continue;
      try { traversal->traverse_child(child, _children[i].localId, Region_var(_bbox->_this()), Transform_var(_tx->_this()));}
      catch (const CORBA::OBJECT_NOT_EXIST &) { _children [i].peer = Warsaw::Graphic::_nil ();}
      catch (const CORBA::COMM_FAILURE &) { _children [i].peer = Warsaw::Graphic::_nil ();}
    }

}

Transform_ptr PolyFigure::transformation()
{
  return _tx->_this();
}

void PolyFigure::need_redraw()
{
  GraphicImpl::need_redraw();
  // Calling GraphicImpl::needRedraw does not allow us to take
  // advantage of bbox for damage. However, to do damage with
  // bbox, we would need to grow the transformed bbox to compensate
  // for the brush size of leaves. (In truth, we should do the same for
  // the cull test as well since currently culling will prevent redraw
  // when the outer part of an outer leaf's brush is damaged.)
}

void PolyFigure::need_resize()
{
  _bbox->valid = false;
  PolyGraphic::need_resize();
}

UPolyFigure::UPolyFigure(const UPolyFigure &up) : PolyFigure(up) {}

/*
 * FIXME !!!: implement this according to Fresco's comments
 *            in figures.idl: FigureKit::ugroup()
 */
void UPolyFigure::traverse(Traversal_ptr traversal)
{
  Trace trace("UPolyFigure::traverse");  
}
