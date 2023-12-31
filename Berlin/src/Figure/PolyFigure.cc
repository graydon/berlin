/*$Id: PolyFigure.cc,v 1.4 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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

#include "Warsaw/config.hh"
#include "Warsaw/Traversal.hh"
#include "Warsaw/PickTraversal.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Figure/PolyFigure.hh"
#include "Berlin/Logger.hh"

PolyFigure::PolyFigure()
  : tx(new TransformImpl),
    bbox(new RegionImpl)
{
}

PolyFigure::PolyFigure(const PolyFigure &pf)
  : tx(new TransformImpl),
    bbox(new RegionImpl)
{
  bbox->valid = pf.bbox->valid;
  if (bbox->valid) bbox->copy(pf.bbox);
}

PolyFigure::~PolyFigure()
{
}

void PolyFigure::updateBbox()
{
  if (!bbox->valid)
    {
      CORBA::ULong n = numChildren();
      if (n > 0)
	{
	  Allocation::Info info;
	  for (CORBA::ULong i = 0; i < n; i++)
	    children[i].first->extension(info, bbox);
	}
    }
}

void PolyFigure::allocate(Tag, const Allocation::Info &info)
{
  // undefine the allocation...how ? -Stefan
//   info.allocation->;
  info.transformation->premultiply(tx);
}

void PolyFigure::request(Requisition &r)
{
  GraphicImpl::initRequisition(r);
  Impl_var<RegionImpl> region(new RegionImpl);
  updateBbox();
  if (bbox->valid)
    {
      region->copy(bbox);
      region->applyTransform(tx);
      Coord x_lead = -region->lower.x, x_trail = region->upper.x;
      Coord y_lead = -region->lower.y, y_trail = region->upper.y;
      GraphicImpl::requireLeadTrail(r.x, x_lead, x_lead, x_lead, x_trail, x_trail, x_trail);
      GraphicImpl::requireLeadTrail(r.y, y_lead, y_lead, y_lead, y_trail, y_trail, y_trail);
    }
}

// If given transform is nil, PolyFigure::extension considers this a flag
// meaning "ok to calculate an imprecise extension" and thus bounding box
// can be used.
    
void PolyFigure::extension(const Allocation::Info &info, Region_ptr region)
{
  Impl_var<RegionImpl> tmp(new RegionImpl);
  updateBbox();
  if (bbox->valid)
    {
      Impl_var<TransformImpl> transformation(new TransformImpl);
      if (!CORBA::is_nil(info.transformation)) transformation->copy(info.transformation);
      transformation->premultiply(tx);
      tmp->copy(bbox);
      tmp->applyTransform(transformation);
      region->mergeUnion(tmp);
    }
}

/*
 * FIXME !!!: we currently just add a figure specific cull test
 *            and then call PolyGraphic::traverse, we need to push
 *            the figure's trafo as well ! -stefan
 */
void PolyFigure::traverse(Traversal_ptr traversal)
{
  SectionLog section("PolyFigure::traverse");
  updateBbox();
  if (bbox->valid)
    {
      Impl_var<RegionImpl> region(new RegionImpl(bbox, tx));
      if (!traversal->intersectsRegion(Region_var(region->_this()))) return;
    }
  else return;
  CORBA::Long n = numChildren();
  for (CORBA::Long i = 0; i != n; i++)
    {
      traversal->traverseChild(children[i].first, children[i].second, Region_var(bbox->_this()), Transform_var(tx->_this()));
      if (!traversal->ok()) break;
    }

}

Transform_ptr PolyFigure::transformation()
{
  return tx->_this();
}

void PolyFigure::needRedraw()
{
  GraphicImpl::needRedraw();
  // Calling GraphicImpl::needRedraw does not allow us to take
  // advantage of bbox for damage. However, to do damage with
  // bbox, we would need to grow the transformed bbox to compensate
  // for the brush size of leaves. (In truth, we should do the same for
  // the cull test as well since currently culling will prevent redraw
  // when the outer part of an outer leaf's brush is damaged.)
}

void PolyFigure::needResize()
{
  bbox->valid = false;
  PolyGraphic::needResize();
}

UPolyFigure::UPolyFigure(const UPolyFigure &up) : PolyFigure(up) {}

/*
 * FIXME !!!: implement this according to Fresco's comments
 *            in figures.idl: FigureKit::ugroup()
 */
void UPolyFigure::traverse(Traversal_ptr traversal)
{
  SectionLog section("UPolyFigure::traverse");  
}
