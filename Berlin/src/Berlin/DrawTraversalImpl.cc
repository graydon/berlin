/*$Id: DrawTraversalImpl.cc,v 1.40 2001/02/06 22:02:21 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/IO.hh>
#include "Berlin/DrawTraversalImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Provider.hh"
#include "Berlin/Console.hh"

using namespace Prague;
using namespace Warsaw;

DrawTraversalImpl::DrawTraversalImpl(Graphic_ptr g, Region_ptr r, Transform_ptr t, DrawingKit_ptr kit)
  : TraversalImpl(g, r, t),
    _drawing(DrawingKit::_duplicate(kit)),
    _clipping(Region::_duplicate(r)),
    _id(new TransformImpl)
{
  Trace trace("DrawTraversalImpl::DrawTraversalImpl");
}

void DrawTraversalImpl::init()
{
  Trace trace("DrawTraversalImpl::init");
  __this = _this();
  /*
   * initialize the different drawing kit attributes
   */
  _drawing->clipping(_clipping);
  Color fg = {0., 0., 0., 1.};
  _drawing->foreground(fg);
  Color white = {1., 1., 1., 1.};
  _drawing->lighting(white);
  _drawing->transformation(Transform_var(_id->_this()));
  _drawing->surface_fillstyle(DrawingKit::solid);
  _drawing->save();
  Vertex l, u;
  _clipping->bounds(l, u);
  /*
   * clear the background of the damaged region...
   */
  _drawing->draw_rectangle(l, u);
#if 0
  _drawing->flush();
  Console::drawable()->flush();
  sleep(1);
#endif
}

void DrawTraversalImpl::finish()
{
  Trace trace("DrawTraversalImpl::finish");
  _drawing->restore();
}

DrawTraversalImpl::DrawTraversalImpl(const DrawTraversalImpl &traversal)
  : TraversalImpl(traversal),
    _drawing(traversal._drawing),
    _clipping(traversal._clipping)
{
//   drawing->clipping(clipping);
}

DrawTraversalImpl::~DrawTraversalImpl()
{
  _drawing->restore();
}

CORBA::Boolean DrawTraversalImpl::intersects_allocation()
{
  Region_var r = current_allocation();
  Transform_var t = current_transformation();
  RegionImpl region(r, t);
  return region.intersects(_clipping);
}

CORBA::Boolean DrawTraversalImpl::intersects_region(Region_ptr r)
{
  Transform_var t = current_transformation();
  RegionImpl region(r, t);
//   RegionImpl cl(clipping);
//   cout << "DrawTraversalImpl::intersectsRegion " << region << ' ' << clipping << endl;
  return region.intersects(_clipping);
}

void DrawTraversalImpl::traverse_child(Graphic_ptr child, Tag tag, Region_ptr region, Transform_ptr transform)
{
  Trace trace("DrawTraversalImpl::traverse_child");
  if (CORBA::is_nil(region)) region = Region_var(current_allocation());
  Lease_var<TransformImpl> cumulative(Provider<TransformImpl>::provide());
  *cumulative = *get_transformation(size() - 1);
  if (!CORBA::is_nil(transform)) cumulative->premultiply(transform);
  _drawing->transformation(Transform_var(cumulative->_this()));
  //   drawable->clipping(region, Transform_var(tx->_this()));
  push(child, tag, region, cumulative);
  try { child->traverse(__this);}
  catch (...) { pop(); throw;}
  pop(); 
};

void DrawTraversalImpl::visit(Graphic_ptr g) { g->draw(__this);}
Warsaw::Traversal::order DrawTraversalImpl::direction() { return Warsaw::Traversal::up;}
CORBA::Boolean DrawTraversalImpl::ok() { return true;}
DrawingKit_ptr DrawTraversalImpl::drawing() { return DrawingKit::_duplicate(_drawing);}
