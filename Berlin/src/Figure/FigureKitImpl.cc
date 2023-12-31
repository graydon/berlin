/*$Id: FigureKitImpl.cc,v 1.10 1999/10/07 15:11:10 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include "Figure/FigureKitImpl.hh"
#include "Berlin/Allocator.hh"
#include "Figure/FigureImpl.hh"
#include "Figure/PolyFigure.hh"
#include "Figure/Figures.hh"
#include "Figure/ImageImpl.hh"
#include "Figure/TransformatorImpl.hh"
#include "Berlin/Plugin.hh"

using namespace Figures;

FigureKitImpl::FigureKitImpl() {}
FigureKitImpl::~FigureKitImpl() {}

Graphic_ptr FigureKitImpl::root(Graphic_ptr child)
{
  GraphicImpl *g = new TransformAllocator(Alignment(0.5), Alignment(0.5), Alignment(0.5), 
					  Alignment(0.5), Alignment(0.5), Alignment(0.5));
  g->_obj_is_ready(_boa());
  g->body(child);
  return g->_this();
}

Graphic_ptr FigureKitImpl::fitter(Graphic_ptr g)
{
  /* unimplemented */
  return Graphic::_duplicate(g);
}

Graphic_ptr FigureKitImpl::group()
{
  PolyFigure *pf = new PolyFigure;
  pf->_obj_is_ready(_boa());
  return pf->_this();
}

Graphic_ptr FigureKitImpl::ugroup()
{
  UPolyFigure *pf = new UPolyFigure;
  pf->_obj_is_ready(_boa());
  return pf->_this();
}

Point_ptr FigureKitImpl::point(Coord x, Coord y)
{
  Vertex v;
  v.x = x, v.y = y;
  PointImpl *pt = new PointImpl(v);
  pt->_obj_is_ready(_boa());
  return pt->_this();
}

Line_ptr FigureKitImpl::line(Coord x0, Coord y0, Coord x1, Coord y1)
{
  Vertex v1, v2;
  v1.x = x0, v1.y = y0;
  v2.x = x1, v2.y = y1;
  LineImpl *l = new LineImpl(v1, v2);
  l->_obj_is_ready(_boa());
  return l->_this();
}

Rectangle_ptr FigureKitImpl::rectangle(Coord l, Coord t, Coord r, Coord b)
{
  Vertex lower, upper;
  lower.x = l, lower.y = t;
  upper.x = r, upper.y = b;
  RectangleImpl *rect = new RectangleImpl(lower, upper);
  rect->_obj_is_ready(_boa());
  return rect->_this();
}

Circle_ptr FigureKitImpl::circle(Coord x, Coord y, Coord r)
{
  Vertex center;
  center.x = x, center.y = y;
  CircleImpl *c = new CircleImpl(center, r);
  c->_obj_is_ready(_boa());
  return c->_this();
}

Ellipse_ptr FigureKitImpl::ellipse(Coord x, Coord y, Coord r1, Coord r2)
{
  Vertex center;
  center.x = x, center.y = y;
  EllipseImpl *e = new EllipseImpl(center, r1, r2);
  e->_obj_is_ready(_boa());
  return e->_this();
}

Path_ptr FigureKitImpl::multiline(const Figure::Vertices &v)
{
  PathImpl *p = new PathImpl(v);
  p->_obj_is_ready(_boa());
  return p->_this();
}

Path_ptr FigureKitImpl::polygon(const Figure::Vertices &v)
{
  PathImpl *p = new PathImpl(v);
  p->_obj_is_ready(_boa());
  return p->_this();
}

Image_ptr FigureKitImpl::pixmap(Raster_ptr raster)
{
  ImageImpl *image = new ImageImpl(raster);
  image->_obj_is_ready(_boa());
//   figures.push_back(image);
  return image->_this();
}

Transformator_ptr FigureKitImpl::projection(Graphic_ptr g)
{
  TransformatorImpl *transformator = new TransformatorImpl;
  transformator->_obj_is_ready(_boa());
//   figures.push_back(image);
  transformator->body(g);
  return transformator->_this();
}

EXPORT_PLUGIN(FigureKitImpl, interface(FigureKit))
