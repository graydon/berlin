/*$Id: FigureKitImpl.cc,v 1.19 2001/04/18 06:07:27 stefan Exp $
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

#include <Warsaw/config.hh>
#include <Berlin/Allocator.hh>
#include "Figure/FigureKitImpl.hh"
#include "Figure/FigureImpl.hh"
#include "Figure/PolyFigure.hh"
#include "Figure/Figures.hh"
#include "Figure/ImageImpl.hh"
#include "Figure/Transformer.hh"

using namespace Warsaw;

FigureKitImpl::FigureKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
FigureKitImpl::~FigureKitImpl() {}
Graphic_ptr FigureKitImpl::root(Graphic_ptr child)
{
  GraphicImpl *g = new TransformAllocator(Alignment(0.5), Alignment(0.5), Alignment(0.5), 
					  Alignment(0.5), Alignment(0.5), Alignment(0.5));
  activate(g);
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
  activate(pf);
  return pf->_this();
}

Graphic_ptr FigureKitImpl::ugroup()
{
  UPolyFigure *pf = new UPolyFigure;
  activate(pf);
  return pf->_this();
}

Figure::Point_ptr FigureKitImpl::point(Coord x, Coord y)
{
  Vertex v;
  v.x = x, v.y = y;
  PointImpl *pt = new PointImpl(v);
  activate(pt);
  return pt->_this();
}

Figure::Line_ptr FigureKitImpl::line(Coord x0, Coord y0, Coord x1, Coord y1)
{
  Vertex v1, v2;
  v1.x = x0, v1.y = y0;
  v2.x = x1, v2.y = y1;
  LineImpl *l = new LineImpl(v1, v2);
  activate(l);
  return l->_this();
}

Figure::Rectangle_ptr FigureKitImpl::rectangle(Coord l, Coord t, Coord r, Coord b)
{
  Vertex lower, upper;
  lower.x = l, lower.y = t;
  upper.x = r, upper.y = b;
  RectangleImpl *rect = new RectangleImpl(lower, upper);
  activate(rect);
  return rect->_this();
}

Figure::Circle_ptr FigureKitImpl::circle(Coord x, Coord y, Coord r)
{
  Vertex center;
  center.x = x, center.y = y;
  CircleImpl *c = new CircleImpl(center, r);
  activate(c);
  return c->_this();
}

Figure::Ellipse_ptr FigureKitImpl::ellipse(Coord x, Coord y, Coord r1, Coord r2)
{
  Vertex center;
  center.x = x, center.y = y;
  EllipseImpl *e = new EllipseImpl(center, r1, r2);
  activate(e);
  return e->_this();
}

Figure::Path_ptr FigureKitImpl::multiline(const Warsaw::Path &p)
{
  PathImpl *path = new PathImpl(p, false);
  activate(path);
  return path->_this();
}

Figure::Path_ptr FigureKitImpl::polygon(const Warsaw::Path &p)
{
  PathImpl *path = new PathImpl(p, true);
  activate(path);
  return path->_this();
}

Image_ptr FigureKitImpl::pixmap(Raster_ptr raster)
{
  ImageImpl *image = new ImageImpl(raster);
  activate(image);
  return image->_this();
}

Graphic_ptr FigureKitImpl::texture(Graphic_ptr g, Raster_ptr raster)
{
  Texture *t = new Texture(raster);
  activate(t);
  t->body(g);
  return t->_this();
}

Graphic_ptr FigureKitImpl::transformer(Graphic_ptr g)
{
  Transformer *transformer = new Transformer;
  activate(transformer);
  transformer->body(g);
  return transformer->_this();
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "FigureKitImpl"};
  return new KitFactoryImpl<FigureKitImpl> ("IDL:Warsaw/FigureKit:1.0", properties, 1);
}
