/*$Id: GGIDrawingKit.cc,v 1.4 2000/02/29 22:08:21 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include "Warsaw/Types.hh"
#include "Warsaw/Transform.hh"
#include "Drawing/GGI/GGIDrawingKit.hh"

extern "C" {
#include "ggi/ggi2d.h"
}

 // #include "Warsaw/Text.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"
#include "Berlin/Plugin.hh"
#include <strstream>
#include <iostream>

// these are _very_ coarse conversions to device space for GGI. please make them
// less coarse if you are smart. -graydon

static inline ggi_sint dev(Coord x) {
  return static_cast<ggi_sint>(x);
}

static inline ggi_pixel col(Color c1, ggi_visual_t vis) {
  ggi_color c2;
  // GGI _appears_ to use 16 bit color + alpha throughout. *sigh*
  static double scale = 0xffff;
  c2.r = static_cast<uint16>(c1.red * scale);
  c2.g = static_cast<uint16>(c1.green * scale);
  c2.b = static_cast<uint16>(c1.blue * scale);
  c2.a = static_cast<uint16>(c1.alpha * scale);
  return ggiMapColor(vis,&c2);
}

static inline ggi_alpha alpha(Color c1) {
  static double scale = 0xffff;
  return static_cast<ggi_alpha>(c1.alpha * scale);
}


GGIDrawingKit::GGIDrawingKit(KitFactory *f, const PropertytSeq &p)
  : KitImpl(f, p),
    drawable(GGI::drawable()),
    tr(new TransformImpl),
    unifont(drawable->visual()),
    rasters(500)
{
  tr->_obj_is_ready(_boa());
}

GGIDrawingKit::~GGIDrawingKit()
{
  tr->_dispose();
}

void GGIDrawingKit::setTransformation(Transform_ptr t)
{
  tr->copy(t);
}

void GGIDrawingKit::setClipping(Region_ptr r)
{
  Impl_var<RegionImpl> clip(new RegionImpl(r, Transform_var(Transform::_nil())));
  ggi_sint x1 = dev(clip->lower.x + 0.5);
  ggi_sint y1 = dev(clip->lower.y + 0.5);
  ggi_sint x2 = dev(clip->upper.x + 0.5);
  ggi_sint y2 = dev(clip->upper.y + 0.5);
  ggi2dSetClip(drawable->visual(), x1, y1, x2, y2);  
}

void GGIDrawingKit::setForeground(Color c)
{
  fg = c;
  // is this implemented in ggi2d at the moment?
  // ggiSetAlpha(drawable->visual(), alpha(c));
  ggi2dSetDrawColor(drawable->visual(), col(c,drawable->visual()));
}

void GGIDrawingKit::setPointSize(Coord s)
{
  ps = s;
}

void GGIDrawingKit::setLineWidth(Coord w)
{
  lw = w;
}

void GGIDrawingKit::setLineEndstyle(DrawingKit::Endstyle style)
{
  es = style;
}

void GGIDrawingKit::setSurfaceFillstyle(DrawingKit::Fillstyle style)
{
  fs = style;
}

void GGIDrawingKit::setTexture(Raster_ptr t)
{
  tx = t;
  //  GGIRaster *glraster = rasters.lookup(Raster::_duplicate(t));
}

Text::Font_ptr GGIDrawingKit::findFont(const Text::FontDescriptor &f)
{
  return unifont._this();
}

Text::Font_ptr GGIDrawingKit::font()
{
  return unifont._this();
}


// void GGIDrawingKit::clear(Coord l, Coord t, Coord r, Coord b)
// {
// }

void GGIDrawingKit::drawPath(const Path &path)
{
  ggi_visual_t vis = drawable->visual();
  unsigned long l = path.length();
  ggi2d_coord coords[l];
  for (unsigned int i = 0; i < l; i++) {
    coords[i].x = dev(path[i].x);
    coords[i].y = dev(path[i].y);
  }
  if (fs == solid || (fs == textured && CORBA::is_nil(tx)))
    {
      ggi2dFillPoly(vis, coords, static_cast<ggi_uint>(l));
    }
  else if (fs == textured)
    {
      cerr << "sorry, implementation for textured polygons not finished..." << endl;
      ggi2dFillPoly(vis, coords, static_cast<ggi_uint>(l));
    }
  else
    {
      ggi2dDrawPoly(vis, coords, static_cast<ggi_uint>(l));
    }
}

void GGIDrawingKit::drawRect(const Vertex &lower, const Vertex &upper)
{
  ggi_visual_t vis = drawable->visual();
  if (fs == solid || (fs == textured && CORBA::is_nil(tx)))
    {
      ggi2dFillRect(vis,dev(lower.x), dev(lower.y), dev(upper.x), dev(upper.y));
    }
  else if (fs == textured)
    {
      cerr << "sorry, implementation for textured rects not finished..." << endl;
      ggi2dFillRect(vis,dev(lower.x), dev(lower.y), dev(upper.x), dev(upper.y));
    }
  else
    {
      ggi2dDrawRect(vis,dev(lower.x), dev(lower.y), dev(upper.x), dev(upper.y));
    }
}

void GGIDrawingKit::drawEllipse(const Vertex &lower, const Vertex &upper)
{
  // !!!FIXME!!! quadrics code here
}


void GGIDrawingKit::drawImage(Raster_ptr raster)
{
//   GGIRaster *ggiraster = rasters.lookup(Raster::_duplicate(raster));
//   ggiraster->draw();
}

void GGIDrawingKit::drawText(const Unistring &us)
{

}

extern "C" KitFactory *load()
{
  static string properties[] = {"implementation", "GGIDrawingKit"};
  return new KitFactoryImpl<GGIDrawingKit> (interface(DrawingKit), properties, 1);
}
