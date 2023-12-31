/*$Id: PSDrawingKit.cc,v 1.3 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include "Drawing/PostScript/PSDrawingKit.hh"
#include <Warsaw/Transform.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/Raster.hh>
#include <strstream>
#include <iostream>

using namespace Prague;
using namespace Warsaw;

PSDrawingKit::PSDrawingKit(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p)
{
}

PSDrawingKit::~PSDrawingKit()
{
}

void PSDrawingKit::set_transformation(Transform_ptr t)
{
}

void PSDrawingKit::set_clipping(Region_ptr r)
{
}

void PSDrawingKit::set_foreground(const Color &c)
{
}

void PSDrawingKit::set_lighting(const Color &c)
{
}

void PSDrawingKit::set_point_size(Coord s)
{
}

void PSDrawingKit::set_line_width(Coord w)
{
}

void PSDrawingKit::set_line_endstyle(Warsaw::DrawingKit::Endstyle style)
{
}

void PSDrawingKit::set_surface_fillstyle(Warsaw::DrawingKit::Fillstyle style)
{
}

void PSDrawingKit::set_texture(Raster_ptr t)
{
}

void PSDrawingKit::draw_path(const Path &path)
{
}

void PSDrawingKit::draw_rectangle(const Vertex &lower, const Vertex &upper)
{
}

void PSDrawingKit::draw_quadric(const Warsaw::DrawingKit::Quadric, Warsaw::Coord, Warsaw::Coord)
{
}

void PSDrawingKit::draw_ellipse(const Vertex &lower, const Vertex &upper)
{
}

void PSDrawingKit::draw_image(Raster_ptr raster)
{
}

void PSDrawingKit::set_font_size(CORBA::ULong s) {}
void PSDrawingKit::set_font_weight(CORBA::ULong w) {}
void PSDrawingKit::set_font_family(const Unistring &f) {}
void PSDrawingKit::set_font_subfamily(const Unistring &sf) {}
void PSDrawingKit::set_font_fullname(const Unistring &fn) {}
void PSDrawingKit::set_font_style(const Unistring &s) {}
void PSDrawingKit::set_font_attribute(const NVPair & nvp) {}
void PSDrawingKit::allocate_text(const Unistring &s, Graphic::Requisition &req) {}
void PSDrawingKit::draw_text(const Unistring &us) {}
void PSDrawingKit::allocate_char(Unichar c, Graphic::Requisition &req) {}
void PSDrawingKit::draw_char(Unichar c) {}
void PSDrawingKit::copy_drawable(Drawable_ptr d, PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h) {}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "PSDrawingKit"};
  return new KitFactoryImpl<PSDrawingKit> ("IDL:Warsaw/DrawingKit:1.0", properties, 1);
}
