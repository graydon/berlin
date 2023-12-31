/*$Id: Bitmap.cc,v 1.1 1999/12/13 21:14:34 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
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
#include "Drawing/FT/Bitmap.hh"
#include <cstring>

using namespace FT;

template <class T> T inline floor(T t) { return t & -64;}
template <class T> T inline ceiling(T t) { return (t + 63) & -64;}

Bitmap::Bitmap(const Glyph &glyph) throw (exception)
  : w(0), h(0), cols(0), b(0), a(0), dx(0), dy(0)
{
  Glyph::Metrics metrics = glyph.metrics();
  TT_BBox &bbox= metrics.bbox; // glyph bounding box
  a = metrics.advance;
  dx = bbox.xMin;
  dy = bbox.yMin;

  bbox.xMin = floor(bbox.xMin);
  bbox.yMin = floor(bbox.yMin);
  bbox.xMax = ceiling(bbox.xMax);
  bbox.yMax = ceiling(bbox.yMax);

  w = (bbox.xMax - bbox.xMin)/64;
  h = (bbox.yMax - bbox.yMin)/64;

  cols= (w + 7) / 8;
  int size = cols * h;

  if(size <= 0) return;

  b = new unsigned char [size];
  memset(b, 0, size);

  TT_Raster_Map bitmap;

  bitmap.width = w;
  bitmap.cols  = cols;
  bitmap.rows  = h;
  bitmap.flow  = TT_Flow_Up;
  bitmap.size  = size;
  bitmap.bitmap= (void*) b;

  if (TT_Get_Glyph_Bitmap(glyph, &bitmap, -bbox.xMin, -bbox.yMin)) throw exception();
}

Bitmap::~Bitmap()
{
  delete[] b;
}
