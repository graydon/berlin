/*$Id: LibArtRaster.cc,v 1.3 2000/09/19 21:11:07 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 graydon hoare <graydon@pobox.com>
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

#include "Drawing/libArt/LibArtRaster.hh"

using namespace Warsaw;

LibArtRaster::LibArtRaster(Raster_var r) :
  remote(Raster::_duplicate(r)) 
{  
  Raster::Info info = remote->header();
  Raster::ColorSeq_var colors;
  Raster::Index lower, upper;
  lower.x = lower.y = 0;
  upper.x = info.width, upper.y = info.height;
  remote->store_pixels(lower, upper, colors);
  int width = (int)(info.width);
  int height = (int)(info.height);
  pixels = new art_u8[4*width*height];
  for (int y = height - 1; y >= 0; y--) {
    for (int x = 0; x != width; x++) {
      pixels[4*(y*width+x)] = (int)(colors[y * width + x].blue * 0xff);
      pixels[4*(y*width+x)+1] = (int)(colors[y * width + x].green * 0xff);
      pixels[4*(y*width+x)+2] = (int)(colors[y * width + x].red * 0xff);
      pixels[4*(y*width+x)+3] = (int)(colors[y * width + x].alpha * 0xff);
    }
  }
  pixbuf = art_pixbuf_new_const_rgba (pixels, width, height, width * 4);
}

LibArtRaster::~LibArtRaster()
{
  art_pixbuf_free (pixbuf);
  delete[] pixels;
}
