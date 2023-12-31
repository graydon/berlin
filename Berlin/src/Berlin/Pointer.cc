/*$Id: Pointer.cc,v 1.8 2001/04/18 06:07:26 stefan Exp $
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
#include <Prague/Sys/Memory.hh>
#include <Prague/Sys/Tracer.hh>
#include "Berlin/Pointer.hh"
#include <iostream>
#include <algorithm>

using namespace Prague;
using namespace Warsaw;

static unsigned char pointerImg[256] = 

{ 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,2,2,1,0,0,0,0,0,0,0,0,0,0,0,0,
  1,2,2,2,1,0,0,0,0,0,0,0,0,0,0,0,
  1,2,2,2,2,1,0,0,0,0,0,0,0,0,0,0,
  1,2,2,2,2,2,1,0,0,0,0,0,0,0,0,0,
  1,2,2,2,2,2,2,1,0,0,0,0,0,0,0,0,
  1,2,2,2,2,2,2,2,1,0,0,0,0,0,0,0,
  1,2,2,2,2,2,1,1,1,1,0,0,0,0,0,0,
  1,2,2,1,2,2,1,0,0,0,0,0,0,0,0,0,
  1,1,0,1,2,2,2,1,0,0,0,0,0,0,0,0,
  1,0,0,0,1,2,2,1,0,0,0,0,0,0,0,0,
  0,0,0,0,1,2,2,2,1,0,0,0,0,0,0,0,
  0,0,0,0,0,1,2,2,1,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0 };


Pointer::Pointer(Console::Drawable *d)
  : screen(d)
{
  origin[0] = origin[1] = 0;
  position[0] = position[1] = 8;
  size[0] = size[1] = 16;
  scale[0] = 1/screen->resolution(xaxis);
  scale[1] = 1/screen->resolution(yaxis);
  
  Console::Drawable::PixelFormat format = screen->pixel_format();

  Console::Drawable::Pixel trans = 0;
  Console::Drawable::Pixel red = (static_cast<Console::Drawable::Pixel>(1. * (~0L)) >> format.red_shift) & format.red_mask;
  Console::Drawable::Pixel green = (static_cast<Console::Drawable::Pixel>(1. * (~0L)) >> format.green_shift) & format.green_mask;
  Console::Drawable::Pixel blue = (static_cast<Console::Drawable::Pixel>(1. * (~0L)) >> format.blue_shift) & format.blue_mask;
  Console::Drawable::Pixel black =  0;
  Console::Drawable::Pixel white = red | green | blue;

  /*
   * create the pointer image
   */
  PixelCoord depth =  format.size >> 3;
  image = new unsigned char[size[0]*size[1] * depth];
  for (unsigned short y = 0; y != size[1]; y++)
    for (unsigned short x = 0; x != size[0]; x++)
      {
	Console::Drawable::Pixel color = pointerImg[y*size[0] + x] == 1 ? white : black;
 	for (unsigned short d = 0; d != depth; d++)
	  image[y*depth*size[0] + depth*x + d] = (color >> d) & 0xff;
      }
  /*
   * create the pointer mask
   */
  mask = new unsigned char[size[0]*size[1]*depth];
  for (unsigned short y = 0; y != size[1]; y++)
    for (unsigned short x = 0; x != size[0]; x++)
      {
	char flag = pointerImg[y*size[0] + x] == 0 ? 0 : ~0;
	for (unsigned short d = 0; d != depth; d++)
	  mask[y*depth*size[0] + depth*x + d] = flag;
      }
  cache = new unsigned char[size[0]*size[1]*depth];
  save();
}

Pointer::~Pointer()
{
  delete [] image;
  delete [] cache;
}

void Pointer::move(Coord x, Coord y)
{
  restore();
  position[0] = static_cast<PixelCoord>(std::max(static_cast<PixelCoord>(x/scale[0]), origin[0]));
  position[1] = static_cast<PixelCoord>(std::max(static_cast<PixelCoord>(y/scale[1]), origin[1]));
  save();
  draw();
};

void Pointer::save()
{
  Trace trace("Pointer::save");
  PixelCoord x = position[0] - origin[0];
  PixelCoord y = position[1] - origin[1];
  PixelCoord w = size[0];
  PixelCoord h = size[1];
  PixelCoord r = screen->row_length();
  PixelCoord s = screen->vwidth() * screen->vheight();
  PixelCoord d = screen->pixel_format().size >> 3;
  unsigned char *from = static_cast<unsigned char *>(screen->read_buffer()) + y * r + x * d;
  unsigned char *to = cache;
  for (PixelCoord o = 0; o != h && (y + o) * r / d + x + w < s; o++, from += r, to += d * w)
    Memory::copy(from, to, d * w);
}

void Pointer::restore()
{
  Trace trace("Pointer::restore");
  PixelCoord x = position[0] - origin[0];
  PixelCoord y = position[1] - origin[1];
  PixelCoord w = size[0];
  PixelCoord h = size[1];
  PixelCoord r = screen->row_length();
  PixelCoord s = screen->vwidth() * screen->vheight();
  PixelCoord d = screen->pixel_format().size >> 3;
  unsigned char *from = cache;
  unsigned char *to = static_cast<unsigned char *>(screen->write_buffer()) + y * r + x * d;
  for (PixelCoord o = 0; o != h && (y + o) * r / d + x + w < s; o++, from += d * w, to += r)
    Memory::copy(from, to, d * w);
}

void Pointer::draw()
{
  Trace trace("Pointer::draw");
  PixelCoord x = position[0] - origin[0];
  PixelCoord y = position[1] - origin[1];
  PixelCoord w = size[0];
  PixelCoord h = size[1];
  PixelCoord r = screen->row_length();
  PixelCoord s = screen->vwidth() * screen->vheight();
  PixelCoord d = screen->pixel_format().size >> 3;
  unsigned char *from = image;
  unsigned char *bits = mask;
  unsigned char *to = static_cast<unsigned char *>(screen->write_buffer()) + y * r + x * d; 
  for (PixelCoord i = 0; i != h && (y + i) * r / d + x + w < s; i++, to += r - w * d)
    for (PixelCoord j = 0; j != w * d; j++, from++, bits++, to++)
      *to = (*from & *bits) | (*to & ~*bits);
  screen->flush();
}
