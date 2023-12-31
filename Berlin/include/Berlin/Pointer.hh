/*$Id: Pointer.hh,v 1.7 2000/08/31 18:52:31 stefan Exp $
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
#ifndef _Pointer_hh
#define _Pointer_hh

#include <Berlin/Console.hh>

/*
 * a little pointer...
 * should be extended when a first ImageKit is available which knows how to rasterize graphics
 * and how to transform this into ggi's internal format.
 */
class Pointer
{
public:
  Pointer(Console::Drawable *);
  ~Pointer();
  void move(Warsaw::Coord, Warsaw::Coord);
  void draw();
  void save();
  void restore();
  bool intersects(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
private:
  Console::Drawable  *screen;
  Warsaw::PixelCoord  origin[2];
  Warsaw::PixelCoord  position[2];
  Warsaw::PixelCoord  size[2];
  Warsaw::Coord       scale[2];
  unsigned char      *image;
  unsigned char      *mask;
  unsigned char      *cache;
};

inline bool Pointer::intersects(Warsaw::Coord l, Warsaw::Coord r, Warsaw::Coord t, Warsaw::Coord b)
{
  return
    l/scale[0] <= position[0] + size[0] &&
    r/scale[0] >= position[0] &&
    t/scale[1] <= position[1] + size[1] &&
    b/scale[1] >= position[1];
}

#endif /* _Pointer_hh */
