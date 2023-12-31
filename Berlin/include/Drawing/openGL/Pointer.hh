/*$Id: Pointer.hh,v 1.5 1999/05/13 20:37:26 gray Exp $
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
#ifndef _Pointer_hh
#define _Pointer_hh

#include <Drawing/openGL/GLDrawable.hh>

/*
 * a little pointer...
 * should be extended when a first ImageKit is available which knows how to rasterize graphics
 * and how to transform this into ggi's internal format.
 */
class Pointer
{
public:
  Pointer(ggi_visual_t);
  ~Pointer();
  void move(PixelCoord, PixelCoord);
  void draw();
  void backup();
  void restore();
  bool intersects(const PixelCoord &, const PixelCoord &, const PixelCoord &, const PixelCoord &);
private:
  PixelCoord              origin[2];
  PixelCoord              position[2];
  PixelCoord              size[2];
  const ggi_directbuffer *dbuf;
  int                     depth;
  int                     stride;
  int                     maxCoord;
  unsigned char	         *image;
  unsigned char	         *mask;
  unsigned char          *cache;
};

inline bool Pointer::intersects(const PixelCoord &l, const PixelCoord &r, const PixelCoord &t, const PixelCoord &b)
{
  return l <= position[0] + size[0] && r >= position[0] && t <= position[1] + size[1] && b >= position[1];
}

#endif /* _Pointer_hh */
