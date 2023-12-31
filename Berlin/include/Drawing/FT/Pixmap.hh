/*$Id: Pixmap.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
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
#ifndef _FT_Pixmap_hh
#define _FT_Pixmap_hh

#include <Drawing/FT/Glyph.hh>

namespace FT
{

class Pixmap
{
public:
  Pixmap(const Glyph &) throw (exception);
  ~Pixmap();
  int rowlength() const { return cols;}
  int width() const { return w;}
  int height() const { return h;}
  unsigned char *data(unsigned char r, unsigned char g, unsigned char b, unsigned char a);
  // 26.6
  int deltax() const { return dx;}
  // 26.6
  int deltay() const { return dy;}
  // 26.6
  int advance() const { return a;}
private:
  int w, h;
  int cols;
  unsigned char *bitmap;
  unsigned char *pixmap;
  struct Color
  {
    Color() : r(0), g(0), b(0), a(255) {}
    unsigned char r, g, b, a;
  } color;
  int a; // 26.6
  int dx, dy; // 26.6
};

};

#endif /* _FT_Pixmap_hh */
