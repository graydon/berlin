/*$Id: GLBitmapFont.cc,v 1.1 1999/12/13 21:37:24 stefan Exp $
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
#include "Drawing/openGL/GLBitmapFont.hh"
#include <GL/gl.h>

GLBitmapFont::GLBitmapFont(const FT::Face &f, size_t pt)
  : face(f), instance(face), bfont(0), cache(256)
{
  if (pt < 1) pt = 1;
  if (!instance.setResolutions(96, 96)) throw exception();
  if (!instance.setPointSize(pt)) throw exception();
  bfont = new FT::BitmapFont(instance);
}

GLBitmapFont::~GLBitmapFont()
{
  for (vector<block>::iterator i = cache.begin(); i != cache.end(); i++)
    for (vector<FT::Bitmap *>::iterator j = (*i).begin(); j != (*i).end(); j++)
      delete *j;
  delete bfont;
}

void GLBitmapFont::render(int x, int y, const unsigned short *begin, const unsigned short *end) const
{
  x *= 64;
  y *= 64;

  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  for (const unsigned short *uc = begin; uc != end; uc++)
    {
      FT::Bitmap *bitmap = loadGlyph(*uc);
      if (!bitmap) continue;
      if (bitmap->data())
	{
	  int x_dst= (x + bitmap->deltax()) / 64;
	  int y_dst= (y + bitmap->deltay()) / 64;

      // Let's handle the case where the bitmap origin generates an
      // invalid raster pos error
      // Contributed by Dirk Reiners <reiners@ecrc.de>
	  if (x_dst < 0 || y_dst < 0)
	    {
	      int dummy = 0;
	      glRasterPos2i(0, 0);
	      glBitmap(0, 0, 0, 0, x_dst, y_dst, (const GLubyte*)&dummy);
	    }
	  else glRasterPos2i(x_dst, y_dst);
	  glBitmap(bitmap->width(), bitmap->height(), 0., 0., 0., 0., (const GLubyte*) bitmap->data());
	}
      x += bitmap->advance();
    }
}

FT::Bitmap *GLBitmapFont::loadGlyph(unsigned short uc) const
{
  block &b = cache[(uc >> 8) & 0xff];
  if (b.size() && b[uc & 0xff]) return b[uc & 0xff];
  if (!b.size()) b.resize(256, 0);
  if (!b[uc & 0xff]) b[uc & 0xff] = bfont->bitmap(uc);
  return b[uc & 0xff];
}
