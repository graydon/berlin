/*$Id: GLOutlineFont.cc,v 1.1 1999/12/14 15:38:38 stefan Exp $
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
#include "Drawing/openGL/GLOutlineFont.hh"
#include "Drawing/FT/Outline.hh"
#include <GL/gl.h>

int GLOutlineFont::loadGlyph(unsigned short uc) const
{
  block &b = cache[(uc >> 8) & 0xff];
  size_t g = uc & 0xff;
  if (b.glyphs.size() && b.glyphs[g] > -1) return true;
  if (!b.glyphs.size())
    {
      b.lists = glGenLists(256);
      if (!b.lists) return -1;
      b.glyphs.resize(256);
      for (size_t i = 0; i != 256; i++) b.glyphs[i] = -1;
    }
  FT::Glyph *glyph = tfont->glyph(uc);
  if (!glyph)
    {
      glNewList(b.lists + g, GL_COMPILE);
      glEndList();
      return b.lists + g;
    }
  else b.glyphs[g] = b.lists + g;
  FT::Outline outline(*glyph);
  outline.vectorize(precision);
  glNewList(b.lists + g, GL_COMPILE);

  for (size_t i = 0; i < outline.contours().size(); i++)
    {
      const FT::Outline::Contour &contour = outline.contours()[i];
      glBegin(GL_LINE_LOOP);
      for (size_t j = 0; j < contour.points.size(); j++)
	{
	  const FT::Outline::Point &p= contour.points[j];
	  glVertex2f(p.x, p.y);
	}
      glEnd();
    }
  glTranslatef(outline.advance(), 0., 0.);
  glEndList();
  return b.lists + g;
}

