/*$Id: GLVectorFont.cc,v 1.2 1999/12/22 16:53:39 stefan Exp $
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
#include "Drawing/openGL/GLVectorFont.hh"
#include "Drawing/openGL/GLTessellator.hh"
#include <GL/gl.h>
#include <GL/glu.h>

GLVectorFont::GLVectorFont(const FT::Face &f, size_t pt)
  : face(f), instance(face), tfont(0), precision(4.), cache(256)
{
  if (pt < 1) pt = 1;
  int resolution = 96;
  if (!instance.setResolutions(resolution, resolution)) throw exception();
  if (!instance.setPointSize(pt)) throw exception();

  tfont = new FT::Font(instance);
}

GLVectorFont::~GLVectorFont()
{
  for (vector<block>::iterator i = cache.begin(); i != cache.end(); i++)
    if ((*i).lists) glDeleteLists((*i).lists, 256);
  delete tfont;
}

void GLVectorFont::render(const unsigned short *begin, const unsigned short *end) const
{
  glPushMatrix();
  for(const unsigned short *uc = begin; uc != end; uc++)
    {
      loadGlyph(*uc);
      glCallList(cache[(*uc >> 8) & 0xff].lists + (*uc & 0xff));
    }
  glPopMatrix();
}

int GLVectorFont::loadGlyph(unsigned short uc) const
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
  GLTessellator tessellator(*glyph);

  glNewList(b.lists + g, GL_COMPILE);
  tessellator.tessellate(precision);
  glTranslatef(tessellator.advance(), 0., 0.);
  glEndList();
  return b.lists + g;
}

