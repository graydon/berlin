/*$Id: GLPixmapFont.cc,v 1.3 2000/04/16 06:39:20 graydon Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Drawing/openGL/GLPixmapFont.hh"
#include <GL/gl.h>
#include <algorithm>
#include <memory>

using namespace FT;

GLPixmapFont::GLPixmapFont(const FT::Face &f, size_t pt)
  : face(f), instance(face), pfont(0), cache(256), _size(pt), _weight(100)
{
  if(pt < 1) pt = 1;
  if(!instance.setResolutions(96,96)) throw exception();
  if(!instance.setPointSize(pt)) throw exception();
  pfont = new FT::PixmapFont(instance);
}

GLPixmapFont::~GLPixmapFont()
{
  for (vector<block>::iterator i = cache.begin(); i != cache.end(); i++)
    for (vector<FT::Pixmap *>::iterator j = (*i).begin(); j != (*i).end(); j++)
      delete *j;
  delete pfont;
}

void GLPixmapFont::drawText(const Unistring &us)
{
  GLfloat color[4];
  glGetFloatv(GL_CURRENT_COLOR, color);
  unsigned char r = (unsigned char)(color[0] * 255.);
  unsigned char g = (unsigned char)(color[1] * 255.);
  unsigned char b = (unsigned char)(color[2] * 255.);
  unsigned char a = (unsigned char)(color[3] * 255.);

//  glPixelStorei(GL_UNPACK_ALIGNMENT,1);

  GLint swapbytes, lsbfirst, rowlength;
  GLint skiprows, skippixels, alignment;

  // Save the current packing mode for bitmaps.
  glGetIntegerv(GL_UNPACK_SWAP_BYTES, &swapbytes);
  glGetIntegerv(GL_UNPACK_LSB_FIRST, &lsbfirst);
  glGetIntegerv(GL_UNPACK_ROW_LENGTH, &rowlength);
  glGetIntegerv(GL_UNPACK_SKIP_ROWS, &skiprows);
  glGetIntegerv(GL_UNPACK_SKIP_PIXELS, &skippixels);
  glGetIntegerv(GL_UNPACK_ALIGNMENT, &alignment);

  // Enforce a standard packing mode
  glPixelStorei(GL_UNPACK_SWAP_BYTES, GL_FALSE);
  glPixelStorei(GL_UNPACK_LSB_FIRST, GL_FALSE);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

  glPushAttrib(GL_ENABLE_BIT);
  glPushAttrib(GL_PIXEL_MODE_BIT);
  glPushAttrib(GL_COLOR_BUFFER_BIT); // for glEnable(GL_ALPHA_TEST)

  glPixelZoom(1.,1.);
  glPixelTransferf(GL_RED_SCALE,   1.);
  glPixelTransferf(GL_GREEN_SCALE, 1.);
  glPixelTransferf(GL_BLUE_SCALE,  1.);
  glPixelTransferf(GL_ALPHA_SCALE, 1.);
  glPixelTransferf(GL_RED_BIAS,    0.);
  glPixelTransferf(GL_GREEN_BIAS,  0.);
  glPixelTransferf(GL_BLUE_BIAS,   0.);
  glPixelTransferf(GL_ALPHA_BIAS,  0.);

  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GEQUAL, 0.1);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

//   glRasterPos2d(0., 160.);  // position pen
  int x = 0, y = 0;
  for (unsigned long idx = 0; idx < us.length(); idx++)
    {
      FT::Pixmap *pixmap = loadGlyph(*(us.get_buffer() + idx));
      if(!pixmap) continue;
      unsigned char *data = pixmap->data(r, g, b, a);
      if(data)
	{
	  int x_dst = (int)(10. * (x + pixmap->deltax()) / 64);
	  int y_dst = (int)(10. * (y + pixmap->deltay()) / 64);
	  if(x_dst < 0 || y_dst < 0)
	    {
	      int dummy = 0;
	      glRasterPos2i(0, 0);
	      glBitmap(0, 0, 0, 0, x_dst, y_dst, (const GLubyte*)&dummy);
	    }
	  else glRasterPos2i(x_dst, y_dst);
	  glPixelStorei(GL_UNPACK_ROW_LENGTH, pixmap->rowlength());
	  glDrawPixels(pixmap->width(), pixmap->height(), GL_RGBA, GL_UNSIGNED_BYTE, data);
	}
      x += pixmap->advance();
    }
  glPopAttrib();
  glPopAttrib();
  glPopAttrib();

  // Restore saved packing modes.
  glPixelStorei(GL_UNPACK_SWAP_BYTES, swapbytes);
  glPixelStorei(GL_UNPACK_LSB_FIRST, lsbfirst);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, rowlength);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, skiprows);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, skippixels);
  glPixelStorei(GL_UNPACK_ALIGNMENT, alignment);
}

void GLPixmapFont::allocateText(const Unistring &us, Graphic::Requisition &r)
{
  long width = 0;
  long height = 0;
  long bearing = 0;
  for(unsigned int idx = 0; idx < us.length(); idx++)
    {
      auto_ptr<Glyph> glyph(pfont->glyph(*(us.get_buffer() + idx)));
      Glyph::Metrics metrics = glyph->metrics();
      width += metrics.advance;
      height = max(height, metrics.bbox.yMax - metrics.bbox.yMin);
      bearing = max(bearing, metrics.bearingY);
    }
  r.x.defined = true;
  r.x.natural = r.x.minimum = r.x.maximum = 10*width/64;
  r.x.align = 0.;
  r.y.defined = true;
  r.y.natural = r.y.minimum = r.y.maximum = 10*height/64;
  r.y.align = static_cast<double>(bearing)/height;
}


FT::Pixmap *GLPixmapFont::loadGlyph(unsigned short uc) const
{
  block &b = cache[(uc >> 8) & 0xff];
  if (b.size() && b[uc & 0xff]) return b[uc & 0xff];
  if (!b.size()) b.resize(256, 0);
  if (!b[uc & 0xff]) b[uc & 0xff] = pfont->pixmap(uc);
  return b[uc & 0xff];
}
