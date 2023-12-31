/*$Id: GLUnifont.cc,v 1.30 2001/01/26 17:10:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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

#include <Prague/Sys/MMap.hh>
#include <Prague/Sys/Path.hh>
#include <Berlin/RCManager.hh>
#include "Drawing/openGL/GLUnifont.hh"

#include <GL/gl.h>
#include <string>
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cerrno>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

using namespace Prague;
using namespace Warsaw;

GLUnifont::GLUnifont()
  : _family(Unicode::to_CORBA(Babylon::String("GNU Unifont"))),
    _subfamily(),
    _fullname(),
    _style(Unicode::to_CORBA(Babylon::String("monospace")))
{
  Prague::Path path = RCManager::get_path("unifontpath");
  string glyphDB = path.lookup_file("glyph.dat");
  glyphmap = new MMap(glyphDB, -1, MMap::read, MMap::shared, 0, 0);
}

GLUnifont::~GLUnifont() { delete glyphmap ;}
CORBA::ULong GLUnifont::size() { return 16;}
CORBA::ULong GLUnifont::weight() { return 100;}
Unistring *GLUnifont::family() { return new Unistring(_family);}
Unistring *GLUnifont::subfamily() { return new Unistring(_subfamily);}
Unistring *GLUnifont::fullname() { return new Unistring(_fullname);}
Unistring *GLUnifont::style() { return new Unistring(_style);}
DrawingKit::FontMetrics GLUnifont::metrics()
{
  DrawingKit::FontMetrics fm;
  fm.ascender = 16 << 6;
  fm.descender = 0;
  fm.height = 16 << 6;
  fm.max_advance = 16 << 6;
  return fm;
}

DrawingKit::GlyphMetrics GLUnifont::metrics(Unichar uc)
{
  DrawingKit::GlyphMetrics gm;
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  
  unsigned int stride = 33;
  unsigned int base = stride * uc;
  bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
  int width = is_halfwidth ? 8 : 16; 
  int height = 16;
    
  gm.width = width << 6;
  gm.height = height << 6;
  gm.horiBearingX = 0;
  gm.horiBearingY = 0;
  gm.horiAdvance =  width << 6;
  gm.vertBearingX = 0;
  gm.vertBearingY = 0;
  gm.vertAdvance = height << 6;
  return gm;
}

void GLUnifont::Texture::bind(unsigned char *glyphs, GLubyte block)
{
  glGenTextures(1, &name);
  pos = new GLuint[rows * (columns + 1)];
//   data = new GLubyte[65536];
  data = new GLubyte[3*65536];

  glBindTexture(GL_TEXTURE_2D, name);
  unsigned int stride = 33;
  for (int yrow = 0; yrow < Texture::rows; ++yrow)
    {
      unsigned int totalWidth = 0;
      for (int i = 0; i < columns; ++i)
	{
	  unsigned int base = stride * (block + (yrow * rows) + i);
	  bool is_halfwidth = (glyphs[base] == (unsigned char)0xff) ? 0 : 1;
	  unsigned char width = is_halfwidth ? 8 : 16; 
	  unsigned char bytes = is_halfwidth ? 1 : 2;
	  base++; // advance past width marker
	  // expand to a form that openGL can handle:
	  for (int ypos = 0; ypos < 16; ++ypos)
	    for (int j = 0; j < bytes; ++j)
	      {
		GLubyte pos = glyphs[base + (ypos*bytes) + j];
		for (int k = 0; k < 8; ++k)
		  {
		    int index = totalWidth + k + (j * 8) + (15 - ypos + (yrow * 16)) * 1024;
		    data[3*index] = (pos & (128 >> k)) ? 0 : 0xff;
		    data[3*index + 1] = (pos & (128 >> k)) ? 0 : 0xff;
		    data[3*index + 2] = (pos & (128 >> k)) ? 0 : 0xff;
		  }
	      }
	  pos[(yrow*65) + i] = totalWidth;
	  totalWidth += width;
	}
      pos[(yrow*65) + 64] = totalWidth;
    }
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
  glPixelStorei(GL_PACK_ROW_LENGTH, 0);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_SKIP_ROWS, 0);
  glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
//   glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, 1024, 64, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, data);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 1024, 64, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
}

GLUnifont::Texture::~Texture()
{
  if (!data) return;
  glDeleteTextures(1, &name);
  delete pos;
  delete data;
}

void GLUnifont::Texture::coords(Unichar uc, float &x1, float &y1, float &x2, float &y2)
{
  GLuint y = (uc & 0xff) >> 6;
  GLuint x = pos[(y*65) + (uc & 0x3f)];
  GLuint width = pos[(y*65) + (uc & 0x3f) + 1] - x;
  y2 = (((4 - y) * static_cast<float>(16)) / 64);
  y1 = y2 - 0.25;
  x1 = static_cast<float>(x) / 1024;
  x2 = static_cast<float>(x + width) / 1024;
}

void GLUnifont::drawChar(Unichar uc) 
{
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  unsigned int stride = 33;
  unsigned int base = stride * uc;
  bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
  unsigned char width = is_halfwidth ? 8 : 16; 
  unsigned char height = 16;
  base++;			// advance past the width marker
#define TEXFONT 1
#if TEXFONT == 1
  GLubyte block = uc >> 8;
  Texture &texture = textures[block];
  if (!texture.bound()) texture.bind(glyphs, block);

  float x1, y1, x2, y2;
  texture.coords(uc, x1, y1, x2, y2);
  glColor3f(1.0, 1.0, 1.0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture.id());
  glBegin(GL_POLYGON);
  glTexCoord2f(x1, y2); glVertex3f(0.0, 0.0, 0.0);
  glTexCoord2f(x2, y2); glVertex3f(width*10.0, 0.0, 0.0);
  glTexCoord2f(x2, y1); glVertex3f(width*10.0, -height*10.0, 0.0);
  glTexCoord2f(x1, y1); glVertex3f(0.0, -height*10.0, 0.0);
  glEnd();
#else
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1); // set to byte-aligned unpacking
  glRasterPos2d(0., 0.);  // position pen
  glBitmap(width, height, 0.0, 0.0, (float)width, 0.0, (const GLubyte *)(&(glyphs[base])));
#endif
}

void GLUnifont::allocateChar(Unichar uc, Graphic::Requisition &r)
{
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  
  unsigned int stride = 33;
  unsigned int base = stride * uc;
  bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
  int width = is_halfwidth ? 8 : 16; 
  int height = 16;
    
  r.x.natural = r.x.minimum = r.x.maximum = width*10.;
  r.x.defined = true;
  r.x.align = 0.;
  r.y.natural = r.y.minimum = r.y.maximum = height*10.;
  r.y.defined = true;
  r.y.align = 1.;
}

