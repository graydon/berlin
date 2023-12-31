/*$Id: GLUnifont.hh,v 1.14 2001/01/26 17:09:49 stefan Exp $
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
#ifndef _GLUnifont_hh
#define _GLUnifont_hh

#include <GL/gl.h>
#include <vector>
#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Graphic.hh>
#include <Prague/Sys/MMap.hh>
#include "Drawing/openGL/GLFont.hh"
#include <Warsaw/Unicode.hh>

class GLUnifont : public GLFont
//. This is a default font, just in case -- a character cell bitmapped unicode
//. font which is generated "on the fly" from the GNU unifont, which we're
//. storing in a packed binary array we mmap() in. this is so that, even if all
//. the font manufactureres in the world turn against us, we can still render
//. multilingual text, albeit not quite as well as certain (ahem) proprietary
//. text systems
{
  class Texture
  //. Glyph Textures are layed out with 64 columns, 4 rows of glyphs
  //. each 32x16 or 16x16 texels large
  {
    enum { columns = 64, rows = 4};
  public:
    Texture() : name(0), pos(0), data(0) {}
    ~Texture();
    void bind(unsigned char *glyphs, GLubyte block);
    bool bound() const { return data;}
    void coords(Warsaw::Unichar, float &, float &, float &, float &);
    GLuint id() { return name;}
  private:
    GLuint   name;
    GLuint  *pos;
    GLubyte *data;
  };
public:
  GLUnifont();
  virtual ~GLUnifont();
  virtual CORBA::ULong size();
  virtual CORBA::ULong weight();
  virtual Warsaw::Unistring *family();
  virtual Warsaw::Unistring *subfamily();
  virtual Warsaw::Unistring *fullname();
  virtual Warsaw::Unistring *style();
  virtual Warsaw::DrawingKit::FontMetrics metrics();
  virtual Warsaw::DrawingKit::GlyphMetrics metrics(Warsaw::Unichar);

  void drawChar(Warsaw::Unichar);
  void allocateChar(Warsaw::Unichar, Warsaw::Graphic::Requisition &);
private:
  Prague::MMap *glyphmap;
  Warsaw::Unistring _family;
  Warsaw::Unistring _subfamily;
  Warsaw::Unistring _fullname;
  Warsaw::Unistring _style;
  Texture textures[256];
};

#endif
