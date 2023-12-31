/*$Id: GLFTFont.hh,v 1.2 2001/01/09 21:35:08 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _GLFTFont_hh
#define _GLFTFont_hh

#include <Drawing/openGL/GLFont.hh>
#include <freetype/freetype.h>
#include <map>

class GLFTFont : public GLFont
{
  typedef unsigned int atom_t;
  class Atomizer
  {
  public:
    atom_t atomize(Babylon::String &u)
    {
      map<Babylon::String, atom>::iterator i;
      i = atoms.find(u);
      if (i == atoms.end())
	{	
	  atoms[u] = ++atom;
	  return atom;
	}
      else return i->second;
    }
  private:
    atom_t atom;
    map<Babylon::String, atom_t> atoms;
  };
  typedef pair<atom_t, atom_t> style_t;
  typedef map<style, FT_Face> fdictionary_t;
public:
  GLFTFont();
  virtual ~GLFTFont();
  virtual unsigned long size();
  virtual unsigned long weight();
  virtual Unistring *family();
  virtual Unistring *subfamily();
  virtual Unistring *fullname();
  virtual Unistring *style();
  virtual DrawingKit::FontMetrics metrics();
  virtual DrawingKit::GlyphMetrics metrics(Unichar uc);

  virtual void drawChar(Unichar);
  virtual void allocateChar(Unichar, Graphic::Requisition &);
private:
  atom_t atomize(Babylon::String &u) { return atomizer.atomize(u);}
  void setup_face(FT_Face &f);
  void setup_size(FT_Face &f);
  bool load_glyph(Unichar c, FT_Face &f);
  double xres, yres, xdpi, ydpi;  
  Babylon::String family, style;
  size_t size;
  style_t key;
  FT_Library library;
  FT_Face face;
  fdictionary_t faces;
  Atomizer atomizer;
};

#endif

