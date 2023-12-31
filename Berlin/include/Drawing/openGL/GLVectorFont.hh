/*$Id: GLVectorFont.hh,v 1.1 1999/12/13 21:36:01 stefan Exp $
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
#ifndef _GLVectorFont_hh
#define _GLVectorFont_hh

#include <Drawing/FT/Face.hh>
#include <Drawing/FT/Instance.hh>
#include <Drawing/FT/Font.hh>
#include <vector>

class GLVectorFont
//. this font uses display lists
{
  struct block { vector<int> glyphs; int lists;};
public:
  GLVectorFont(const FT::Face &, size_t);
  virtual ~GLVectorFont();
  void setPrecision(double p) { precision = p;}
  const FT::Font &font() const { return *tfont;}
  void render(const unsigned short *, const unsigned short *) const;
//   int width(const char *text) { return tfont->width(text);}
  int height() const { return tfont->height();}
  int descender() const { return tfont->descender();}
//   void bbox(const char *text, int& llx, int& lly, int& urx, int& ury) const { tfont->bbox(text, llx, lly, urx, ury);}
protected:
  virtual int loadGlyph(unsigned short) const;
  const FT::Face &face;
  FT::Instance    instance;
  FT::Font       *tfont;
  double          precision;
  mutable vector<block>   cache;
};

#endif /* _GLVectorFont_hh */
