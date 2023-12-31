/*$Id: GLBitmapFont.hh,v 1.2 2000/03/20 22:23:24 stefan Exp $
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
#ifndef _GLBitmapFont_hh
#define _GLBitmapFont_hh

#include <Drawing/FT/Face.hh>
#include <Drawing/FT/Instance.hh>
#include <Drawing/FT/BitmapFont.hh>

class GLBitmapFont
//. this font uses bitmaps
{
  typedef vector<FT::Bitmap *> block;
public:
  GLBitmapFont(const FT::Face &, size_t);
  ~GLBitmapFont();
//  FT::BitmapFont *font() const { return bfont;}
  virtual unsigned long size() {}
  void render(int x, int y, const unsigned short *, const unsigned short *) const;
//   int width(const char *text) { return bfont->width(text);}
  int height() const { return instance.height();}
  int descender() const { return instance.descender();}
private:
  FT::Bitmap *loadGlyph(unsigned short) const;
  const FT::Face &face;
  FT::Instance    instance;
  FT::BitmapFont *bfont;
  mutable vector<block>   cache;
};

#endif /* _GLBitmapFont_hh */
