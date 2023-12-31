/*$Id: Font.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
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
#ifndef _FT_Font_hh
#define _FT_Font_hh

#include <Drawing/FT/Glyph.hh>
#include <vector>

namespace FT
{

class Font
//. used to create glyphs and to access properties not related to individual glyphs
//. such as kerning etc.
{
public:
  Font(const Instance &ii) : i(ii) {}
  ~Font() {}
  Glyph *glyph(unsigned short) const;
  const Instance &instance() const { return i;}
  int height() const { return i.height();}
  int descender() const { return i.descender();}
protected:
  const Instance &i;
};

};

#endif /* _FT_Font_hh */

