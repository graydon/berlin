/*$Id: BitmapFont.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
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
#ifndef _FT_BitmapFont_hh
#define _FT_BitmapFont_hh

#include <Drawing/FT/Font.hh>
#include <Drawing/FT/Bitmap.hh>

namespace FT
{

class BitmapFont : public Font
{
public:
  BitmapFont(const Instance &instance) : Font(instance) {}
  ~BitmapFont() {}
  Bitmap *bitmap(const Glyph &);
  Bitmap *bitmap(unsigned int);
};

};

#endif /* _FT_BitmapFont_hh */
