/*$Id: BitmapFont.cc,v 1.1 1999/12/13 21:14:34 stefan Exp $
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
#include "Drawing/FT/BitmapFont.hh"
#include <memory>

using namespace FT;

Bitmap *BitmapFont::bitmap(const Glyph &glyph)
{
  Bitmap *bmp = 0;
  try
    {
      bmp = new Bitmap(glyph);
    }
  catch (exception &) {}
  return bmp;
}

Bitmap *BitmapFont::bitmap(unsigned int code)
{
  Bitmap *bmp = 0;
  auto_ptr<Glyph> tmp(glyph(code));
  if (tmp.get()) bmp = bitmap(*tmp);
  return bmp;
}

// int BitmapFont::width(const char *text)
// {
//   if( text == 0 )
//     return 0;

//   int w= 0;
//   for(;;)
//     {
//     int ch= (unsigned char) *(text++);
//     if( ch == 0 )
//       break;

//     loadGlyph(ch);
//     if( bitmaps[ch] == 0 )
//       continue;

//     w+= bitmaps[ch]->getAdvance();
//     }

//   return w / 64;
// }

