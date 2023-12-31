/*
 * gltt graphics library
 * Copyright (C) 1998-1999 Stephane Rehel
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
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "Drawing/openGL/gltt/FTBitmapFont.h"
#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphBitmap.h"

/////////////////////////////////////////////////////////////////////////////

FTBitmapFont::FTBitmapFont( FTInstance* _instance ): FTFont(_instance)
{
  bitmaps= 0;
  loaded= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTBitmapFont::~FTBitmapFont()
{
  destroy();
}

/////////////////////////////////////////////////////////////////////////////

void FTBitmapFont::destroy()
{
  if( bitmaps != 0 )
    {
    for( int i= 0; i < 256; ++i )
      delete bitmaps[i];

    delete[] bitmaps;
    bitmaps= 0;
    }

  delete[] loaded;
  loaded= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTBitmapFont::create()
{
  destroy();

  if( ! FTFont::create() )
    return GLTT_FALSE;

  int i;

  bitmaps= new FTGlyphBitmap* [ 256 ];
  loaded= new GLTTboolean [ 256 ];

  for( i= 0; i < 256; ++i )
    {
    bitmaps[i]= 0;
    loaded[i]= GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTBitmapFont::loadGlyph( int ascii_code )
{
  if( ascii_code < 0 || ascii_code > 255 || bitmaps == 0 || loaded == 0 )
    return GLTT_FALSE;

  if( loaded[ascii_code] )
    return GLTT_TRUE;

  loaded[ascii_code]= GLTT_TRUE;

  FTGlyph* glyph= FTFont::glyphs[ascii_code];
  if( glyph == 0 )
    return GLTT_FALSE;

  FTGlyphBitmap* gbitmap= new FTGlyphBitmap(glyph);
  if( ! gbitmap->create() )
    {
    delete gbitmap;
    return GLTT_FALSE;
    }

  bitmaps[ascii_code]= gbitmap;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void FTBitmapFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

int FTBitmapFont::getWidth( const char* text )
{
  if( text == 0 )
    return 0;

  int w= 0;
  for(;;)
    {
    int ch= (unsigned char) *(text++);
    if( ch == 0 )
      break;

    loadGlyph(ch);
    if( bitmaps[ch] == 0 )
      continue;

    w+= bitmaps[ch]->getAdvance();
    }

  return w / 64;
}

/////////////////////////////////////////////////////////////////////////////

