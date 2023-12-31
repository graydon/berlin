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

#include "Drawing/openGL/gltt/FTPixmapFont.h"
#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphPixmap.h"

/////////////////////////////////////////////////////////////////////////////

FTPixmapFont::FTPixmapFont( FTInstance* _instance ): FTFont(_instance)
{
  pixmaps= 0;
  loaded= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTPixmapFont::~FTPixmapFont()
{
  destroy();
}

/////////////////////////////////////////////////////////////////////////////

void FTPixmapFont::destroy()
{
  if( pixmaps != 0 )
    {
    for( int i= 0; i < 256; ++i )
      delete pixmaps[i];

    delete[] pixmaps;
    pixmaps= 0;
    }

  delete[] loaded;
  loaded= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTPixmapFont::create()
{
  destroy();

  if( ! FTFont::create() )
    return GLTT_FALSE;

  int i;

  pixmaps= new FTGlyphPixmap* [ 256 ];
  loaded= new GLTTboolean [ 256 ];

  for( i= 0; i < 256; ++i )
    {
    pixmaps[i]= 0;
    loaded[i]= GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTPixmapFont::loadGlyph( int ascii_code )
{
  if( ascii_code < 0 || ascii_code > 255 || pixmaps == 0 || loaded == 0 )
    return GLTT_FALSE;

  if( loaded[ascii_code] )
    return GLTT_TRUE;

  loaded[ascii_code]= GLTT_TRUE;

  FTGlyph* glyph= FTFont::glyphs[ascii_code];
  if( glyph == 0 )
    return GLTT_FALSE;

  FTGlyphPixmap* gpixmap= new FTGlyphPixmap(glyph);
  if( ! gpixmap->create() )
    {
    delete gpixmap;
    return GLTT_FALSE;
    }

  pixmaps[ascii_code]= gpixmap;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void FTPixmapFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

int FTPixmapFont::getWidth( const char* text )
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
    if( pixmaps[ch] == 0 )
      continue;

    w+= pixmaps[ch]->getAdvance();
    }

  return w / 64;
}

/////////////////////////////////////////////////////////////////////////////
