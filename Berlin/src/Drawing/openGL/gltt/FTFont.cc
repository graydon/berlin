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

#include "Drawing/openGL/gltt/FTFont.h"
#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTGlyph.h"

/////////////////////////////////////////////////////////////////////////////

FTFont::FTFont( FTInstance* _instance )
{
  instance= _instance;

  glyphs= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTFont::~FTFont()
{
  destroy();

  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

void FTFont::destroy()
{
  if( glyphs != 0 )
    {
    for( int i= 0; i < 256; ++i )
      delete glyphs[i];

    delete[] glyphs;
    glyphs= 0;
    }
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTFont::create()
{
  destroy();

  if( instance == 0 )
    return GLTT_FALSE;

  int i;

  glyphs= new FTGlyph* [ 256 ];

  for( i= 0; i < 256; ++i )
    glyphs[i]= 0;

  for( i= 0; i < 256; ++i )
    {
    FTGlyph* glyph= new FTGlyph(instance);
    if( ! glyph->create(i) )
      {
      delete glyph;
      continue;
      }

    glyphs[i]= glyph;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

int FTFont::getHeight() const
{
  if( instance == 0 )
    return 0;

  return instance->getHeight();
}

/////////////////////////////////////////////////////////////////////////////

int FTFont::getDescender() const
{
  if( instance == 0 )
    return 0;

  return instance->getDescender();
}

/////////////////////////////////////////////////////////////////////////////

int FTFont::getWidth( const char* text )
{
  if( text == 0 || glyphs == 0 )
    return 0;

  int w= 0;
  for(;;)
    {
    int ch= (unsigned char) *(text++);
    if( ch == 0 )
      break;

    if( glyphs[ch] == 0 )
      continue;

    w+= glyphs[ch]->getAdvance();
    }

  return w / 64;
}

/////////////////////////////////////////////////////////////////////////////

// Contributed by Gerard L. Lanois <gerard@msi.com>
void FTFont::getBBox( const char* text,
                      int& llx, int& lly, int& urx, int& ury ) const
{
  llx= lly= urx= ury= 0;

  if( text == 0 || glyphs == 0 )
    return;

  GLTTboolean first= GLTT_TRUE;

  for(;;)
    {
    int ch= (unsigned char) *(text++);
    if( ch == 0 )
      break;

    if( glyphs[ch] == 0 )
      continue;

    int xMin, yMin, xMax, yMax;
    if( ! glyphs[ch]->getBBox(xMin,yMin,xMax,yMax) )
      continue;

    if( first )
      {
      llx= xMin;
      first= GLTT_FALSE;
      }

    if( yMin < lly )
      lly= yMin;

    if( yMax > ury )
      ury= yMax;

    urx += glyphs[ch]->getAdvance();
    }

  llx /= 64;
  lly /= 64;
  urx /= 64;
  ury /= 64;
}

/////////////////////////////////////////////////////////////////////////////
