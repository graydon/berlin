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

#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTFace.h"
#include "Drawing/openGL/gltt/FTGlyph.h"

#include <freetype.h>

/////////////////////////////////////////////////////////////////////////////

FTGlyph::FTGlyph( FTInstance* _instance )
{
  instance= _instance;

  glyph= 0;

  metrics= new TT_Glyph_Metrics;
}

/////////////////////////////////////////////////////////////////////////////

FTGlyph::~FTGlyph()
{
  delete metrics;
  metrics= 0;

  if( glyph != 0 )
    {
    TT_Done_Glyph(*glyph);
    delete glyph;
    glyph= 0;
    }

  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyph::create( int _ascii_code )
{
  ascii_code= _ascii_code;

  if( glyph != 0 )
    {
    delete glyph;
    glyph= 0;
    }

  if( instance == 0 )
    return GLTT_FALSE;
  if( instance->getInstance() == 0 )
    return GLTT_FALSE;

  FTFace* face= instance->getFace();
  if( face == 0 )
    return GLTT_FALSE;

  int glyph_index= face->getGlyphIndex(ascii_code);

  glyph= new TT_Glyph;

  TT_Error err;

  err= TT_New_Glyph( *face->getFace(), glyph );

  if( err )
    {
    delete glyph;
    glyph= 0;
    return GLTT_FALSE;
    }

  err= TT_Load_Glyph( *(instance->getInstance()),
                      *glyph, glyph_index, TTLOAD_DEFAULT );

  if( err )
    {
    delete glyph;
    glyph= 0;
    return GLTT_FALSE;
    }

  err= TT_Get_Glyph_Metrics( *glyph, metrics );
  if( err )
    {
    delete metrics;
    metrics= 0;

    return GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

int FTGlyph::getBearingX() const
{
  return (metrics==0) ? 0 : metrics->bearingX;
}

/////////////////////////////////////////////////////////////////////////////

int FTGlyph::getBearingY() const
{
  return (metrics==0) ? 0 : metrics->bearingY;

//  return 0; //(metrics==0) ? 0 : metrics->bearingY;
            // metrics.bearingY is left uninitialized by freetype?!
}

/////////////////////////////////////////////////////////////////////////////

int FTGlyph::getAdvance() const
{
  return (metrics==0) ? 0 : metrics->advance;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyph::getBBox( int& xMin, int& yMin,
                              int& xMax, int& yMax ) const
{
  if( metrics == 0 )
    return GLTT_FALSE;

  TT_BBox& bbox= metrics->bbox; // glyph bounding box

  xMin= bbox.xMin;
  yMin= bbox.yMin;
  xMax= bbox.xMax;
  yMax= bbox.yMax;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////
