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

#include <string.h>

#include "Drawing/openGL/gltt/FTGlyphBitmap.h"
#include "Drawing/openGL/gltt/FTGlyph.h"

#include <freetype.h>

/////////////////////////////////////////////////////////////////////////////

FTGlyphBitmap::FTGlyphBitmap( FTGlyph* _glyph )
{
  glyph= _glyph;
  width= height= 0;
  cols= 0;
  buffer= 0;
  advance= 0;
  delta_x= delta_y= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTGlyphBitmap::~FTGlyphBitmap()
{
  destroy();

  glyph= 0;
}

/////////////////////////////////////////////////////////////////////////////

void FTGlyphBitmap::destroy()
{
  delete[] buffer;
  buffer= 0;

  width= height= 0;
  cols= 0;

  advance= 0;
  delta_x= delta_y= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyphBitmap::create()
{
  destroy();

  if( glyph == 0 )
    return GLTT_FALSE;
  if( glyph->getGlyph() == 0 )
    return GLTT_FALSE;

  TT_Glyph_Metrics metrics;
  TT_Error err= TT_Get_Glyph_Metrics( *glyph->getGlyph(), &metrics );

  if( err )
    return GLTT_FALSE;

  TT_BBox& bbox= metrics.bbox; // glyph bounding box

  advance= metrics.advance;
  delta_x= bbox.xMin;
  delta_y= bbox.yMin;

  #define  FLOOR(x)    ((x) & -64)
  #define  CEILING(x)  (((x)+63) & -64)
  bbox.xMin= FLOOR(bbox.xMin);
  bbox.yMin= FLOOR(bbox.yMin);
  bbox.xMax= CEILING(bbox.xMax);
  bbox.yMax= CEILING(bbox.yMax);
  #undef CEILING
  #undef FLOOR

  width = (bbox.xMax - bbox.xMin)/64;
  height= (bbox.yMax - bbox.yMin)/64;

  cols= (width+7) / 8;
  int size= cols * height;

  if( size <= 0 )
    return GLTT_TRUE;

  buffer= new unsigned char [ size ];

  memset( (void*) buffer, 0, size );

  TT_Raster_Map bitmap;

  bitmap.width = width;
  bitmap.cols  = cols;
  bitmap.rows  = height;
  bitmap.flow  = TT_Flow_Up;
  bitmap.size  = size;
  bitmap.bitmap= (void*) buffer;

  err= TT_Get_Glyph_Bitmap( *glyph->getGlyph(),
                            &bitmap,
                            -bbox.xMin,
                            -bbox.yMin );
  if( err )
    {
    delete buffer;
    buffer= 0;
    return GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////
