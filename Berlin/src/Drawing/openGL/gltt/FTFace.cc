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

#include "Drawing/openGL/gltt/FTFace.h"
#include "Drawing/openGL/gltt/FTEngine.h"

#include <freetype.h>

/////////////////////////////////////////////////////////////////////////////

FTFace::FTFace( FTEngine* _engine /* = 0 */ )
{
  if( _engine == 0 )
    engine= FTEngine::getStaticEngine();
   else
    engine= _engine;

  face= 0;

  CP_table= 0;
  for( int i= 0; i < 256; ++i )
    indices[i]= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTFace::~FTFace()
{
  if( face != 0 )
    {
    TT_Close_Face(*face);
    delete face;
    face= 0;
    }

  engine= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTFace::open( const char* filename )
{
  if( face != 0 )
    return GLTT_FALSE; // already open?!

  if( engine == 0 )
    return GLTT_FALSE;
  if( engine->getEngine() == 0 )
    return GLTT_FALSE;

  face= new TT_Face;
  if( TT_Open_Face( *(engine->getEngine()), filename, face ) )
    {
    delete face;
    face= 0;
    return GLTT_FALSE;
    }

  return makeIndicesTable();
}

/////////////////////////////////////////////////////////////////////////////

int FTFace::CP_Translate( int code ) const
{
  if( CP_table == 0 )
    return code;

  return CP_table[code];
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTFace::makeIndicesTable()
{
  if( face == 0 )
    return GLTT_FALSE;

  // First, look for a Unicode charmap
  TT_CharMap char_map;
  int n= TT_Get_CharMap_Count(*face);
  int i;

  for( i= 0; i < n; ++i )
    {
    short unsigned platform= 0;
    short unsigned encoding= 0;
    TT_Get_CharMap_ID( *face, i, &platform, &encoding );
    if ( (platform == 3 && encoding == 1 )  ||
         (platform == 0 && encoding == 0 ) )
      {
      TT_Get_CharMap( *face, i, &char_map );
      break;
      }
    }

  if( i == n )
    {
    TT_Face_Properties  properties;
    TT_Get_Face_Properties( *face, &properties );
    int num_glyphs= properties.num_Glyphs;

    for( int ascii_code= 0; ascii_code < 256; ++ascii_code )
      {
//      int glyph_index= ascii_code - int(' ') + 1;
      int glyph_index= TT_Char_Index( char_map, CP_Translate(ascii_code) );
      if( glyph_index < 0 || glyph_index >= num_glyphs )
        glyph_index= 0;

      indices[ascii_code]= glyph_index;
      }
    }
   else
    {
    for( int ascii_code= 0; ascii_code < 256; ++ascii_code )
      {
      int glyph_index= TT_Char_Index( char_map, ascii_code );
      if( glyph_index < 0 )
        glyph_index = 0;  // FIXME! default code

      indices[ascii_code]= glyph_index;
      }
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////
