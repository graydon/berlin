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

#ifndef __FTFace_h
#define __FTFace_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

struct TT_Face_;
typedef struct TT_Face_ TT_Face;

class FTEngine;

/////////////////////////////////////////////////////////////////////////////

class FTFace
{
protected:
  FTEngine* engine;
  TT_Face* face;
  int indices[256];
  int* CP_table;

public:
  FTFace( FTEngine* _engine = 0 );

  virtual ~FTFace();

  GLTTboolean open( const char* filename );

  int CP_Translate( int code ) const;

private:
  GLTTboolean makeIndicesTable();

public:
  FTEngine* getEngine() const
    {
    return engine;
    }

  TT_Face* getFace() const
    {
    return face;
    }

  int getGlyphIndex( int ascii_code ) const
    {
    if( ascii_code < 0 || ascii_code > 255 )
      ascii_code= 0;

    return indices[ascii_code];
    }
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTFace_h
