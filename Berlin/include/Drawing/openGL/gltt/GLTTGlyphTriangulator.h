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

#ifndef __GLTTGlyphTriangulator_h
#define __GLTTGlyphTriangulator_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

#ifndef __GLTTGlyphPolygonizerHandler_h
#include "Drawing/openGL/gltt/GLTTGlyphPolygonizerHandler.h"
#endif

// class FTGlyphVectorizer;
// struct FTGlyphVectorizer::POINT;
class GLTTGlyphPolygonizer;

/////////////////////////////////////////////////////////////////////////////

class GLTTGlyphTriangulator: public GLTTGlyphPolygonizerHandler
{
protected:
  int type;
  GLTTGlyphPolygonizer* polygonizer;

public:
  GLTTGlyphTriangulator( FTGlyphVectorizer* _vectorizer = 0,
                         GLTTboolean _verbose = GLTT_FALSE );

  virtual ~GLTTGlyphTriangulator();

  void setPrecision( double _precision );

  GLTTboolean init( FTGlyph* _glyph );

  GLTTboolean triangulate();

  virtual void triangle( FTGlyphVectorizer::POINT* p1,
                         FTGlyphVectorizer::POINT* p2,
                         FTGlyphVectorizer::POINT* p3 ) = 0;

private:
  virtual void begin( int _type );
  virtual void vertex( FTGlyphVectorizer::POINT* point );
  virtual void end();

  int vertex_index;
  FTGlyphVectorizer::POINT* vertices[2];
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTGlyphTriangulator_h
