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

#ifndef __GLTTGlyphPolygonizer_h
#define __GLTTGlyphPolygonizer_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

class FTGlyph;
class FTGlyphVectorizer;
class GLTTGlyphPolygonizerHandler;

/////////////////////////////////////////////////////////////////////////////

class GLTTGlyphPolygonizer
{
protected:
  FTGlyph* glyph;

  FTGlyphVectorizer* vectorizer;
  GLTTboolean own_vectorizer;

public:
  GLTTGlyphPolygonizer( FTGlyphVectorizer* _vectorizer = 0 );

  virtual ~GLTTGlyphPolygonizer();

  void setPrecision( double _precision );

  GLTTboolean init( FTGlyph* _glyph );

  GLTTboolean polygonize( GLTTGlyphPolygonizerHandler* handler = 0 );

  FTGlyphVectorizer* getVectorizer() const
    {
    return vectorizer;
    }

  double getBearingX() const;
  double getBearingY() const;
  double getAdvance() const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTGlyphPolygonizer_h
