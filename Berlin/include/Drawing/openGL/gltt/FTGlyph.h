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

#ifndef __FTGlyph_h
#define __FTGlyph_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

struct TT_Glyph_;
typedef struct TT_Glyph_ TT_Glyph;
struct TT_Glyph_Metrics_;
typedef struct TT_Glyph_Metrics_ TT_Glyph_Metrics;

class FTInstance;

/////////////////////////////////////////////////////////////////////////////

class FTGlyph
{
protected:
  FTInstance* instance;

  TT_Glyph* glyph;

  int ascii_code;

  TT_Glyph_Metrics* metrics;

public:
  FTGlyph( FTInstance* _instance );

  virtual ~FTGlyph();

  GLTTboolean create( int _ascii_code );

  int getAsciiCode() const
    {
    return ascii_code;
    }

  TT_Glyph* getGlyph()
    {
    return glyph;
    }

  FTInstance* getInstance()
    {
    return instance;
    }

  int getBearingX() const;
  int getBearingY() const;
  int getAdvance() const;

  GLTTboolean getBBox( int& xMin, int& yMin, int& xMax, int& yMax ) const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTGlyph_h
