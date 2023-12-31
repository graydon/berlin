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

#ifndef __GLTTOutlineFont_h
#define __GLTTOutlineFont_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

class FTFace;
class FTInstance;
class FTFont;

/////////////////////////////////////////////////////////////////////////////

class GLTTOutlineFont
{
protected:
  FTFace* face;

  FTInstance* instance;

  FTFont* font;

  GLTTboolean* loaded;
  int list_base;

  double precision;

public:
  GLTTOutlineFont( FTFace* _face );

  virtual ~GLTTOutlineFont();

  void destroy();

  void setPrecision( double _precision );

  GLTTboolean create( int point_size );

  GLTTboolean loadGlyph( int i );
  void load( int from = 0, int to = 255 );
  void load( const char* text );

  FTFont* getFont() const
    {
    return font;
    }

  void output( const char* text );

  int getHeight() const;
  int getDescender() const;
  int getWidth( const char* text );
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTOutlineFont_h
