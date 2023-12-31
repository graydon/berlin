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

#ifndef __FTGlyphBitmap_h
#define __FTGlyphBitmap_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

class FTGlyph;

/////////////////////////////////////////////////////////////////////////////

class FTGlyphBitmap
{
protected:
  FTGlyph* glyph;

  int width, height;
  int cols;
  unsigned char* buffer;

  int advance; // 26.6
  int delta_x, delta_y; // 26.6

public:
  FTGlyphBitmap( FTGlyph* _glyph );

  virtual ~FTGlyphBitmap();

  void destroy();

  GLTTboolean create();

  FTGlyph* getGlyph()
    {
    return glyph;
    }

  int getBitmapAllocatedWidth() const
    {
    return cols;
    }
  int getBitmapWidth() const
    {
    return width;
    }
  int getBitmapHeight() const
    {
    return height;
    }
  unsigned char* getBitmap() const
    {
    return buffer;
    }

  // 26.6
  int getDeltaX() const
    {
    return delta_x;
    }

  // 26.6
  int getDeltaY() const
    {
    return delta_y;
    }

  // 26.6
  int getAdvance() const
    {
    return advance;
    }
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTGlyphBitmap_h
