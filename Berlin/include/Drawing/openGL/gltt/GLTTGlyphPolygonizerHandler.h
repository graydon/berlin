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

#ifndef __GLTTGlyphPolygonizerHandler_h
#define __GLTTGlyphPolygonizerHandler_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

#ifndef __FTGlyphVectorizer_h
#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"
#endif

/////////////////////////////////////////////////////////////////////////////

// Mmh, this class name is a bit long... /SR
class GLTTGlyphPolygonizerHandler
{
  friend class GLTTGlyphPolygonizer;

protected:
  GLTTboolean verbose;

  GLTTGlyphPolygonizer* polygonizer; // set by GLTTGlyphPolygonizer

public:
  GLTTGlyphPolygonizerHandler( GLTTboolean _verbose = GLTT_FALSE );

  virtual ~GLTTGlyphPolygonizerHandler();

  virtual void begin( int type );
  virtual void vertex( FTGlyphVectorizer::POINT* point );
  virtual void end();
  virtual void error( int error );
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTGlyphPolygonizerHandler_h
