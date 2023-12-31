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

#include "Drawing/openGL/gltt/GLTTBitmapFont.h"

#include "Drawing/openGL/gltt/FTBitmapFont.h"
#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphBitmap.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
#else
  #include <GL/gl.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTBitmapFont::GLTTBitmapFont( FTFace* _face )
{
  face= _face;

  instance= 0;

  bitmaps= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTBitmapFont::~GLTTBitmapFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTBitmapFont::destroy()
{
  delete bitmaps;
  bitmaps= 0;

  delete instance;
  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTBitmapFont::create( int point_size )
{
  destroy();

  if( point_size < 1 )
    point_size= 1;

  instance= new FTInstance(face);

  if( ! instance->create() )
    return GLTT_FALSE;

  if( ! instance->setResolutions(96,96) )
    return GLTT_FALSE;

  if( ! instance->setPointSize(point_size) )
    return GLTT_FALSE;

  bitmaps= new FTBitmapFont(instance);

  if( ! bitmaps->create() )
    return GLTT_FALSE;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTBitmapFont::output( int x, int y, const char* text )
{
  if( text == 0 || bitmaps == 0 )
    return;

  x *= 64;
  y *= 64;

  glPixelStorei(GL_UNPACK_ALIGNMENT,1);

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    FTGlyphBitmap* gbitmap= bitmaps->getBitmap(ch);
    if( gbitmap == 0 )
      continue;

    if( gbitmap->getBitmap() != 0 )
      {
      int x_dst= (x+gbitmap->getDeltaX()) / 64;
      int y_dst= (y+gbitmap->getDeltaY()) / 64;

      // Let's handle the case where the bitmap origin generates an
      // invalid raster pos error
      // Contributed by Dirk Reiners <reiners@ecrc.de>
      if( x_dst < 0 || y_dst < 0 )
        {
        int dummy = 0;
        glRasterPos2i( 0, 0 );
        glBitmap( 0, 0, 0, 0, x_dst, y_dst, (const GLubyte*)&dummy );
        }
       else
        glRasterPos2i( x_dst, y_dst );

      glBitmap( gbitmap->getBitmapWidth(),
                gbitmap->getBitmapHeight(),
                0., 0., // x,y orig
                0., 0., // x,y move
                (const GLubyte*) gbitmap->getBitmap() );
      }

    x += gbitmap->getAdvance();
    }
}

/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getWidth( const char* text )
{
  if( bitmaps == 0 )
    return 0;

  return bitmaps->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getHeight() const
{
  if( instance == 0 )
    return 0;

  return instance->getHeight();
}


/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getDescender() const
{
  if( instance == 0 )
    return 0;

  return instance->getDescender();
}

/////////////////////////////////////////////////////////////////////////////
