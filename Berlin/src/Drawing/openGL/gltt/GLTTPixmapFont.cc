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

#include "Drawing/openGL/gltt/GLTTPixmapFont.h"

#include "Drawing/openGL/gltt/FTPixmapFont.h"
#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphPixmap.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
#else
  #include <GL/gl.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTPixmapFont::GLTTPixmapFont( FTFace* _face )
{
  face= _face;

  instance= 0;

  pixmaps= 0;
  pixmaps= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTPixmapFont::~GLTTPixmapFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTPixmapFont::destroy()
{
  delete pixmaps;
  pixmaps= 0;

  delete pixmaps;
  pixmaps= 0;

  delete instance;
  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTPixmapFont::create( int point_size )
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

  pixmaps= new FTPixmapFont(instance);

  if( ! pixmaps->create() )
    return GLTT_FALSE;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTPixmapFont::output( int x, int y, const char* text )
{
  if( text == 0 || pixmaps == 0 )
    return;

  x *= 64;
  y *= 64;

  GLfloat color[4];
  glGetFloatv( GL_CURRENT_COLOR, color );
  unsigned char r= (unsigned char)(color[0] * 255.);
  unsigned char g= (unsigned char)(color[1] * 255.);
  unsigned char b= (unsigned char)(color[2] * 255.);
  unsigned char a= (unsigned char)(color[3] * 255.);

//  glPixelStorei(GL_UNPACK_ALIGNMENT,1);

  GLint swapbytes, lsbfirst, rowlength;
  GLint skiprows, skippixels, alignment;

  // Save the current packing mode for bitmaps.
  glGetIntegerv( GL_UNPACK_SWAP_BYTES, &swapbytes );
  glGetIntegerv( GL_UNPACK_LSB_FIRST, &lsbfirst );
  glGetIntegerv( GL_UNPACK_ROW_LENGTH, &rowlength );
  glGetIntegerv( GL_UNPACK_SKIP_ROWS, &skiprows );
  glGetIntegerv( GL_UNPACK_SKIP_PIXELS, &skippixels );
  glGetIntegerv( GL_UNPACK_ALIGNMENT, &alignment );

  // Enforce a standard packing mode
  glPixelStorei( GL_UNPACK_SWAP_BYTES, GL_FALSE );
  glPixelStorei( GL_UNPACK_LSB_FIRST, GL_FALSE );
  glPixelStorei( GL_UNPACK_SKIP_ROWS, 0 );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0 );

  glPushAttrib(GL_ENABLE_BIT);
  glPushAttrib(GL_PIXEL_MODE_BIT);
  glPushAttrib(GL_COLOR_BUFFER_BIT); // for glEnable(GL_ALPHA_TEST)

  glPixelZoom(1.,1.);
  glPixelTransferf( GL_RED_SCALE,   1. );
  glPixelTransferf( GL_GREEN_SCALE, 1. );
  glPixelTransferf( GL_BLUE_SCALE,  1. );
  glPixelTransferf( GL_ALPHA_SCALE, 1. );
  glPixelTransferf( GL_RED_BIAS,    0. );
  glPixelTransferf( GL_GREEN_BIAS,  0. );
  glPixelTransferf( GL_BLUE_BIAS,   0. );
  glPixelTransferf( GL_ALPHA_BIAS,  0. );

  glEnable(GL_ALPHA_TEST);
  glAlphaFunc( GL_GEQUAL, 0.1 );
   glPixelStorei( GL_UNPACK_ALIGNMENT, 4 );

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    FTGlyphPixmap* gpixmap= pixmaps->getPixmap(ch);
    if( gpixmap == 0 )
      continue;

    unsigned char* data= gpixmap->getPixmap(r,g,b,a);
    if( data != 0 )
      {
      int x_dst= (x+gpixmap->getDeltaX()) / 64;
      int y_dst= (y+gpixmap->getDeltaY()) / 64;

      if( x_dst < 0 || y_dst < 0 )
        {
        int dummy = 0;
        glRasterPos2i( 0, 0 );
        glBitmap( 0, 0, 0, 0, x_dst, y_dst, (const GLubyte*)&dummy );
        }
       else
        glRasterPos2i( x_dst, y_dst );

      glPixelStorei( GL_UNPACK_ROW_LENGTH,
 		     gpixmap->getPixmapAllocatedWidth());

      glDrawPixels( gpixmap->getPixmapWidth(),
                    gpixmap->getPixmapHeight(),
                    GL_RGBA,
                    GL_UNSIGNED_BYTE,
                    (const GLvoid*) data );
      }

    x += gpixmap->getAdvance();
    }

  glPopAttrib();
  glPopAttrib();
  glPopAttrib();

  // Restore saved packing modes.
  glPixelStorei( GL_UNPACK_SWAP_BYTES, swapbytes );
  glPixelStorei( GL_UNPACK_LSB_FIRST, lsbfirst );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, rowlength );
  glPixelStorei( GL_UNPACK_SKIP_ROWS, skiprows );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, skippixels );
  glPixelStorei( GL_UNPACK_ALIGNMENT, alignment );
}

/////////////////////////////////////////////////////////////////////////////

int GLTTPixmapFont::getWidth( const char* text )
{
  if( pixmaps == 0 )
    return 0;

  return pixmaps->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////

int GLTTPixmapFont::getHeight() const
{
  if( instance == 0 )
    return 0;

  return instance->getHeight();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTPixmapFont::getDescender() const
{
  if( instance == 0 )
    return 0;

  return instance->getDescender();
}

/////////////////////////////////////////////////////////////////////////////
