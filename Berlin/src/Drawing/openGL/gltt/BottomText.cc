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

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
#else
  #include <GL/gl.h>
#endif

#include "Drawing/openGL/gltt/BottomText.h"
#include "Drawing/openGL/gltt/GLTTPixmapFont.h"

/////////////////////////////////////////////////////////////////////////////

BottomText::BottomText( int _screen_width, int _screen_height )
{
  point_size= 14;
  font= 0;
  msg= "";
  r= g= b= 1.;
  height= 0;
  screen_width= _screen_width;
  screen_height= _screen_height;
}

/////////////////////////////////////////////////////////////////////////////

BottomText::~BottomText()
{
  delete font;
  font= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean BottomText::init( FTFace* face )
{
  delete font;

  font= new GLTTPixmapFont(face);

  if( ! font->create( point_size ) )
    return GLTT_FALSE;

  height= font->getHeight();

  set(1.,1.,1.);

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void BottomText::set( const char* _msg )
{
  msg= _msg;
}

/////////////////////////////////////////////////////////////////////////////

void BottomText::set( float _r, float _g, float _b )
{
  r= _r;
  g= _g;
  b= _b;
}

/////////////////////////////////////////////////////////////////////////////

void BottomText::draw( GLTTboolean center /* = GLTT_FALSE */ )
{
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glOrtho( 0.0, (GLfloat) screen_width,
           0.0, (GLfloat) screen_height, -1.0, 1.0);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  int x= 5;
  int y= 10;

  if( center )
    {
    x= (screen_width - font->getWidth(msg))/2;
    y= (screen_height - font->getHeight())/2;
    }

  glColor3f(r,g,b);
  font->output(x,y,msg);

  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
}

/////////////////////////////////////////////////////////////////////////////
