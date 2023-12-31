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

#include <stdio.h>

#include "Drawing/openGL/gltt/GLTTGlyphPolygonizerHandler.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
  #include <CygnusGL/winglu.h>
#else
  #include <GL/gl.h>
  #include <GL/glu.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTGlyphPolygonizerHandler::GLTTGlyphPolygonizerHandler(
                                           int _verbose /* = GLTT_FALSE */ )
{
  verbose= _verbose;
  polygonizer= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTGlyphPolygonizerHandler::~GLTTGlyphPolygonizerHandler()
{}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphPolygonizerHandler::begin( int type )
{
  glBegin( GLenum(type) );
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphPolygonizerHandler::vertex( FTGlyphVectorizer::POINT* point )
{
  if( point == 0 )
    return;

  glVertex2f( point->x, point->y );
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphPolygonizerHandler::end()
{
  glEnd();
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphPolygonizerHandler::error( int error )
{
  if( ! verbose )
    return;

  const char* str= (const char*)gluErrorString(GLenum(error));

  fprintf( stderr, "GLU error #%d (%s)\n", int(error),
                                           (str==0) ? "" : str );
}

/////////////////////////////////////////////////////////////////////////////
