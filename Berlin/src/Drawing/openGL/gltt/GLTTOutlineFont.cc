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

#include "Drawing/openGL/gltt/GLTTOutlineFont.h"

#include "Drawing/openGL/gltt/FTFont.h"
#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
#else
  #include <GL/gl.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTOutlineFont::GLTTOutlineFont( FTFace* _face )
{
  face= _face;

  instance= 0;
  font= 0;

  loaded= 0;
  list_base= 0;

  precision= 4.;
}

/////////////////////////////////////////////////////////////////////////////

GLTTOutlineFont::~GLTTOutlineFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTOutlineFont::destroy()
{
  delete[] loaded;
  loaded= 0;

  if( list_base != 0 )
    {
    glDeleteLists( list_base, 256 );
    list_base= 0;
    }

  delete font;
  font= 0;

  delete instance;
  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTOutlineFont::setPrecision( double _precision )
{
  precision= _precision;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTOutlineFont::create( int point_size )
{
  destroy();

  if( point_size < 1 )
    point_size= 1;

  instance= new FTInstance(face);

  if( ! instance->create() )
    return GLTT_FALSE;

  int resolution= 96;
  if( ! instance->setResolutions(resolution,resolution) )
    return GLTT_FALSE;

  if( ! instance->setPointSize(point_size) )
    return GLTT_FALSE;

//  int pixel_size= resolution * point_size / 72;

  font= new FTFont(instance);

  if( ! font->create() )
    return GLTT_FALSE;

  list_base= glGenLists(256);
  if( list_base == 0 )
    return GLTT_FALSE;

  loaded= new GLTTboolean [ 256 ];
  for( int i= 0; i < 256; ++i )
    loaded[i]= GLTT_FALSE;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTOutlineFont::loadGlyph( int i )
{
  if( i < 0 || i > 256 )
    return GLTT_FALSE;
  if( font == 0 || loaded == 0 )
    return GLTT_FALSE;

  if( loaded[i] )
    return GLTT_TRUE;

  loaded[i]= GLTT_TRUE;

  FTGlyphVectorizer vectorizer;
  vectorizer.setPrecision(precision);

  int list= list_base + i;
  FTGlyph* glyph= font->getGlyph(i);
  if( glyph == 0 )
    {
    err:
    glNewList(list,GL_COMPILE);
    glEndList();
    return GLTT_TRUE;
    }

  if( ! vectorizer.init(glyph) )
    goto err;

  vectorizer.vectorize();

  glNewList(list,GL_COMPILE);

  for( int j= 0; j < vectorizer.getNContours(); ++j )
    {
    FTGlyphVectorizer::Contour* contour= vectorizer.getContour(j);
    if( contour == 0 )
      continue;

    if( contour->nPoints <= 0 )
      continue;

    glBegin(GL_LINE_LOOP);

    for( int k= 0; k < contour->nPoints; ++k )
      {
      FTGlyphVectorizer::POINT& p= contour->points[k];

      glVertex2f( p.x, p.y );
      }

    glEnd();
    }

  glTranslatef( vectorizer.getAdvance(), 0., 0. );
  glEndList();

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTOutlineFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTOutlineFont::load( const char* text )
{
  if( text == 0 || list_base == 0 )
    return;

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    if( ! loaded[ch] )
      loadGlyph(ch);
    }
}

/////////////////////////////////////////////////////////////////////////////

void GLTTOutlineFont::output( const char* text )
{
  if( text == 0 || list_base == 0 )
    return;

  glPushMatrix();

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    if( ! loaded[ch] )
      loadGlyph(ch);

    glCallList( list_base + ch );
    }

//  glPushAttrib( GL_LIST_BIT );
//  glListBase(list_base);
//  glCallLists( strlen(text), GL_UNSIGNED_BYTE, (GLubyte*)text );
//  glPopAttrib();

  glPopMatrix();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTOutlineFont::getHeight() const
{
  if( font == 0 )
    return 0;

  return font->getHeight();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTOutlineFont::getDescender() const
{
  if( font == 0 )
    return 0;

  return font->getDescender();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTOutlineFont::getWidth( const char* text )
{
  if( font == 0 )
    return 0;

  return font->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////
