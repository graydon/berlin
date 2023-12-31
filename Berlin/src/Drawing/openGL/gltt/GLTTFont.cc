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

#include "Drawing/openGL/gltt/GLTTFont.h"

#include "Drawing/openGL/gltt/FTFont.h"
#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"

#include "Drawing/openGL/gltt/GLTTGlyphPolygonizer.h"

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

GLTTFont::GLTTFont( FTFace* _face )
{
  face= _face;

  instance= 0;
  font= 0;

  loaded= 0;
  list_base= 0;

  precision= 4.;
}

/////////////////////////////////////////////////////////////////////////////

GLTTFont::~GLTTFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::destroy()
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

void GLTTFont::setPrecision( double _precision )
{
  precision= _precision;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTFont::create( int point_size )
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

GLTTboolean GLTTFont::loadGlyph( int i )
{
  if( i < 0 || i > 256 )
    return GLTT_FALSE;

  if( list_base == 0 || loaded == 0 )
    return GLTT_FALSE;

  if( loaded[i] )
    return GLTT_TRUE;

  loaded[i]= GLTT_TRUE;

  GLTTGlyphPolygonizer polygonizer;

  polygonizer.setPrecision(precision);

  int list= list_base + i;
  FTGlyph* glyph= font->getGlyph(i);

  if( glyph == 0 )
    {
    err:
    glNewList(list,GL_COMPILE);
    glEndList();
    return GLTT_TRUE;
    }

  if( ! polygonizer.init(glyph) )
    goto err;

  glNewList(list,GL_COMPILE);

  polygonizer.polygonize();

  glTranslatef( polygonizer.getAdvance(), 0., 0. );

  glEndList();

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::load( const char* text )
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

void GLTTFont::output( const char* text )
{
  if( text == 0 || list_base == 0 || loaded == 0 )
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

int GLTTFont::getHeight() const
{
  if( font == 0 )
    return 0;

  return font->getHeight();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTFont::getDescender() const
{
  if( font == 0 )
    return 0;

  return font->getDescender();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTFont::getWidth( const char* text )
{
  if( font == 0 )
    return 0;

  return font->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::getBBox( const char* text,
                        int& llx, int& lly, int& urx, int& ury ) const
{
  llx= lly= urx= ury= 0;

  if( font == 0 )
    return;

  font->getBBox( text, llx, lly, urx, ury );
}

/////////////////////////////////////////////////////////////////////////////

