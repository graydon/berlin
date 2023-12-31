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

/*
  WARNING: GLTTGlyphPolygonizer is *NOT* multi-thread safe! /SR
*/

#include <stdio.h>

#include "Drawing/openGL/gltt/FTGlyph.h"
#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"
#include "Drawing/openGL/gltt/GLTTGlyphPolygonizer.h"
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

GLTTGlyphPolygonizer::GLTTGlyphPolygonizer(
                                  FTGlyphVectorizer* _vectorizer /* = 0 */ )
{
  glyph= 0;

  if( _vectorizer == 0 )
    {
    vectorizer= new FTGlyphVectorizer;
    own_vectorizer= GLTT_TRUE;
    }
   else
    {
    vectorizer= _vectorizer;
    own_vectorizer= GLTT_FALSE;
    }
}

/////////////////////////////////////////////////////////////////////////////

GLTTGlyphPolygonizer::~GLTTGlyphPolygonizer()
{
  if( own_vectorizer )
    delete vectorizer;
  vectorizer= 0;

  glyph= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphPolygonizer::setPrecision( double _precision )
{
  vectorizer->setPrecision(_precision);
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTGlyphPolygonizer::init( FTGlyph* _glyph )
{
  glyph= _glyph;

  if( ! own_vectorizer )
    return GLTT_TRUE;

  return vectorizer->init(glyph);
}

/////////////////////////////////////////////////////////////////////////////

// MMh, this code is *NOT* multi-thread safe!
// (we could synchronize the polygonize() function (in the Java way..)
// /SR
static GLTTGlyphPolygonizerHandler* handler= 0;

// Default OpenGL handler
static GLTTGlyphPolygonizerHandler* default_handler= 0;

// CALLBACK is for windoze users... not SGI ones!
// Thanks to G. Lanois (gerard@msi.com)
#if !defined(CALLBACK) && !defined(__WIN32__) && !defined(__WINDOWS__)
  #define CALLBACK
#endif

// IMHO, the (...) vs. (void) warning is due to GNU-C/C++.
#if defined(__GNUC__) || defined(_GNUG_)
 #define CALLBACKARG ...
#else
 #define CALLBACKARG void
#endif

static void CALLBACK gltt_polygonizer_begin( GLenum type )
{
  if( handler != 0 )
    handler->begin(int(type));
}

static void CALLBACK gltt_polygonizer_vertex( void* data )
{
  if( handler != 0 )
    handler->vertex( (FTGlyphVectorizer::POINT*) data );
}

static void CALLBACK gltt_polygonizer_end()
{
  if( handler != 0 )
    handler->end();
}

static void CALLBACK gltt_polygonizer_error( GLenum error )
{
  if( handler != 0 )
    handler->error(int(error));
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTGlyphPolygonizer::polygonize(
                            GLTTGlyphPolygonizerHandler* handler /* = 0 */ )
{
  if( glyph == 0 )
    {
    if( vectorizer == 0 )
      return GLTT_FALSE;

    glyph= vectorizer->getGlyph();

    if( glyph == 0 )
      return GLTT_FALSE;
    }

  if( handler == 0 )
    {
    if( default_handler == 0 )
      default_handler= new GLTTGlyphPolygonizerHandler();
    handler= ::default_handler; // use the default OpenGL handler
    }

  GLUtriangulatorObj* tobj= gluNewTess();

  if( tobj == 0 )
    return GLTT_FALSE;

  if( own_vectorizer )
    {
    if( ! vectorizer->vectorize() )
      {
      gluDeleteTess(tobj);
      return GLTT_FALSE;
      }
    }

  ::handler= handler;
  handler->polygonizer= this;

  // this time it should work /srh July 98
#if defined(WIN32) && !defined(__CYGWIN32__)
  typedef void (CALLBACK *glu_callback)(CALLBACKARG);
#else
  typedef void CALLBACK (*glu_callback)(CALLBACKARG);
#endif

  gluTessCallback( tobj, GLenum(GLU_BEGIN),
                   (glu_callback) gltt_polygonizer_begin );
  gluTessCallback( tobj, GLenum(GLU_VERTEX),
                   (glu_callback) gltt_polygonizer_vertex );
  gluTessCallback( tobj, GLenum(GLU_END),
                   (glu_callback) gltt_polygonizer_end );
  gluTessCallback( tobj, GLenum(GLU_ERROR),
                   (glu_callback) gltt_polygonizer_error );

  int nContours= vectorizer->getNContours();

  GLTTboolean in_polygon= GLTT_FALSE;
  GLTTboolean first_contour= GLTT_FALSE;

  for( int j= 0; j < nContours; ++j )
    {
    FTGlyphVectorizer::Contour* contour= vectorizer->getContour(j);
    if( contour == 0 )
      continue;

    if( contour->clockwise )
      {
      if( in_polygon )
        gluEndPolygon(tobj);

      gluBeginPolygon(tobj);
      in_polygon= GLTT_TRUE;
      first_contour= GLTT_TRUE;
      }

    if( ! in_polygon )
      continue;

    if( first_contour )
      {
      gluNextContour( tobj, GLenum(GLU_EXTERIOR) );
      first_contour= GLTT_FALSE;
      }
     else
      gluNextContour( tobj, GLenum(GLU_INTERIOR) );

    int nPoints= contour->nPoints;

    for( int k= nPoints-1; k >= 0; --k )
      {
      FTGlyphVectorizer::POINT& p= contour->points[k];

      GLdouble data[3];
      data[0]= p.x;
      data[1]= p.y;
      data[2]= 0.;
      gluTessVertex( tobj, data, (void*) &(contour->points[k]) );
      }
    }

  if( in_polygon )
    gluEndPolygon(tobj);

  gluDeleteTess(tobj);

  handler->polygonizer= 0;
  ::handler= 0;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

double GLTTGlyphPolygonizer::getBearingX() const
{
  return vectorizer->getBearingX();
}

/////////////////////////////////////////////////////////////////////////////

double GLTTGlyphPolygonizer::getBearingY() const
{
  return vectorizer->getBearingY();
}

/////////////////////////////////////////////////////////////////////////////

double GLTTGlyphPolygonizer::getAdvance() const
{
  return vectorizer->getAdvance();
}

/////////////////////////////////////////////////////////////////////////////
