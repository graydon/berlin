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

#include "Drawing/openGL/gltt/GLTTGlyphTriangulator.h"

#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"
#include "Drawing/openGL/gltt/GLTTGlyphPolygonizer.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __CYGWIN32__
  #include <CygnusGL/wingl.h>
#else
  #include <GL/gl.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTGlyphTriangulator::GLTTGlyphTriangulator(
                                   FTGlyphVectorizer* _vectorizer /* = 0 */,
                                   GLTTboolean _verbose /* = GLTT_FALSE */ ):
  GLTTGlyphPolygonizerHandler(_verbose)
{
  polygonizer= new GLTTGlyphPolygonizer(_vectorizer);

  vertex_index= 0;
  vertices[0]= vertices[1]= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTGlyphTriangulator::~GLTTGlyphTriangulator()
{
  delete polygonizer;
  polygonizer= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphTriangulator::setPrecision( double _precision )
{
  polygonizer->setPrecision(_precision);
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTGlyphTriangulator::init( FTGlyph* _glyph )
{
  return polygonizer->init(_glyph);
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTGlyphTriangulator::triangulate()
{
  vertex_index= 0;
  vertices[0]= vertices[1]= 0;

  return polygonizer->polygonize(this);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphTriangulator::begin( int _type )
{
  type= _type;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphTriangulator::vertex( FTGlyphVectorizer::POINT* point )
{
  if( type == 0 || point == 0 )
    return;

  if( vertex_index++ < 2 )
    {
    vertices[vertex_index-1]= point;
    return;
    }

  triangle( vertices[0], vertices[1], point );

  switch( type )
    {
    case GL_TRIANGLE_STRIP:
      {
//      vertices[0]= vertices[1];
//      vertices[1]= point;

      // Contributed by Dirk Reiners <reiners@ecrc.de>
      if( vertex_index & 1 )
        vertices[0]= point;
       else
        vertices[1]= point;
      break;
      }
    case GL_TRIANGLE_FAN:
      {
      vertices[1]= point;
      break;
      }
    case GL_TRIANGLES:
    default:
      {
      vertex_index= 0;
      break;
      }
    }
}

/////////////////////////////////////////////////////////////////////////////

void GLTTGlyphTriangulator::end()
{
  type= 0;
  vertex_index = 0;
}

/////////////////////////////////////////////////////////////////////////////
