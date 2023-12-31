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

#ifndef __FTGlyphVectorizer_h
#define __FTGlyphVectorizer_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

struct TT_Outline_;
typedef struct TT_Outline_ TT_Outline;

class FTGlyph;

/////////////////////////////////////////////////////////////////////////////

class FTGlyphVectorizer
{
public:
  struct POINT
    {
    double x, y;
    void* data;
    };

  class Contour
    {
    public:
      int max_points;
      POINT* points;
      int nPoints;
      GLTTboolean clockwise;
      double area;
      double x_min, x_max;
      double y_min, y_max;

      Contour()
        {
        clockwise= GLTT_FALSE;
        nPoints= 0;
        points= 0;
        max_points= 0;
        area= 0.;
        x_min= y_min= 1e20;
        x_max= y_max=-1e20;
        }
      ~Contour()
        {
        delete[] points;
        points= 0;
        nPoints= 0;
        }

    GLTTboolean exterior() const
      {
      return clockwise;
      }

    private:
      void add_point( double x, double y );

    friend FTGlyphVectorizer;
    };

protected:
  FTGlyph* glyph;

  TT_Outline* outline;

  // needed distance between two bezier curve evaluations (in pixels)
  double precision;

  Contour** contours;
  int nContours;

public:
  FTGlyphVectorizer();

  virtual ~FTGlyphVectorizer();

  void destroy();

  GLTTboolean init( FTGlyph* _glyph );

  void setPrecision( double _precision );

  FTGlyph* getGlyph() const
    {
    return glyph;
    }

  GLTTboolean vectorize();

  int getNContours() const
    {
    return nContours;
    }

  // 0 <= c < nContours
  Contour* getContour( int c ) const
    {
    if( c < 0 || c > nContours || contours == 0 )
      return 0;
    return contours[c];
    }

  double getBearingX() const;
  double getBearingY() const;
  double getAdvance() const;

private:
  GLTTboolean vectorizeContour( int c );
  void add_point( int c, double x, double y );

  void sortContours();
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTGlyphVectorizer_h
