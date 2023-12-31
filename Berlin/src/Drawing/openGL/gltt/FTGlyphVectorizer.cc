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

#include <math.h>
#include <string.h>

#include "Drawing/openGL/gltt/FTGlyphVectorizer.h"
#include "Drawing/openGL/gltt/FTGlyph.h"

#include <freetype.h>

#include "Drawing/openGL/gltt/GLTTminmax.h"

/////////////////////////////////////////////////////////////////////////////

// private
void FTGlyphVectorizer::Contour::add_point( double x, double y )
{
  if( points == 0 )
    {
    max_points= 8;
    points= new POINT [ max_points ];
    nPoints= 0;
    }
   else
    {
    if( nPoints >= max_points )
      {
      int new_max_points= max_points + max_points / 2;
      POINT* new_points= new POINT [ new_max_points ];
      memcpy( (void*) new_points, (void*) points, nPoints * sizeof(points[0]) );
      delete[] points;
      points= new_points;
      max_points= new_max_points;
      }
    }

  register POINT* p= points + nPoints;

  if( nPoints > 0 )
    {
    POINT* prev= p - 1;
    if( fabs(prev->x - x) < 1e-8 && fabs(prev->y - y) < 1e-8 )
      return; // coincident vertices
    }

  p->x= x;
  p->y= y;
  p->data= 0;

  if( x < x_min ) x_min= x;
  if( x > x_max ) x_max= x;
  if( y < y_min ) y_min= y;
  if( y > y_max ) y_max= y;

  if( nPoints > 0 )
    {
    --p;
    area += p->x*y - x*p->y;
    }

  ++nPoints;
}

/////////////////////////////////////////////////////////////////////////////

FTGlyphVectorizer::FTGlyphVectorizer()
{
  glyph= 0;

  outline= new TT_Outline;

  contours= 0;
  nContours= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTGlyphVectorizer::~FTGlyphVectorizer()
{
  delete outline;
  outline= 0;

  destroy();
}

/////////////////////////////////////////////////////////////////////////////

void FTGlyphVectorizer::destroy()
{
  if( contours != 0 )
    {
    for( int i= 0; i < nContours; ++i )
      delete contours[i];

    delete[] contours;
    contours= 0;
    }

  nContours= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyphVectorizer::init( FTGlyph* _glyph )
{
  destroy();

  glyph= _glyph;

  if( glyph == 0 )
    return 0;

  TT_Error error= TT_Get_Glyph_Outline( *glyph->getGlyph(), outline );
  if( error )
    return GLTT_FALSE;

  nContours= (int) outline->n_contours;

  contours= new Contour* [ nContours ];
  for( int i= 0; i < nContours; ++i )
    contours[i]= new Contour;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void FTGlyphVectorizer::setPrecision( double _precision )
{
  precision= max( _precision, 0.01 );
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyphVectorizer::vectorize()
{
  int i;
  for( i= 0; i < nContours; ++i )
    {
    if( ! vectorizeContour(i) )
      return GLTT_FALSE;
    }

  // remove null contours now
  for( i= 0; i < nContours; ++i )
    {
    Contour* c= contours[i];
    if( c->nPoints < 2 )
      {
      delete c;

      for( int j= i+1; j < nContours; ++j )
        contours[j-1]= contours[j];

      contours[nContours-1]= 0;

      --nContours;
      --i;
      }
    }

  sortContours();

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

// 0 <= c < nContours
GLTTboolean FTGlyphVectorizer::vectorizeContour( int c )
{
  if( c < 0 || c >= nContours || contours == 0 )
    return GLTT_FALSE;

  Contour& contour= *contours[c];

  contour.area= 0.;
  contour.nPoints= 0;

#define BEZIER(x1,y1,x2,y2,x3,y3)                                   \
          {                                                         \
          double extent= max( fabs(max(x1,x2,x3) - min(x1,x2,x3)),  \
                              fabs(max(y1,y2,y3) - min(y1,y2,y3)) );\
          double _subdivisions= extent / precision + .5;            \
          int subdivisions= int(_subdivisions);                     \
          contour.add_point(x1,y1);                                 \
          if( subdivisions > 0 )                                    \
            {                                                       \
            double dt= 1. / double(subdivisions);                   \
            register double t= dt;                                  \
            for( int a= 1; a < subdivisions; ++a, t+= dt )          \
              {                                                     \
              register double tt= 1. - t;                           \
              register double t1= tt * tt;                          \
              register double t2= 2. * t * tt;                      \
              register double t3= t * t;                            \
              register double x= t1 * (x1) + t2 * (x2) + t3 * (x3); \
              register double y= t1 * (y1) + t2 * (y2) + t3 * (y3); \
              contour.add_point(x,y);                               \
              }                                                     \
            }                                                       \
          }

  int first= (c==0) ? 0 : (outline->contours[c-1]+1);
  int last= outline->contours[c];

  int k1= first;
  int k2= k1+1;

  int on1= (outline->flags[k1] & 1);
  int on2= (outline->flags[k2] & 1);

  const double scale= 1. / 64.;
  double x1= double(outline->points[k1].x) * scale;
  double y1= double(outline->points[k1].y) * scale;
  double x2= double(outline->points[k2].x) * scale;
  double y2= double(outline->points[k2].y) * scale;
  int skip_next= 0;

  for( int k= first+1; k <= last; ++k )
    {
    int k3= (k==last) ? first : (k+1);
    int on3= (outline->flags[k3] & 1);
    double x3= double(outline->points[k3].x) * scale;
    double y3= double(outline->points[k3].y) * scale;

    if( ! skip_next )
      {
      if( on1 )
        {
        if( on2 )
          {
          contour.add_point(x1,y1);
          if( k == last )
            contour.add_point(x2,y2);
          }
         else
          {
          if( on3 )
            {
            BEZIER(x1,y1,x2,y2,x3,y3);
            if( k == last-1 )
              contour.add_point(x3,y3);
            skip_next= 1;
            }
           else
            {
            double x23= (x2+x3) * .5;
            double y23= (y2+y3) * .5;
            BEZIER(x1,y1,x2,y2,x23,y23);
            }
          }
        }
       else
        {
        if( on2 )
          {
          }
         else
          {
          if( on3 )
            {
            double x12= (x1+x2) * .5;
            double y12= (y1+y2) * .5;
            BEZIER(x12,y12,x2,y2,x3,y3);
            if( k == last-1 )
              contour.add_point(x3,y3);
            skip_next= 1;
            }
           else
            {
            double x12= (x1+x2) * .5;
            double y12= (y1+y2) * .5;
            double x23= (x2+x3) * .5;
            double y23= (y2+y3) * .5;
            BEZIER(x12,y12,x2,y2,x23,y23);
            }
          }
        }
      }

    k1= k2; k2= k3;
    x1= x2; x2= x3;
    y1= y2; y2= y3;
    on1=on2;on2=on3;
    skip_next= 0;
    }

#undef BEZIER

  if( contour.nPoints >= 2 )
    {
    contour.area += contour.points[contour.nPoints-1].x
                   *contour.points[0                ].y
                  - contour.points[0                ].x
                   *contour.points[contour.nPoints-1].y;
    contour.area *= .5;
    }

  contour.clockwise= contour.area < 0.;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

double FTGlyphVectorizer::getBearingX() const
{
  return (glyph==0) ? 0. : (double(glyph->getBearingX())/64.);
}

/////////////////////////////////////////////////////////////////////////////

double FTGlyphVectorizer::getBearingY() const
{
  return (glyph==0) ? 0. : (double(glyph->getBearingY())/64.);
}

/////////////////////////////////////////////////////////////////////////////

double FTGlyphVectorizer::getAdvance() const
{
  return (glyph==0) ? 0. : (double(glyph->getAdvance())/64.);
}

/////////////////////////////////////////////////////////////////////////////

// What we'd like to have in contour[] is:
//  + first exterior contour
//  +   its interior contours
//  + next exterior contour
//  +   its interior contours
//  etc.
void FTGlyphVectorizer::sortContours()
{
  if( nContours == 0 )
    return;

  Contour** scontours= new Contour* [ nContours ];
  int nSContours= 0;

  for(;;)
    {
    // first, get the first exterior contour
    int iext= -1;
    int i;
    for( i= 0; i < nContours; ++i )
      {
      if( contours[i] == 0 )
        continue;
      if( contours[i]->exterior() )
        {
        iext= i;
        break;
        }
      }

    if( iext == -1 )
      break; // no more exterior contour

    Contour* ext= contours[iext];
    scontours[nSContours++]= ext;
    contours[iext]= 0; // remove it from the list

    // let's find its children interior contours
    for( i= 0; i < nContours; ++i )
      {
      Contour* inte= contours[i];
      if( inte == 0 )
        continue;
      if( inte->exterior() )
        continue;

      // ok, we have an interior contour in *inte
      // Is it _into_ the *ext contour?

      // Check bounding boxes: the *inte bbox must be _into_ the
      // *ext bbox
      if( inte->x_min < ext->x_min || inte->x_max > ext->x_max ||
          inte->y_min < ext->y_min || inte->y_max > ext->y_max )
        continue;

      // ok, let's take the first point of *inte
      double x= inte->points[0].x;
      double y= inte->points[0].y;

      // now, count how many times the half line (-inf -> x, y)
      // intersects the *ext countour
      double x1= ext->points[0].x;
      double y1= ext->points[0].y;
      register double x2= 0.;
      register double y2= 0.;
      int nIntersections= 0;
      for( int j= 0; j < ext->nPoints; ++j, x1= x2, y1= y2 )
        {
        register int j2= j + 1;
        if( j2 == ext->nPoints )
          j2= 0;
        x2= ext->points[j2].x;
        y2= ext->points[j2].y;
        if( (y1 > y && y2 > y) || (y1 < y && y2 < y) )
          continue;
        if( y1 == y2 )
          {
          if( y1 == y && (x1 < x || x2 < x) )
            ++nIntersections;
          continue;
          }
        register double Ix= x1 + (y - y1) * (x2 - x1) / (y2 - y1);
        if( Ix <= x )
          ++nIntersections;
        }

      int into= (nIntersections & 1) != 0;
      if( ! into )
        continue;

      // ok, this contour is _into_ *ext
      // let's append it after exti
      scontours[nSContours++]= inte;
      contours[i]= 0;
      }
    }

  if( nSContours < nContours )
    {
    // oups!! I could not find where were these remaining interior
    // contours!
    // Just append them and cross your fingers

    // Example: glyph="a"="yin-yang symbol" in wingding.ttf
//printf( "glyph = %d (%c)\n", glyph->getAsciiCode(), glyph->getAsciiCode() );
    for( int i= 0; i < nContours; ++i )
      {
      Contour* c= contours[i];
      if( c == 0 )
        continue;
      scontours[nSContours++]= c;
      contours[i]= 0;
      }
    }

  delete[] contours;
  contours= scontours;
}

/////////////////////////////////////////////////////////////////////////////
