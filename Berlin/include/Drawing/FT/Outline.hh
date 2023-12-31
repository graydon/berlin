/*$Id: Outline.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
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
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */
#ifndef _FT_Outline_hh
#define _FT_Outline_hh

#include <Drawing/FT/Glyph.hh>
#include <vector>

namespace FT
{

class Outline
{
public:
  struct Point
  {
    Point() : x(0), y(0), data(0) {}
    Point(double xx, double yy) : x(xx), y(yy), data(0) {}
    double x, y;
    void* data;
  };
  struct Contour
  {
    typedef vector<Point>::const_iterator iterator;
    Contour() : area(0), xmin(1e20), xmax(-1e20), ymin(1e20), ymax(-1e20) {}
    void add(double, double);
    void bezier(double x1, double y1, double x2, double y2, double x3, double y3, double precision);
    bool exterior() const { return area < 0.;}
    vector<Point> points;
    double area;
    double xmin, xmax;
    double ymin, ymax;
  };
  typedef vector<Contour> clist_t;
public:
  Outline(const Glyph &);
  bool vectorize(double);
  const clist_t &contours() const { return c;}
  double bearingX() const { return glyph.metrics().bearingX/64.;}
  double bearingY() const { return glyph.metrics().bearingY/64.;}
  double advance() const { return glyph.metrics().advance/64.;}
private:
  const Glyph &glyph;
  TT_Outline outline;
  clist_t c;
  bool vectorizeContour(size_t, double);
  void sortContours();
};

};

#endif /* _FT_Outline_hh */
