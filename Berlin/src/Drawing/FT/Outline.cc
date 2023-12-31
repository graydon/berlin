/*$Id: Outline.cc,v 1.1 1999/12/13 21:14:34 stefan Exp $
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

#include "Drawing/FT/Outline.hh"
#include <cmath>
#include <cstring>

using namespace FT;

inline double min(double d1, double d2, double d3) { return min(min(d1, d2), d3);}
inline double max(double d1, double d2, double d3) { return max(max(d1, d2), d3);}

void Outline::Contour::add(double x, double y)
{
  Point current(x, y);
  if (points.size())
    {
      Point &last = points.back();
      if (abs(current.x - last.x) < 1e-8 && abs(current.y - last.y) < 1e-8) return;
      area += last.x * current.y - current.x * last.y;
    }
  points.push_back(current);
  xmin = min(x, xmin);
  xmax = max(x, xmax);
  ymin = min(y, ymin);
  ymax = max(y, ymax);
}

inline void Outline::Contour::bezier(double x1, double y1, double x2, double y2, double x3, double y3, double precision)
{
  double extent = max(abs(max(x1,x2,x3) - min(x1,x2,x3)), abs(max(y1,y2,y3) - min(y1,y2,y3)));
  int subdivisions = static_cast<int>(extent / precision + .5);
  add(x1, y1);
  if(subdivisions > 0)
    {
      double dt = 1. / subdivisions;
      double t = dt;
      for(int a = 1; a < subdivisions; ++a, t+= dt)
	{
	  double tt = 1. - t;
	  double t1 = tt * tt;
	  double t2 = 2. * t * tt;
	  double t3 = t * t;
	  double x = t1 * x1 + t2 * x2 + t3 * x3;
	  double y = t1 * y1 + t2 * y2 + t3 * y3;
	  add(x,y);
	}
    }
}

Outline::Outline(const Glyph &g)
  : glyph(g)
{
  if (TT_Get_Glyph_Outline(glyph, &outline)) throw exception();
  c.resize(outline.n_contours);
}

bool Outline::vectorize(double precision)
{
  for(size_t i = 0; i < c.size(); i++)
    {
      if(!vectorizeContour(i, precision)) return false;
    }
  // remove null contours now
  for(int i = c.size() - 1; i >= 0; i--)
    if(c[i].points.size() < 2) c.erase(c.begin() + i);
  sortContours();
  return true;
}

bool Outline::vectorizeContour(size_t i, double precision)
{
  Contour &contour = c[i];

  contour.area = 0.;
  contour.points.resize(0);

  int first = i ? outline.contours[i-1] + 1 : 0;
  int last = outline.contours[i];

  int k1 = first;
  int k2 = k1 + 1;

  int on1 = outline.flags[k1] & 1;
  int on2 = outline.flags[k2] & 1;

  const double scale = 1. / 64.;
  double x1 = outline.points[k1].x * scale;
  double y1 = outline.points[k1].y * scale;
  double x2 = outline.points[k2].x * scale;
  double y2 = outline.points[k2].y * scale;
  int skip_next = 0;

  for(int k = first + 1; k <= last; k++)
    {
      int k3 = k == last ? first : k + 1;
      int on3 = outline.flags[k3] & 1;
      double x3 = outline.points[k3].x * scale;
      double y3 = outline.points[k3].y * scale;
      if(!skip_next)
	{
	  if(on1)
	    {
	      if(on2)
		{
		  contour.add(x1, y1);
		  if(k == last) contour.add(x2, y2);
		}
	      else
		{
		  if(on3)
		    {
		      contour.bezier(x1, y1, x2, y2, x3, y3, precision);
		      if (k == last - 1) contour.add(x3, y3);
		      skip_next = 1;
		    }
		  else
		    {
		      double x23 = (x2 + x3) * .5;
		      double y23 = (y2 + y3) * .5;
		      contour.bezier(x1, y1, x2, y2, x23, y23, precision);
		    }
		}
	    }
	  else
	    {
	      if(on2);
	      else
		{
		  if(on3)
		    {
		      double x12 = (x1 + x2) * .5;
		      double y12 = (y1 + y2) * .5;
		      contour.bezier(x12, y12, x2, y2, x3, y3, precision);
		      if (k == last - 1) contour.add(x3, y3);
		      skip_next = 1;
		    }
		  else
		    {
		      double x12 = (x1 + x2) * .5;
		      double y12 = (y1 + y2) * .5;
		      double x23 = (x2 + x3) * .5;
		      double y23 = (y2 + y3) * .5;
		      contour.bezier(x12, y12, x2, y2, x23, y23, precision);
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
  if(contour.points.size() >= 2)
    {
      contour.area += (contour.points.back().x * contour.points.front().y -
		       contour.points.front().x * contour.points.back().y);
      contour.area *= .5;
    }
  return true;
}

/*
 * What we'd like to have in contours is:
 *  + first exterior contour
 *  +   its interior contours
 *  + next exterior contour
 *  +   its interior contours
 *  etc.
 */
void Outline::sortContours()
{
  if(!c.size()) return;
  clist_t old = c;
  size_t count = 0;
  while (true)
    {
    // first, get the first exterior contour
      int iext = -1;
      for (size_t i = 0; i < old.size(); i++)
	{
	  if(old[i].exterior())
	    {
	      iext = i;
	      break;
	    }
	}

      if (iext == -1) break; // no more exterior contour

      c[count] = old[iext];
      old.erase(old.begin() + iext);
      Contour *exterior = &c[count++];
      size_t i = 0;
      while (i < old.size())
	{
	  Contour *interior = &old[i];
	  if (interior->exterior() || // ok, we have an interior contour. Is it within the exterior contour?
	      interior->xmin < exterior->xmin || interior->xmax > exterior->xmax ||   // Check bounding boxes: the interior bbox 
	      interior->ymin < exterior->ymin || interior->ymax > exterior->ymax)     // must be within the exterior bbox
	    {
	      i++;
	      continue;
	    }
	  // ok, let's take the first point of interior
	  double x = interior->points[0].x;
	  double y = interior->points[0].y;

	  // now, count how many times the half line (-inf -> x, y)
	  // intersects the exterior countour
	  double x1 = exterior->points[0].x;
	  double y1 = exterior->points[0].y;
	  double x2 = 0.;
	  double y2 = 0.;
	  int intersections = 0;
	  for (size_t j = 0; j < exterior->points.size(); j++, x1 = x2, y1 = y2)
	    {
	      size_t j2 = j + 1;
	      if(j2 == exterior->points.size()) j2 = 0;
	      x2 = exterior->points[j2].x;
	      y2 = exterior->points[j2].y;
	      if ((y1 > y && y2 > y) || (y1 < y && y2 < y)) continue;
	      if(y1 == y2)
		{
		  if (y1 == y && (x1 < x || x2 < x)) intersections++;
		  continue;
		}
	      double ix = x1 + (y - y1) * (x2 - x1) / (y2 - y1);
	      if (ix <= x) intersections++;
	    }
	  if (intersections & 1)
	    // ok, this contour is within exterior
	    // let's append it after exterior
	    {
	      c[count++]= *interior;
	      old.erase(old.begin() + i);
	    }
	  else i++;
	}
    }

  if(count < c.size())
    {
      // oups!! I could not find where were these remaining interior
      // contours!
      // Just append them and cross your fingers

      // Example: glyph="a"="yin-yang symbol" in wingding.ttf
      //printf( "glyph = %d (%c)\n", glyph->getAsciiCode(), glyph->getAsciiCode() );
      for(size_t i= 0; i < old.size(); i++) c[count++]= old[i];
    }
}
