/*$Id: Math.hh,v 1.2 1999/04/16 16:32:04 gray Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _Math_hh
#define _Math_hh

#include <algorithm>
#include <cmath>

class Math
{
public:
  static const double pi = M_PI;
  template <class T> static T min(T a, T b) { return a < b ? a : b;}
  template <class T> static T max(T a, T b) { return a < b ? b : a;}

  template <class T> static T min(T a, T b, const T &c, const T &d) { return min(min(a, b), min(c, d));}
  template <class T> static T max(T a, T b, const T &c, const T &d) { return max(max(a, b), max(c, d));}

  static float abs(float a) { return fabs(a);}
  static double abs(double a) { return fabs(a);}
  static int abs(int a) { return abs(a);}
  static long abs(long a) { return abs(a);}

  template <class T> static int round(T a) { return a > 0 ? static_cast<int>(a + 0.5) : - static_cast<int>(-a + 0.5);}

  template <class T> static bool equal(T a, T b, T e) { return a - b < e && b - a < e;}
};

#endif /* _Math_hh */
