/*$Id: GLQuadric.hh,v 1.3 2000/08/31 18:52:32 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _GLQuadric_hh
#define _GLQuadric_hh

#include <Warsaw/config.hh>
#include <Warsaw/DrawingKit.hh>

class GLQuadric
{
public:
  enum orientation {in, out};
  enum normals {none, flat, smooth};
  GLQuadric(Warsaw::DrawingKit::Fillstyle s, orientation o, normals n = none) : style(s), orient(o), norm(n) {}
  ~GLQuadric() {}
  void cylinder(double, double, double, int, int);
  void sphere(double, int, int);
  void disk(double, double, int, int);
  void partialDisk(double, double, int, int, double, double);
private:
  Warsaw::DrawingKit::Fillstyle style;
  orientation orient;
  normals norm;
};

#endif 
