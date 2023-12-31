/*$Id: IO.cc,v 1.6 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include "Warsaw/IO.hh"
#include <iomanip>

using namespace Warsaw;

inline bool equal(Coord a, Coord b, Coord e) { return a - b < e && b - a < e;}

std::ostream &operator << (std::ostream &os, const Color &c)
{
  if (c.alpha != 1.) os << '(' << c.red << ',' << c.green << ',' << c.blue << ';' << c.alpha << ')';
  else os << '(' << c.red << ',' << c.green << ',' << c.blue << ')';
  return os;
}

std::ostream &operator << (std::ostream &os, const Graphic::Requirement &r)
{
  if (!r.defined) os << "undef";
  else
    {
      double tol = 1e-2;
      os << std::setiosflags(std::ios::fixed);
      if (equal(r.natural, r.minimum, tol))
	{
	  if (equal(r.natural, r.maximum, tol))
	    os << std::setprecision(2) << r.natural;
	  else
	    os << '(' << std::setprecision(2) << r.natural
	       << ',' << std::setprecision(2) << r.maximum << ')';
	}
      else if (equal(r.natural, r.maximum, tol))
	os << '(' << std::setprecision(2) << r.minimum
	   << ',' << std::setprecision(2) << r.natural << ')';
      else
	os << '(' << std::setprecision(2) << r.minimum
	   << ',' << std::setprecision(2) << r.natural
	   << ',' << std::setprecision(2) << r.maximum << ')';
      if (!equal(r.align, 0., tol))
	os << " @ " << std::setprecision(1) << r.align;
    }
  return os;
};

std::ostream &operator << (std::ostream &os, const Graphic::Requisition &r)
{
  return os << r.x << ", " << r.y << ", " << r.z;
}

std::ostream &operator << (std::ostream &os, const Region::Allotment &a)
{
  os << std::setiosflags(std::ios::fixed) << std::setprecision(2) << a.begin << ',' << std::setprecision(2) << a.end;
  if (!equal(a.align, 0., 1e-2)) os << " @ " << std::setprecision(1) << a.align;
  return os;
}

std::ostream &operator << (std::ostream &os, Region_ptr r)
{
  Region::Allotment a;
  os << "X(";
  r->span(xaxis, a);
  os << a << "), Y(";
  r->span(yaxis, a);
  os << a << "), Z(";
  r->span(zaxis, a);
  os << a << ')';
  return os;
}

std::ostream &operator << (std::ostream &os, const Transform::Matrix &m)
{
  os << '[' << m[0][0] << ',' << m[0][1] << ',' << m[0][2] << ',' << m[0][3] << "]\n"
     << '[' << m[1][0] << ',' << m[1][1] << ',' << m[1][2] << ',' << m[1][3] << "]\n"
     << '[' << m[2][0] << ',' << m[2][1] << ',' << m[2][2] << ',' << m[2][3] << "]\n"
     << '[' << m[3][0] << ',' << m[3][1] << ',' << m[3][2] << ',' << m[3][3] << "]\n";
  return os;
};

std::ostream &operator << (std::ostream &os, Transform_ptr transform)
{
  Transform::Matrix matrix;
  transform->store_matrix(matrix);
  return os << matrix;
};
