/*$Id: Warsaw.cc,v 1.2 1999/10/30 17:33:10 gray Exp $
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

#include "Warsaw.hh"
#include <iomanip>
/*
 * sorry, quick hack...
 * we need to set up libraries correctly...
 */
#include "../Berlin/Math.hh"

ostream &operator << (ostream &os, const Graphic::Requirement &r)
{
  if (!r.defined) os << "undef";
  else
    {
      double tol = 1e-2;
      os << setiosflags(ios::fixed);
      if (Math::equal(r.natural, r.minimum, tol))
	{
	  if (Math::equal(r.natural, r.maximum, tol))
	    os << setprecision(2) << r.natural;
	  else
	    os << '(' << setprecision(2) << r.natural
	       << ',' << setprecision(2) << r.maximum << ')';
	}
      else if (Math::equal(r.natural, r.maximum, tol))
	os << '(' << setprecision(2) << r.minimum
	   << ',' << setprecision(2) << r.natural << ')';
      else
	os << '(' << setprecision(2) << r.minimum
	   << ',' << setprecision(2) << r.natural
	   << ',' << setprecision(2) << r.maximum << ')';
      if (!Math::equal(r.align, 0., tol))
	os << " @ " << setprecision(1) << r.align;
    }
  return os;
};

ostream &operator << (ostream &os, const Graphic::Requisition &r)
{
  return os << r.x << ", " << r.y << ", " << r.z;
}

ostream &operator << (ostream &os, const Region::Allotment &a)
{
  os << setiosflags(ios::fixed) << setprecision(2) << a.begin << ',' << setprecision(2) << a.end;
  if (!Math::equal(a.align, 0., 1e-2)) cout << " @ " << setprecision(1) << a.align;
  return os;
}

ostream &operator << (ostream &os, Region_ptr r)
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
