/*$Id: Vertex.hh,v 1.4 1999/09/15 20:52:18 gray Exp $
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
#ifndef _Vertex_hh
#define _Vertex_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <iostream>

inline Vertex &operator += (Vertex &p, const Vertex &q)
{
  p.x += q.x;
  p.y += q.y;
  p.z += q.z;
  return p;
}

inline Vertex &operator -= (Vertex &p, const Vertex &q)
{
  p.x -= q.x;
  p.y -= q.y;
  p.z -= q.z;
  return p;
}

inline Vertex operator + (const Vertex &p, const Vertex &q)
{
  Vertex r;
  r.x = p.x + q.x;
  r.y = p.y + q.y;
  r.z = p.z + q.z;
  return r;
}

inline Vertex operator - (const Vertex &p, const Vertex &q)
{
  Vertex r;
  r.x = p.x - q.x;
  r.y = p.y - q.y;
  r.z = p.z - q.z;
  return r;
}

inline ostream &operator << (ostream &os, const Vertex &v) { return os << '(' << v.x << ',' << v.y << ',' << v.z << ')';}

#endif /* _Vertex_hh */
