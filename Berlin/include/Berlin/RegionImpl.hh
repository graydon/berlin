/*$Id: RegionImpl.hh,v 1.9 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
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
#ifndef _RegionImpl_hh
#define _RegionImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Region.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/Vertex.hh"
#include "Berlin/Math.hh"
#include <iostream>

class RegionImpl : implements(Region)
{
public:
  RegionImpl();
  RegionImpl(const RegionImpl &);
  RegionImpl(Region_ptr);
  RegionImpl(Region_ptr, Transform_ptr);
  virtual ~RegionImpl();

  virtual CORBA::Boolean defined();
  virtual CORBA::Boolean contains(const Vertex &);
  virtual CORBA::Boolean containsPlane(const Vertex &, Axis a);
  virtual CORBA::Boolean intersects(Region_ptr);
  virtual void copy(Region_ptr);
  virtual void mergeIntersect(Region_ptr);
  virtual void mergeUnion(Region_ptr);
  virtual void subtract(Region_ptr);
  virtual void applyTransform(Transform_ptr);
  virtual void bounds(Vertex &, Vertex &);
  virtual void center(Vertex &);
  virtual void origin(Vertex &);
  virtual void span(Axis, Region::Allotment &);
  virtual void outline(Path *&);
public:
  void normalize(Vertex &);
  void normalize(TransformImpl *);
  bool valid;
  Vertex lower, upper;
  Alignment xalign, yalign, zalign;

  static void mergeMin(Vertex &, const Vertex &);
  static void mergeMax(Vertex &, const Vertex &);
  static Coord spanAlign(Coord, Coord, Coord);
  static Coord spanOrigin(Coord, Coord, Coord);
};

inline Coord RegionImpl::spanOrigin(Coord lower, Coord upper, Coord align)
{
  Coord orig;
  if (Math::equal(lower, upper, 1e-4)) orig = Coord(0.0);
  else orig = lower + align * (upper - lower);
  return orig;
}

inline Coord RegionImpl::spanAlign(Coord lower, Coord upper, Coord origin)
{
  Coord s;
  if (Math::equal(lower, upper, 1e-4)) s = Coord(0.0);
  else s = Coord((origin - lower) / (upper - lower));
  return s;
}

inline void RegionImpl::mergeMin(Vertex &v0, const Vertex &v)
{
  v0.x = Math::min(v0.x, v.x);
  v0.y = Math::min(v0.y, v.y);
  v0.z = Math::min(v0.z, v.z);
}

inline void RegionImpl::mergeMax(Vertex &v0, const Vertex &v)
{
  v0.x = Math::max(v0.x, v.x);
  v0.y = Math::max(v0.y, v.y);
  v0.z = Math::max(v0.z, v.z);
}

inline void RegionImpl::normalize(Vertex &o)
{
  o.x = spanOrigin(lower.x, upper.x, xalign);
  o.y = spanOrigin(lower.y, upper.y, yalign);
  o.z = spanOrigin(lower.z, upper.z, zalign);
  lower -= o;
  upper -= o;
}

inline void RegionImpl::normalize(TransformImpl *t)
{
  Vertex o;
  normalize(o);
  t->translate(o);
}

inline ostream &operator << (ostream &os, const RegionImpl &region)
{
  return os << "[(" << region.lower.x << ',' << region.lower.y << ',' << region.lower.z << ')'
	    << ",(" << region.upper.x << ',' << region.upper.y << ',' << region.upper.z << ")]";
}

#endif /* _RegionImpl_hh */
