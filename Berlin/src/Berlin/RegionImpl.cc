/*$Id: RegionImpl.cc,v 1.17 1999/11/06 20:23:08 stefan Exp $
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
#include "Berlin/RegionImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/Logger.hh"

RegionImpl::RegionImpl()
{
  Coord zero = Coord(0.0);
  valid = false;
  lower.x = lower.y = zero;
  upper.x = upper.y = zero;
  lower.z = upper.z = zero;
  xalign = yalign = zalign = zero;
}

RegionImpl::RegionImpl(const RegionImpl &region)
  : valid(region.valid),
    lower(region.lower),
    upper(region.upper),
    xalign(region.xalign),
    yalign(region.yalign),
    zalign(region.zalign)
{}

RegionImpl::RegionImpl(Region_ptr region)
{
  RegionImpl::copy(region);
}

RegionImpl::RegionImpl(Region_ptr region, Transform_ptr transformation)
{
  RegionImpl::copy(region);
  if (!CORBA::is_nil(transformation) && !transformation->Identity())
    RegionImpl::applyTransform(transformation);
}

RegionImpl::~RegionImpl() {}

CORBA::Boolean RegionImpl::defined() { return valid;}

CORBA::Boolean RegionImpl::contains(const Vertex &v)
{
  return (valid &&
	  v.x >= lower.x && v.x <= upper.x &&
	  v.y >= lower.y && v.y <= upper.y &&
	  v.z >= lower.z && v.z <= upper.z
	  );
}

CORBA::Boolean RegionImpl::containsPlane(const Vertex &v, Axis a)
{
  bool b = false;
  if (valid)
    {
      switch (a)
	{
	case xaxis:
	  b = (v.y >= lower.y && v.y <= upper.y &&
	       v.z >= lower.z && v.z <= upper.z);
	  break;
	case yaxis:
	  b = (v.x >= lower.x && v.x <= upper.x &&
	       v.z >= lower.z && v.z <= upper.z);
	  break;
	case zaxis:
	  b = (v.x >= lower.x && v.x <= upper.x &&
	       v.y >= lower.y && v.y <= upper.y);
	  break;
	}
    }
  return b;
}

CORBA::Boolean RegionImpl::intersects(Region_ptr region)
{
  if (valid)
    {
      Vertex l, u;
      region->bounds(l, u);
      return lower.x <= u.x && upper.x >= l.x && lower.y <= u.y && upper.y >= l.y;
    }
  return false;
}

void RegionImpl::copy(Region_ptr region)
{
  if (!CORBA::is_nil(region) && region->defined())
    {
      Region::Allotment x, y, z;
      region->span(xaxis, x);
      region->span(yaxis, y);
      region->span(zaxis, z);
      valid = true;
      lower.x = x.begin;
      lower.y = y.begin;
      lower.z = z.begin;
      upper.x = x.end;
      upper.y = y.end;
      upper.z = z.end;
      xalign = x.align;
      yalign = y.align;
      zalign = z.align;
    }
}

void RegionImpl::mergeIntersect(Region_ptr region)
{
  if (region->defined())
    {
      if (valid)
	{
	  Vertex l, u;
	  region->bounds(l, u);
	  mergeMax(lower, l);
	  mergeMin(upper, u);
        }
      else copy(region);
    }
}

void RegionImpl::mergeUnion(Region_ptr region)
{
  if (region->defined())
    {
      if (valid)
	{
	  Vertex l, u;
	  region->bounds(l, u);
	  mergeMin(lower, l);
	  mergeMax(upper, u);
        }
      else copy(region);
    }
}

void RegionImpl::subtract(Region_ptr region)
{
  // not implemented
}

void RegionImpl::applyTransform(Transform_ptr transformation)
{
  SectionLog section("RegionImpl::applyTransform");
  if (valid)
    {
      Vertex o;

      origin(o);
      transformation->transformVertex(o);
      Transform::Matrix m;
      transformation->storeMatrix(m);

      Coord w = upper.x - lower.x;
      Coord h = upper.y - lower.y;

      Vertex center;
      center.x = Coord((upper.x + lower.x) * 0.5);
      center.y = Coord((upper.y + lower.y) * 0.5);

      // transform the center

      transformation->transformVertex(center);

      // optimized computation of new width and height
      Coord nw = Coord(Math::abs(w * m[0][0]) + Math::abs(h * m[1][0]));
      Coord nh = Coord(Math::abs(w * m[0][1]) + Math::abs(h * m[1][1]));

      // form the new box
      lower.x = Coord(center.x - nw * 0.5);
      upper.x = Coord(center.x + nw * 0.5);
      lower.y = Coord(center.y - nh * 0.5);
      upper.y = Coord(center.y + nh * 0.5);

      if (!Math::equal(nw, Coord(0), 1e-4)) xalign = (o.x - lower.x) / nw;
      if (!Math::equal(nh, Coord(0), 1e-4)) yalign = (o.y - lower.y) / nh;
    }
}

void RegionImpl::bounds(Vertex &l, Vertex &u)
{
  l = lower;
  u = upper;
}

void RegionImpl::center(Vertex &c)
{
  c.x = (lower.x + upper.x) * 0.5;
  c.y = (lower.y + upper.y) * 0.5;
  c.z = 0.0;
}

void RegionImpl::origin(Vertex &v)
{
  v.x = spanOrigin(lower.x, upper.x, xalign);
  v.y = spanOrigin(lower.y, upper.y, yalign);
  v.z = spanOrigin(lower.z, upper.z, zalign);
}

void RegionImpl::span(Axis a, Region::Allotment &s)
{
  switch (a)
    {
    case xaxis:
      s.begin = lower.x;
      s.end = upper.x;
      s.align = xalign;
      break;
    case yaxis:
      s.begin = lower.y;
      s.end = upper.y;
      s.align = yalign;
      break;
    case zaxis:
      s.begin = lower.z;
      s.end = upper.z;
      s.align = zalign;
      break;
    }
}

void RegionImpl::outline(Path *&p)
{
};
