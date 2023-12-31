/*$Id: RegionImpl.cc,v 1.27 2001/04/18 06:07:27 stefan Exp $
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
#include "Berlin/RegionImpl.hh"
#include "Berlin/TransformImpl.hh"
#include <iomanip>
#include <cassert>

using namespace Warsaw;

RegionImpl::RegionImpl()
  : valid(false), xalign(0.), yalign(0.), zalign(0.), _this_valid(false)
{
  lower.x = lower.y = 0.;
  upper.x = upper.y = 0.;
  lower.z = upper.z = 0.;
}

RegionImpl::RegionImpl(const RegionImpl &region)
  : valid(region.valid),
    lower(region.lower),
    upper(region.upper),
    xalign(region.xalign),
    yalign(region.yalign),
    zalign(region.zalign),
    _this_valid(false)
{}

RegionImpl::RegionImpl(Region_ptr region)
  : _this_valid(false)
{
  RegionImpl::copy(region);
}

RegionImpl::RegionImpl(Region_ptr region, Transform_ptr transformation)
  : _this_valid(false)
{
  RegionImpl::copy(region);
  if (!CORBA::is_nil(transformation) && !transformation->identity())
    RegionImpl::apply_transform(transformation);
}

RegionImpl::~RegionImpl() {}

RegionImpl &RegionImpl::operator = (const RegionImpl &region)
{
  assert(_active);
  valid = region.valid;
  lower = region.lower;
  upper = region.upper;
}

CORBA::Boolean RegionImpl::defined() { return valid;}
void RegionImpl::clear() { valid = false;}

CORBA::Boolean RegionImpl::contains(const Vertex &v)
{
  return (valid &&
	  v.x >= lower.x && v.x <= upper.x &&
	  v.y >= lower.y && v.y <= upper.y &&
	  v.z >= lower.z && v.z <= upper.z
	  );
}

CORBA::Boolean RegionImpl::contains_plane(const Vertex &v, Axis a)
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
      Warsaw::Region::Allotment x, y, z;
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

void RegionImpl::merge_intersect(Region_ptr region)
{
  if (region->defined())
    {
      if (valid)
	{
	  Vertex l, u;
	  region->bounds(l, u);
	  merge_max(lower, l);
	  merge_min(upper, u);
        }
      else copy(region);
    }
}

void RegionImpl::merge_union(Region_ptr region)
{
  if (region->defined())
    {
      if (valid)
	{
	  Vertex l, u;
	  region->bounds(l, u);
	  merge_min(lower, l);
	  merge_max(upper, u);
        }
      else copy(region);
    }
}

void RegionImpl::subtract(Region_ptr region)
{
  // not implemented
}

/*
 * note: the new region is the bounding box of the transformed
 *       old region
 *
 * This algorithm takes advantage of some
 * interesting properties of affine transformations: i.e., opposite sides
 * are always parallel and same in length.  Another property of
 * affine transformation is that the transformed center of a box
 * is equal to the center of the transformed box.  Realizing these
 * properties, finding the transformed box can be broken down as finding
 * the effective width, height, depth, and center.  The last is easy.
 * Each of the other three values is found by first breaking down
 * each of the transformed axis vectors into x, y,  and z vectors.
 * Joining all the absolute vectors on a certain axis gives you
 * the size of the box on that axis.  Width, for example,
 * forms a (w, 0, 0) vector.  After transformation, it becomes
 * (Xw, Yw, Zw). The new width is then given as abs(Xw) + abs(Xh) + abs(Xd)
 */
void RegionImpl::apply_transform(Transform_ptr transformation)
{
  if (valid)
    {
      Vertex o;

      origin(o);
      transformation->transform_vertex(o);
      Transform::Matrix m;
      transformation->store_matrix(m);

      Coord lx = upper.x - lower.x;
      Coord ly = upper.y - lower.y;
      Coord lz = upper.z - lower.z;

      Vertex center;
      center.x = (upper.x + lower.x) * 0.5;
      center.y = (upper.y + lower.y) * 0.5;
      center.z = (upper.z + lower.z) * 0.5;

      // transform the center

      transformation->transform_vertex(center);

      // optimized computation of new width and height
      Coord nlx = Math::abs(lx * m[0][0]) + Math::abs(ly * m[0][1]) + Math::abs(lz * m[0][2]);
      Coord nly = Math::abs(lx * m[1][0]) + Math::abs(ly * m[1][1]) + Math::abs(lz * m[1][2]);
      Coord nlz = Math::abs(lx * m[2][0]) + Math::abs(ly * m[2][1]) + Math::abs(lz * m[2][2]);

      // form the new box
      lower.x = center.x - nlx * 0.5;
      upper.x = center.x + nlx * 0.5;
      lower.y = center.y - nly * 0.5;
      upper.y = center.y + nly * 0.5;
      lower.z = center.z - nlz * 0.5;
      upper.z = center.z + nlz * 0.5;

      if (!Math::equal(nlx, 0., 1e-4)) xalign = (o.x - lower.x) / nlx;
      if (!Math::equal(nly, 0., 1e-4)) yalign = (o.y - lower.y) / nly;
      if (!Math::equal(nlz, 0., 1e-4)) zalign = (o.z - lower.z) / nlz;
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
  v.x = span_origin(lower.x, upper.x, xalign);
  v.y = span_origin(lower.y, upper.y, yalign);
  v.z = span_origin(lower.z, upper.z, zalign);
}

void RegionImpl::span(Axis a, Warsaw::Region::Allotment &s)
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

void RegionImpl::outline(Path_out)
{
};

std::ostream &operator << (std::ostream &os, const RegionImpl &region)
{
  os << "X(" << region.lower.x << ',' << region.upper.x;
  if (!Math::equal(region.xalign, 0., 1e-2)) os << " @ " << std::setprecision(1) << region.xalign;
  os << "), Y(" << region.lower.y << ',' << region.upper.y;
  if (!Math::equal(region.yalign, 0., 1e-2)) os << " @ " << std::setprecision(1) << region.yalign;
  os << "), Z(" << region.lower.z << ',' << region.upper.z;
  if (!Math::equal(region.zalign, 0., 1e-2)) os << " @ " << std::setprecision(1) << region.zalign;
  os << ')';
  return os;
}
