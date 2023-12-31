/*$Id: RegionImpl.hh,v 1.18 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _RegionImpl_hh
#define _RegionImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Region.hh>
#include <Berlin/ServantBase.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/Vertex.hh>
#include <Berlin/Math.hh>
#include <Berlin/Provider.hh>
#include <iostream>

class RegionImpl : public virtual POA_Warsaw::Region,
                   public virtual ServantBase
{
  friend class Provider<RegionImpl>;
public:
  RegionImpl();
  RegionImpl(const RegionImpl &);
  RegionImpl(Warsaw::Region_ptr);
  RegionImpl(Warsaw::Region_ptr, Warsaw::Transform_ptr);
  virtual ~RegionImpl();
  RegionImpl &operator = (const RegionImpl &);
  virtual CORBA::Boolean defined();
  virtual CORBA::Boolean contains(const Warsaw::Vertex &);
  virtual CORBA::Boolean contains_plane(const Warsaw::Vertex &, Warsaw::Axis a);
  virtual CORBA::Boolean intersects(Warsaw::Region_ptr);
  virtual void copy(Warsaw::Region_ptr);
  virtual void merge_intersect(Warsaw::Region_ptr);
  virtual void merge_union(Warsaw::Region_ptr);
  virtual void subtract(Warsaw::Region_ptr);
  virtual void apply_transform(Warsaw::Transform_ptr);
  virtual void bounds(Warsaw::Vertex &, Warsaw::Vertex &);
  virtual void center(Warsaw::Vertex &);
  virtual void origin(Warsaw::Vertex &);
  virtual void span(Warsaw::Axis, Warsaw::Region::Allotment &);
  virtual void outline(Warsaw::Path_out);

  void clear();

  Warsaw::Region_ptr _this ()
  {
    if (!_this_valid)
      {
	__this = POA_Warsaw::Region::_this();
	_this_valid = true;
      }
    return Warsaw::Region::_duplicate (__this);
  }

public:
  void normalize(Warsaw::Vertex &);
  void normalize(Warsaw::Transform_ptr);
  bool valid;
  Warsaw::Vertex lower, upper;
  Warsaw::Alignment xalign, yalign, zalign;

  static void merge_min(Warsaw::Vertex &, const Warsaw::Vertex &);
  static void merge_max(Warsaw::Vertex &, const Warsaw::Vertex &);
  static Warsaw::Coord span_align(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  static Warsaw::Coord span_origin(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);

private:
  bool _active : 1;
  bool _this_valid;
  Warsaw::Region_var __this;
};

inline Warsaw::Coord RegionImpl::span_origin(Warsaw::Coord lower, Warsaw::Coord upper, Warsaw::Coord align)
{
  Warsaw::Coord orig;
  if (Math::equal(lower, upper, 1e-4)) orig = 0.;
  else orig = lower + align * (upper - lower);
  return orig;
}

inline Warsaw::Coord RegionImpl::span_align(Warsaw::Coord lower, Warsaw::Coord upper, Warsaw::Coord origin)
{
  Warsaw::Coord s;
  if (Math::equal(lower, upper, 1e-4)) s = 0.;
  else s = (origin - lower) / (upper - lower);
  return s;
}

inline void RegionImpl::merge_min(Warsaw::Vertex &v0, const Warsaw::Vertex &v)
{
  v0.x = Math::min(v0.x, v.x);
  v0.y = Math::min(v0.y, v.y);
  v0.z = Math::min(v0.z, v.z);
}

inline void RegionImpl::merge_max(Warsaw::Vertex &v0, const Warsaw::Vertex &v)
{
  v0.x = Math::max(v0.x, v.x);
  v0.y = Math::max(v0.y, v.y);
  v0.z = Math::max(v0.z, v.z);
}

inline void RegionImpl::normalize(Warsaw::Vertex &o)
{
  o.x = span_origin(lower.x, upper.x, xalign);
  o.y = span_origin(lower.y, upper.y, yalign);
  o.z = span_origin(lower.z, upper.z, zalign);
  lower -= o;
  upper -= o;
}

inline void RegionImpl::normalize(Warsaw::Transform_ptr t)
{
  Warsaw::Vertex o;
  normalize(o);
  if (!CORBA::is_nil(t)) t->translate(o);
}

std::ostream &operator << (std::ostream &, const RegionImpl &);

#endif
