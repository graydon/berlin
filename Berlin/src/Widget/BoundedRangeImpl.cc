/*$Id: BoundedRangeImpl.cc,v 1.8 1999/09/30 17:23:34 gray Exp $
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

#include "Widget/BoundedRangeImpl.hh"

using namespace Prague;

BoundedRangeImpl::BoundedRangeImpl(Coord ll, Coord uu, Coord lvv, Coord uvv, Coord ss, Coord pp)
  : l(l), u(uu), lv(lvv), uv(uvv), s(ss), p(pp)
{
};

BoundedRangeImpl::~BoundedRangeImpl()
{
};

Coord BoundedRangeImpl::lower()
{
  MutexGuard guard(mutex);
  return l;
}

void BoundedRangeImpl::lower(Coord ll)
{
  {
    MutexGuard guard(mutex);
    if (ll == l) return;
    l = ll;
    if (lv < l) lv = l;
    if (uv < l) uv = l;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedRangeImpl::upper()
{
  MutexGuard guard(mutex);
  return u;
}

void BoundedRangeImpl::upper(Coord uu)
{
  {
    MutexGuard guard(mutex);
    if (uu == u) return;
    u = uu;
    if (lv > u) lv = u;
    if (uv > u) uv = u;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedRangeImpl::step()
{
  MutexGuard guard(mutex);
  return s;
}

void BoundedRangeImpl::step(Coord ss)
{
  MutexGuard guard(mutex);
  s = ss;
}

Coord BoundedRangeImpl::page()
{
  MutexGuard guard(mutex);
  return p;
}

void BoundedRangeImpl::page(Coord pp)
{
  MutexGuard guard(mutex);
  p = pp;
}

void BoundedRangeImpl::forward()
{
  {
    MutexGuard guard(mutex);
    Coord t = uv + s > u ? u - uv : s;
    if (t <= 0.) return;
    lv += t;
    uv += t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::backward()
{
  {
    MutexGuard guard(mutex);
    Coord t = lv - s < l ? lv - l : s;
    if (t <= 0.) return;
    lv -= t;
    uv -= t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::fastforward()
{
  {
    MutexGuard guard(mutex);
    Coord t = uv + p > u ? u - uv : p;
    if (t <= 0.) return;
    lv += t;
    uv += t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::fastbackward()
{
  {
    MutexGuard guard(mutex);
    Coord t = lv - p < l ? lv - l : p;
    if (t <= 0.) return;
    lv -= t;
    uv -= t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::begin()
{
  {
    MutexGuard guard(mutex);
    Coord t = lv - l;
    if (t == 0.) return;
    lv -= t;
    uv -= t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::end()
{
  {
    MutexGuard guard(mutex);
    Coord t = u - uv;
    if (t == 0.) return;
    lv += t;
    uv += t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedRangeImpl::lvalue(Coord vv)
{
  {
    MutexGuard guard(mutex);
    if (vv > u) vv = u;
    else if (vv < l) vv = l;
    if (vv == lv) return;
    lv = vv;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedRangeImpl::lvalue()
{
  MutexGuard guard(mutex);
  return lv;
}


void BoundedRangeImpl::uvalue(Coord vv)
{
  {
    MutexGuard guard(mutex);
    if (vv > u) vv = u;
    else if (vv < l) vv = l;
    if (vv == uv) return;
    uv = vv;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedRangeImpl::uvalue()
{
  MutexGuard guard(mutex);
  return uv;
}


void BoundedRangeImpl::adjust(Coord d)
{
  {
    MutexGuard guard(mutex);
    Coord t =
      uv + d > u ? u - uv :
      lv + d < l ? lv - l : d;
    if (t == 0.) return;
    lv += t;
    uv += t;
  }
  CORBA::Any any;
  notify(any);
}
