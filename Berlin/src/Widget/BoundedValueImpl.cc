/*$Id: BoundedValueImpl.cc,v 1.12 1999/09/30 17:23:34 gray Exp $
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

#include "Widget/BoundedValueImpl.hh"

using namespace Prague;

BoundedValueImpl::BoundedValueImpl(Coord ll, Coord uu, Coord vv, Coord ss, Coord pp)
  : l(l), u(uu), v(vv), s(ss), p(pp)
{
};

BoundedValueImpl::~BoundedValueImpl()
{
};

Coord BoundedValueImpl::lower()
{
  MutexGuard guard(mutex);
  return l;
}

void BoundedValueImpl::lower(Coord ll)
{
  {
    MutexGuard guard(mutex);
    if (ll == l) return;
    l = ll;
    if (v < l) v = l;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedValueImpl::upper()
{
  MutexGuard guard(mutex);
  return u;
}

void BoundedValueImpl::upper(Coord uu)
{
  {
    MutexGuard guard(mutex);
    if (uu == u) return;
    u = uu;
    if (v > u) v = u;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedValueImpl::step()
{ 
  MutexGuard guard(mutex);
  return s;
}

void BoundedValueImpl::step(Coord ss)
{
  MutexGuard guard(mutex);
  s = ss;
}

Coord BoundedValueImpl::page()
{
  MutexGuard guard(mutex);
  return p;
}

void BoundedValueImpl::page(Coord pp)
{
  MutexGuard guard(mutex);
  p = pp;
}

void BoundedValueImpl::forward()
{
  {
    MutexGuard guard(mutex);
    Coord t = v + s;
    if (t > u) t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedValueImpl::backward()
{
  {
    MutexGuard guard(mutex);
    Coord t = v - s;
    if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedValueImpl::fastforward()
{
  {
    MutexGuard guard(mutex);
    Coord t = v + p;
    if (t > u) t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedValueImpl::fastbackward()
{
  {
    MutexGuard guard(mutex);
    Coord t = v - p;
    if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedValueImpl::begin()
{
  {
    MutexGuard guard(mutex);
    Coord t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}


void BoundedValueImpl::end()
{
  {
    MutexGuard guard(mutex);
    Coord t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}

void BoundedValueImpl::value(Coord vv)
{
  {
    MutexGuard guard(mutex);
    if (vv > u) vv = u;
    else if (vv < l) vv = l;
    if (vv == v) return;
    v = vv;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedValueImpl::value()
{
  MutexGuard guard(mutex);
  return v;
}


void BoundedValueImpl::adjust(Coord d)
{
  {
    MutexGuard guard(mutex);
    Coord t = v + d;
    if (t > u) t = u;
    else if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  notify(any);
}
