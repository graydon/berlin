/*$Id: BoundedValueImpl.cc,v 1.5 2001/02/06 19:46:16 tobias Exp $
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

#include "Command/BoundedValueImpl.hh"

using namespace Prague;
using namespace Warsaw;

BoundedValueImpl::BoundedValueImpl(Coord ll, Coord uu, Coord vv, Coord ss, Coord pp)
  : l(ll), u(uu), v(vv), s(ss), p(pp)
{
};

BoundedValueImpl::~BoundedValueImpl()
{
};

Coord BoundedValueImpl::lower()
{
  Prague::Guard<Mutex> guard(mutex);
  return l;
}

void BoundedValueImpl::lower(Coord ll)
{
  {
    Prague::Guard<Mutex> guard(mutex);
    if (ll == l) return;
    l = ll;
    if (v < l) v = l;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedValueImpl::upper()
{
  Prague::Guard<Mutex> guard(mutex);
  return u;
}

void BoundedValueImpl::upper(Coord uu)
{
  {
    Prague::Guard<Mutex> guard(mutex);
    if (uu == u) return;
    u = uu;
    if (v > u) v = u;
  }
  CORBA::Any any;
  notify(any);
}

Coord BoundedValueImpl::step()
{ 
  Prague::Guard<Mutex> guard(mutex);
  return s;
}

void BoundedValueImpl::step(Coord ss)
{
  Prague::Guard<Mutex> guard(mutex);
  s = ss;
}

Coord BoundedValueImpl::page()
{
  Prague::Guard<Mutex> guard(mutex);
  return p;
}

void BoundedValueImpl::page(Coord pp)
{
  Prague::Guard<Mutex> guard(mutex);
  p = pp;
}

void BoundedValueImpl::forward()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = v + s;
    if (t > u) t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

void BoundedValueImpl::backward()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = v - s;
    if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

void BoundedValueImpl::fastforward()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = v + p;
    if (t > u) t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

void BoundedValueImpl::fastbackward()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = v - p;
    if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

void BoundedValueImpl::begin()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}


void BoundedValueImpl::end()
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = u;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

void BoundedValueImpl::value(Coord vv)
{
  {
    Prague::Guard<Mutex> guard(mutex);
    if (vv > u) vv = u;
    else if (vv < l) vv = l;
    if (vv == v) return;
    v = vv;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}

Coord BoundedValueImpl::value()
{
  Prague::Guard<Mutex> guard(mutex);
  return v;
}


void BoundedValueImpl::adjust(Coord d)
{
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = v + d;
    if (t > u) t = u;
    else if (t < l) t = l;
    if (t == v) return;
    v = t;
  }
  CORBA::Any any;
  any <<= v;
  notify(any);
}
