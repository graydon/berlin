/*$Id: TelltaleImpl.cc,v 1.8 2001/04/18 06:07:27 stefan Exp $
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
#include "Command/TelltaleImpl.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

TelltaleImpl::TelltaleImpl(TelltaleConstraint_ptr c, unsigned long m)
  : _mask(m), _constraint(c)
{}

TelltaleImpl::~TelltaleImpl()
{}

void TelltaleImpl::set(Warsaw::Telltale::Mask m)
{
  Trace trace("TelltaleImpl::set");
  if (!CORBA::is_nil(_constraint)) _constraint->trymodify(Telltale_var(_this()), m, true);
  else modify(m, true);
}

void TelltaleImpl::clear(Warsaw::Telltale::Mask m)
{
  Trace trace("TelltaleImpl::clear");
  if (!CORBA::is_nil(_constraint)) _constraint->trymodify(Telltale_var(_this()), m, false);
  else modify(m, false);
}

CORBA::Boolean TelltaleImpl::test(Warsaw::Telltale::Mask m)
{
  Prague::Guard<Mutex> guard(_mutex);
  return (_mask & m) == m;
}

void TelltaleImpl::modify(Warsaw::Telltale::Mask m, CORBA::Boolean on)
{
  unsigned long nf = on ? _mask | m : _mask & ~m;
  {
    Prague::Guard<Mutex> guard(_mutex);
    if (nf == _mask) return;
    else _mask = nf;
  }
  CORBA::Any any;
  any <<= nf;
  notify(any);
}

void TelltaleImpl::constraint(TelltaleConstraint_ptr constraint)
{
  Prague::Guard<Mutex> guard(_mutex);
  _constraint = constraint;
}


TelltaleConstraint_ptr TelltaleImpl::constraint()
{
  Prague::Guard<Mutex> guard(_mutex);
  return TelltaleConstraint::_duplicate(_constraint);
}

void TelltaleConstraintImpl::add(Telltale_ptr t)
{
  Prague::Guard<Mutex> guard(_mutex);
  _telltales.push_back(Telltale::_duplicate(t));
  t->constraint(TelltaleConstraint_var(_this()));
}

void TelltaleConstraintImpl::remove(Telltale_ptr t)
{
  Prague::Guard<Mutex> guard(_mutex);
  for (tlist_t::iterator i = _telltales.begin(); i != _telltales.end(); ++i)
    if ((*i) == t)
      {
	_telltales.erase(i);
	break;
      }
}

ExclusiveChoice::ExclusiveChoice(Warsaw::Telltale::Mask m)
  : _mask(m)
{}

void ExclusiveChoice::trymodify(Telltale_ptr t, Warsaw::Telltale::Mask m, CORBA::Boolean b)
{
  Prague::Guard<Mutex> guard(_mutex);
  if (b)
    for (tlist_t::iterator i = _telltales.begin(); i != _telltales.end(); ++i)
      if ((*i)->test(m)) (*i)->modify(m, false);
  t->modify(m, b);
}

SelectionRequired::SelectionRequired(Warsaw::Telltale::Mask m)
  : _mask(m)
{
}

void SelectionRequired::trymodify(Telltale_ptr t, Warsaw::Telltale::Mask m, CORBA::Boolean b)
{
  Prague::Guard<Mutex> guard(_mutex);
  size_t selected = 0;
  if (!b)
    for (tlist_t::iterator i = _telltales.begin(); i != _telltales.end(); ++i)
      if ((*i)->test(m)) selected++;
  if (b || selected > 1) t->modify(m, b);
}

ExclusiveRequired::ExclusiveRequired(Warsaw::Telltale::Mask m)
  : _mask(m)
{}

void ExclusiveRequired::trymodify(Telltale_ptr t, Warsaw::Telltale::Mask m, CORBA::Boolean b)
{
  Prague::Guard<Mutex> guard(_mutex);
  if (b)
    {
      for (tlist_t::iterator i = _telltales.begin(); i != _telltales.end(); ++i)
	if ((*i)->test(m)) (*i)->modify(m, false);
      t->modify(m, true);
    }
  else
    {
      size_t selected = 0;
      for (tlist_t::iterator i = _telltales.begin(); i != _telltales.end(); ++i)
	if ((*i)->test(m)) selected++;
      if (selected > 1) t->modify(m, false);
    }
}

