/*$Id: Unicode.cc,v 1.1 1999/11/29 01:20:25 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Tobias Hunger <Tobias_Hunger@gmx.de>
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

// This headerfile is necessary since Prague does not know about
// the Unistring (defined in Warsaw) that CORBA needs.

#include <Warsaw/Unicode.hh>

// This is ugly but necessary since Unistring does not
// know iterators! So I can´t do decent assigment
// operators :-(
Unistring Unicode::toCORBA(const Unicode::String &s)
{
  Unistring tmp;
  tmp.length(s.length());
  for (unsigned long i = 0; i < s.length(); i++)
    tmp[i] = s[i].myUnicode();
  return tmp;
}

Unichar   Unicode::toCORBA(const Unicode::Char &c)
{
  Unichar tmp = c.myUnicode();
  return tmp;
}

Unicode::String Unicode::toPrague(const Unistring & us)
{
  Unicode::String tmp;
  tmp.resize(us.length());
  for (unsigned long i = 0; i < us.length(); i++)
    tmp[i] = us[i];
  return tmp;
}

Unicode::Char   Unicode::toPrague(const Unichar   & uc)
{
  Unicode::Char tmp = uc;
  return tmp;
}
