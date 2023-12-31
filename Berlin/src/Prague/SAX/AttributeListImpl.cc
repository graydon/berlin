/*$Id: AttributeListImpl.cc,v 1.2 2000/09/23 21:18:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Jez Higgins <jez@jezuk.demon.co.uk>
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
#include "Prague/SAX/AttributeListImpl.hh"
#include <algorithm>

using namespace SAX;

void AttributeListImpl::addAttribute(const string &name, const string &type, const string &value)
{
  names.push_back(name);
  types.push_back(type);
  values.push_back(value);
}

void AttributeListImpl::removeAttribute(const string &name)
{
  size_t i = index(name);
  if(i == getLength()) return;

  names.erase(names.begin() + i);
  types.erase(types.begin() + i);
  values.erase(values.begin() + i);
}

void AttributeListImpl::clear()
{
  names.clear();
  types.clear();
  values.clear();
}

const string empty;

const string &AttributeListImpl::getName(size_t i) const
{
  if(i > getLength()) return empty;
  return names[i];
}

const string &AttributeListImpl::getType(size_t i) const
{
  if(i > getLength()) return empty;
  return types[i];
}

const string &AttributeListImpl::getValue(size_t i) const
{
  if(i > getLength()) return empty;
  return values[i];
}

const string &AttributeListImpl::getType(const string &name) const
{
  if(size_t i = index(name) == getLength()) return empty;
  return getType(index(name));
}

const string &AttributeListImpl::getValue(const string &name) const
{
  if(size_t i = index(name) == getLength()) return empty;
  return getValue(index(name));
}
 
size_t AttributeListImpl::index(const string &name) const
{
  return find(names.begin(), names.end(), name) - names.begin();
}

void AttributeListImpl::copy(const AttributeList &al)
{
  size_t count = al.getLength();
  for (size_t i = 0; i < count; i++)
    addAttribute(al.getName(i), al.getType(i), al.getValue(i));
}
