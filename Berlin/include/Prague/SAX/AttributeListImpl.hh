/*$Id: AttributeListImpl.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _AttributeListImpl_hh
#define _AttributeListImpl_hh

#include <Prague/SAX/AttributeList.hh>
#include <vector>

namespace SAX
{
  
class AttributeListImpl : public AttributeList
{
public:
  AttributeListImpl() {}
  AttributeListImpl(const AttributeList &al) { copy(al);}
  AttributeListImpl &operator= (const AttributeList &al) { clear(); copy(al); return *this;}
  virtual ~AttributeListImpl() {}
  
  void addAttribute(const std::string &name, const std::string &type, const std::string &value);
  void removeAttribute(const std::string &name);
  void clear();

  virtual size_t getLength() const { return names.size();}
  virtual const std::string &getName(size_t i) const;
  virtual const std::string &getType(size_t i) const;
  virtual const std::string &getValue(size_t i) const;

  virtual const std::string &getType(const std::string& name) const;
  virtual const std::string &getValue(const std::string& name) const;

private:
  size_t index(const std::string &name) const;
  void copy(const AttributeList &);
  std::vector<std::string> names;
  std::vector<std::string> types;
  std::vector<std::string> values;
};

};

#endif

