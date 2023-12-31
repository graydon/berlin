/*$Id: KitFactory.cc,v 1.3 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Berlin/KitFactory.hh"
#include "Berlin/KitImpl.hh"

using namespace Warsaw;

KitFactory::KitFactory(const std::string &t, const std::string *p, unsigned short s)
  : _props(0), _id(t), _counter(0)
{
  _props = new Kit::PropertySeq();
  _props->length(s);
  for (size_t i = 0; i != s; i++)
    {
      (*_props)[i].name = CORBA::string_dup((p++)->c_str());
      (*_props)[i].value = CORBA::string_dup((p++)->c_str());
    }
}

bool KitFactory::supports(const Kit::PropertySeq &set1, const Kit::PropertySeq &set2)
//. test whether set1 contains set2
{
  const Kit::Property *begin2 = set2.get_buffer();
  const Kit::Property *end2 = begin2 + set2.length();
  for (const Kit::Property *property2 = begin2; property2 != end2; property2++)
    {
      const Kit::Property *begin1 = set1.get_buffer();
      const Kit::Property *end1 = begin1 + set1.length();
      const Kit::Property *property1;
      for (property1 = begin1; property1 != end1; property1++)
	if (strcmp(property1->name, property2->name) == 0)
	  {
	    if (strcmp(property1->value, property2->value) == 0) break;
	    else return false; // value not supported
	  }
      if (property1 == end1) return false; // property not supported
    }
  return true;
}

void KitFactory::activate(KitImpl *kit, PortableServer::POA_ptr poa)
{
  Prague::Trace trace("KitFactory::activate");
  kit->poa = PortableServer::POA::_duplicate(poa);
  PortableServer::ObjectId *oid = poa->activate_object(kit);
  kit->_remove_ref();
  delete oid;
}

