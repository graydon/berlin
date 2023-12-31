//
// $id:$
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 The Berlin Consortium 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
// Author: ANOQ of the Sun
// EMail: anoq@berlin-consortium.org or anoq@vip.cybercity.dk

#ifndef __DOMATTRIB_IMPL__
#define __DOMATTRIB_IMPL__

#include <Attribute.hh>
#include "support/dom/impl/domNode_impl.hh"

class domAttrib_impl : virtual public _sk_Attrib, domNode_impl
{
private:
    wstring *_attrName;
    wstring *_attrValue;
    CORBA::Boolean _specified;

public:
  virtual NodeType getNodeType(void);
  virtual wstring * getName (  );
  virtual wstring * getValue (  );
  virtual CORBA::Boolean  specified ();
  virtual void specified (CORBA::Boolean  _value);
  virtual wstring * toString (  );

  //Implementation specific... DO NOT USE!
  virtual ~domAttrib_impl();
  domAttrib_impl(const wstring & name, const wstring & value);
};

#endif //__DOMATTRIB_IMPL__