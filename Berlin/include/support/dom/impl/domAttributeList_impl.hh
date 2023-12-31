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

#ifndef __DOMATTRIBUTELIST_IMPL__
#define __DOMATTRIBUTELIST_IMPL__

#include "support/dom/impl/domAttrib_impl.hh"
#include <AttributeList.hh>
#include <stddef.h>

class domAttributeList_impl : virtual public _sk_AttributeList
{
private:
    Attrib_ptr *_attrArray;
    CORBA::ULong _arrSize, _arrLength;
public:
    domAttributeList_impl() : _sk_AttributeList()
      { _attrArray = (Attrib_ptr *)NULL; _arrSize = 0; _arrLength = 0; }
    virtual ~domAttributeList_impl();

    virtual Attrib_ptr getAttribute(const wstring & attrName);
    virtual Attrib_ptr setAttribute(Attrib_ptr attr);
    virtual Attrib_ptr remove(const wstring & attrName);
    virtual Attrib_ptr item(CORBA::ULong index);
    virtual CORBA::ULong getLength(void);
};

#endif //__DOMATTRIBUTELIST_IMPL__
