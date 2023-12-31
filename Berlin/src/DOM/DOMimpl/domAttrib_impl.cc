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

#include "support/dom/impl/domAttrib_impl.hh"
#include "support/dom/impl/domString.hh"

domAttrib_impl::~domAttrib_impl()
{
    domStringFree(_attrName);
    domStringFree(_attrValue);
}

domAttrib_impl::domAttrib_impl(const wstring & name, const wstring & value) : _sk_Attrib(), domNode_impl()
{
    _attrName = domStringDup(&name);
    _attrValue = domStringDup(&value);
    _specified = 0;
}

Node::NodeType domAttrib_impl::getNodeType(void)
{
    return Node::ATTRIBUTE;;
}

wstring * domAttrib_impl::getName (  )
{
    return domStringDup(_attrName);
}

wstring * domAttrib_impl::getValue (  )
{
    return domStringDup(_attrValue);
}

CORBA::Boolean  domAttrib_impl::specified ()
{
    return _specified;
}

void domAttrib_impl::specified (CORBA::Boolean  _value)
{
    _specified = _value;
}

wstring * domAttrib_impl::toString (  )
{
    return (wstring *)0;
}

