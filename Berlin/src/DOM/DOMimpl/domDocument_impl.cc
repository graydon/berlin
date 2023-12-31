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

#include "support/dom/impl/domDocument_impl.hh"

Node::NodeType domDocument_impl::getNodeType(void)
{
    return Node::DOCUMENT;
}

Node_ptr domDocument_impl::documentType( void)
{
    return (Node_ptr)0;
};

void domDocument_impl::documentType(Node_ptr _value)
{
};

Element_ptr domDocument_impl::documentElement( void)
{
    return (Element_ptr)0;
};

void domDocument_impl::documentElement(Element_ptr _value)
{
};

Element_ptr domDocument_impl::createElement(const wstring & tagName ,AttributeList_ptr attributes)
{
    return new domElement_impl(tagName, attributes);
};

//Text_ptr domDocument_impl::createTextNode(const wstring & data)
//{
//    return (Text_ptr)0;
//};

Comment_ptr domDocument_impl::createComment(const wstring & data)
{
    return (Comment_ptr)0;
};

ProcessingInstruction_ptr domDocument_impl::createProcessingInstruction(const wstring & name ,const wstring & data)
{
    return (ProcessingInstruction_ptr)0;
};

Attrib_ptr domDocument_impl::createAttribute(const wstring & name ,const wstring & value)
{
    return new domAttrib_impl(name, value);
};

AttributeList_ptr domDocument_impl::createAttributeList( void)
{
    return new domAttributeList_impl();
};

NodeIterator_ptr domDocument_impl::getElementsByTagName(const wstring & tagname)
{
    return (NodeIterator_ptr)0;
};

