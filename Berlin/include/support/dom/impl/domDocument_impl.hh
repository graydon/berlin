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

#ifndef __DOMDOCUMENT_IMPL__
#define __DOMDOCUMENT_IMPL__

#include "support/dom/impl/domNode_impl.hh"
#include "support/dom/impl/domNodeIterator_impl.hh"
#include "support/dom/impl/domElement_impl.hh"
#include "support/dom/impl/domAttributeList_impl.hh"
//#include "domText_impl.hh"
#include "support/dom/impl/domComment_impl.hh"
#include "support/dom/impl/domProcessingInstruction_impl.hh"
#include "support/dom/impl/domAttrib_impl.hh"
#include <Document.hh>

class domDocument_impl : virtual public domNode_impl, _sk_Document
{
    virtual NodeType getNodeType(void);
    virtual Node_ptr documentType(void);
    virtual void documentType(Node_ptr _value);
    virtual Element_ptr documentElement(void);
    virtual void documentElement(Element_ptr _value);
    virtual Element_ptr createElement(const wstring & tagName ,AttributeList_ptr attributes);
    //virtual Text_ptr createTextNode(const wstring & data);
    virtual Comment_ptr createComment(const wstring & data);
    virtual ProcessingInstruction_ptr createProcessingInstruction(const wstring & name ,const wstring & data);
    virtual Attrib_ptr createAttribute(const wstring & name ,const wstring & value);//FIXME: Was: Node_ptr value
    virtual AttributeList_ptr createAttributeList(void);
    virtual NodeIterator_ptr getElementsByTagName(const wstring & tagname);
};

#endif //__DOMDOCUMENT_IMPL__
