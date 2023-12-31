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

#ifndef __DOMELEMENT_IMPL__
#define __DOMELEMENT_IMPL__

#include "support/dom/impl/domString.hh"
#include "support/dom/impl/domAttrib_impl.hh"
#include "support/dom/impl/domAttributeList_impl.hh"
#include "support/dom/impl/domNodeIterator_impl.hh"
#include <Element.hh>
#include <Document.hh>

class domElement_impl : virtual public _sk_Element, domNode_impl
{
private:
	wstring *_tagName;
        AttributeList_ptr _attribList;

  Document_ptr getMasterDoc(void);
        
public:
  virtual NodeType getNodeType(void);
  virtual wstring * getTagName (  );
  virtual NodeIterator_ptr  getAttributes (  );//AttributeList? Wait for next DOM spec.
  virtual wstring * getAttribute ( const wstring & name );
  virtual void setAttribute ( const wstring & name, const wstring & value );
  virtual void removeAttribute ( const wstring & name );
  virtual Attrib_ptr  getAttributeNode ( const wstring & name );
  virtual void setAttributeNode ( Attrib_ptr  newAttr );
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr );
  virtual void getElementsByTagName ( const wstring & tagname );
  virtual void normalize (  );

  //Implementation specific methods - DO NOT USE!
  virtual ~domElement_impl();
  domElement_impl(const wstring & tagName, AttributeList_ptr attributes);
};

#endif //__DOMELEMENT_IMPL__