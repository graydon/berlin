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

#include "support/dom/impl/domElement_impl.hh"
#include <NoSuchAttributeException.hh>
#include <stdio.h>
#include <Document.hh>

domElement_impl::domElement_impl(const wstring & tagName, AttributeList_ptr attributes)
{
    _tagName = domStringDup(&tagName);

    if(!attributes)
    {
        //Create attribute list...
        Document_ptr masterDoc = getMasterDoc();
        if(!masterDoc)
            return;

        _attribList = masterDoc->createAttributeList();
    }
    else
        _attribList = attributes;
}

domElement_impl::~domElement_impl()
{
    domStringFree(_tagName);
    //FIXME:
    //if(_attribList)
    //    delete _attribList;
}

Document_ptr domElement_impl::getMasterDoc(void)
{
    Node_ptr doc = this;
    while(doc)
        doc = doc->getParentNode();

    Document_ptr masterDoc = (Document_ptr)NULL;
    if(doc->getNodeType() == Node::DOCUMENT)
        masterDoc = Document::_narrow(doc);
    
    if(!masterDoc)
        puts("Cannot find master document in domElement_impl::getMasterDoc");
    //FIXME: Exception?

    return masterDoc;
}

Node::NodeType domElement_impl::getNodeType(void)
{
    return Node::ELEMENT;
}

wstring * domElement_impl::getTagName (  )
{
    return domStringDup(_tagName);
}


NodeIterator_ptr  domElement_impl::getAttributes (  )
{
    //FIXME: AttributeList? Wait for next DOM draft...
    return (NodeIterator_ptr)0;
}

wstring * domElement_impl::getAttribute ( const wstring & name )
{
    return getAttributeNode(name)->getValue();
}

void domElement_impl::setAttribute ( const wstring & name, const wstring & value )
{
    Document_ptr masterDoc = getMasterDoc();
    if(!masterDoc)
        return;
    
    Attrib_ptr attr = masterDoc->createAttribute(name, value);
    
    if(attr)
        setAttributeNode(attr);
    else
        puts("Cannot create attribute in domElement_impl::setAttribute");
    //FIXME: Exception?
    
}

void domElement_impl::removeAttribute ( const wstring & name )
{
    if(!_attribList)
        throw NoSuchAttributeException();

    _attribList->remove(name);//FIXME: Free returned attribute?
}

Attrib_ptr  domElement_impl::getAttributeNode ( const wstring & name )
{
    if(!_attribList)
        throw NoSuchAttributeException();
    
    return _attribList->getAttribute(name);
}

void domElement_impl::setAttributeNode ( Attrib_ptr  newAttr )
{
    _attribList->setAttribute(newAttr);
}

void domElement_impl::removeAttributeNode ( Attrib_ptr  oldAttr )
{
    if(!oldAttr)
        throw NoSuchAttributeException();
    
    wstring *name = oldAttr->getName();
    try
    {
        removeAttribute(*name);
    }
    catch(...)
    {
        delete name;
        throw;
    }
}

void domElement_impl::getElementsByTagName ( const wstring & tagname )
{
    //FIXME: This method does not return anything!!! Wait for next DOM spec.
}

void domElement_impl::normalize (  )
{
    //FIXME: Not implemented yet... Wait for next DOM spec.
}
