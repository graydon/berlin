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

#include "support/dom/impl/domNode_impl.hh"
#include "support/dom/impl/domNodeIterator_impl.hh"
#include <NotMyChildException.hh>
#include <stddef.h>

Node_ptr domNode_impl::getParentNode( void)
{
    return _parentNode;
};

NodeIterator_ptr domNode_impl::getChildNodes( void)
{
    return new domNodeIterator_impl(_firstChild);
};

CORBA::Boolean domNode_impl::hasChildNodes( void)
{
    return (_firstChild != (Node_ptr)NULL);
};

Node_ptr domNode_impl::getFirstChild( void)
{
    return _firstChild;
};

Node_ptr domNode_impl::getPreviousSibling( void)
{
    return _previousSibling;
};

Node_ptr domNode_impl::getNextSibling( void)
{
    return _nextSibling;
};

Node_ptr domNode_impl::insertBefore(Node_ptr newChild ,Node_ptr refChild)
{
    Node_ptr curr = _firstChild;
    Node_ptr prevNode = (Node_ptr)NULL;

    if(!curr || !refChild)
            throw NotMyChildException();
    
    while(curr != refChild)
    {
        prevNode = curr;
        curr = curr->getNextSibling();
        if(!curr && refChild)
            throw NotMyChildException();
    }
    newChild->setParentNode__(this);
    newChild->setPreviousSibling__(prevNode);
    newChild->setNextSibling__(curr);
    if(prevNode)
        prevNode->setNextSibling__(newChild);
    else
        _firstChild = newChild;//This is inserted as the first child...
    curr->setPreviousSibling__(newChild);
    
    return newChild;
};

Node_ptr domNode_impl::replaceChild(Node_ptr newChild ,Node_ptr oldChild)
{
    Node_ptr curr = _firstChild;
    Node_ptr prevNode = (Node_ptr)NULL;

    if(!curr || !oldChild)
            throw NotMyChildException();
    
    while(curr != oldChild)
    {
        curr = curr->getNextSibling();
        if(!curr)
            throw NotMyChildException();
    }
    Node_ptr nextNode = curr->getNextSibling();
    newChild->setParentNode__(this);
    newChild->setPreviousSibling__(prevNode);
    newChild->setNextSibling__(nextNode);
    if(prevNode)
        prevNode->setNextSibling__(newChild);
    else
        _firstChild = newChild;//This is inserted as the first child
    if(nextNode)
        nextNode->setPreviousSibling__(newChild);
    oldChild->setPreviousSibling__((Node_ptr)NULL);
    oldChild->setNextSibling__((Node_ptr)NULL);
    oldChild->setParentNode__((Node_ptr)NULL);
    
    return newChild;
};

Node_ptr domNode_impl::removeChild(Node_ptr oldChild)
{
    Node_ptr curr = _firstChild;

    if(!curr || !oldChild)
            throw NotMyChildException();

    while(curr != oldChild)
    {
        curr = curr->getNextSibling();
        if(!curr)
            throw NotMyChildException();
    }
    Node_ptr prevNode = curr->getPreviousSibling();
    Node_ptr nextNode = curr->getPreviousSibling();
    if(prevNode)
        prevNode->setNextSibling__(nextNode);
    else
        _firstChild = nextNode;
    if(nextNode)
        nextNode->setPreviousSibling__(prevNode);
    curr->setPreviousSibling__((Node_ptr)NULL);
    curr->setNextSibling__((Node_ptr)NULL);
    curr->setParentNode__((Node_ptr)NULL);
    
    return oldChild;
};

void domNode_impl::setParentNode__ ( Node_ptr  newParent )
{
    _parentNode = newParent;
}

void domNode_impl::setPreviousSibling__ ( Node_ptr  newPrevSib )
{
    _previousSibling = newPrevSib;
}

void domNode_impl::setNextSibling__ ( Node_ptr  newNextSib )
{
    _nextSibling = newNextSib;
}
