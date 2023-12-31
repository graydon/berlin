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

#include "support/dom/impl/domNodeIterator_impl.hh"
#include <stddef.h>

CORBA::ULong domNodeIterator_impl::getLength( void)
{
    return 0;
};

CORBA::ULong domNodeIterator_impl::getCurrentPos( void)
{
    return 0;
};

CORBA::Boolean domNodeIterator_impl::atFirst( void)
{
    return (_currentNode->getPreviousSibling() == ((Node_ptr)NULL));
};

CORBA::Boolean domNodeIterator_impl::atLast( void)
{
    return (_currentNode->getNextSibling() == ((Node_ptr)NULL));
};

Node_ptr domNodeIterator_impl::toNextNode( void)
{
    Node_ptr next = _currentNode->getNextSibling();
    if(next)
        _currentNode = next;
    return _currentNode;
};

Node_ptr domNodeIterator_impl::toPrevNode( void)
{
    Node_ptr prev = _currentNode->getPreviousSibling();
    if(prev)
        _currentNode = prev;
    return _currentNode;
};

Node_ptr domNodeIterator_impl::toFirstNode( void)
{
    _currentNode = _currentNode->getParentNode()->getFirstChild();
    return _currentNode;;
};

Node_ptr domNodeIterator_impl::toLastNode( void)
{
    Node_ptr nextNode = _currentNode->getNextSibling();
    while(nextNode)
    {
        _currentNode = nextNode;
        nextNode = _currentNode->getNextSibling();
    }
    return _currentNode;
};

Node_ptr domNodeIterator_impl::moveTo(CORBA::Long n)
{
    return (Node_ptr)0;
};

