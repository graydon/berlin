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

#ifndef __DOMNODEITERATOR_IMPL__
#define __DOMNODEITERATOR_IMPL__

#include <NodeIterator.hh>
#include "support/dom/impl/domNode_impl.hh"

class domNodeIterator_impl : virtual public _sk_NodeIterator
{
private:
    Node_ptr _currentNode;
public:
    domNodeIterator_impl(Node_ptr startNode) : _sk_NodeIterator()
    	{ _currentNode = startNode; }

    virtual CORBA::ULong getLength(void);
    virtual CORBA::ULong getCurrentPos(void);
    virtual CORBA::Boolean atFirst(void);
    virtual CORBA::Boolean atLast(void);
    virtual Node_ptr toNextNode(void);
    virtual Node_ptr toPrevNode(void);
    virtual Node_ptr toFirstNode(void);
    virtual Node_ptr toLastNode(void);
    virtual Node_ptr moveTo(CORBA::Long n);
};

#endif //__DOMNODEITERATOR_IMPL__
