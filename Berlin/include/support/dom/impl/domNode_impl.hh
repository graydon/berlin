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

#ifndef __DOMNODE_IMPL__
#define __DOMNODE_IMPL__

#include <Node.hh>

class domNode_impl : virtual public _sk_Node
{
public:
    //virtual NodeType getNodeType(void);
    virtual Node_ptr getParentNode(void);
    virtual NodeIterator_ptr getChildNodes(void);
    virtual CORBA::Boolean hasChildNodes(void);
    virtual Node_ptr getFirstChild(void);
    virtual Node_ptr getPreviousSibling(void);
    virtual Node_ptr getNextSibling(void);
    virtual Node_ptr insertBefore(Node_ptr newChild ,Node_ptr refChild);
    virtual Node_ptr replaceChild(Node_ptr newChild ,Node_ptr oldChild);
    virtual Node_ptr removeChild(Node_ptr oldChild);

    //implementaion specific methods...
    virtual void setParentNode__ ( Node_ptr  newParent );
    virtual void setPreviousSibling__ ( Node_ptr  newPrevSib );
    virtual void setNextSibling__ ( Node_ptr  newNextSib );    

private:
    Node_ptr _parentNode;
    Node_ptr _nextSibling;
    Node_ptr _previousSibling;
    Node_ptr _firstChild;
};

#endif //__DOMNODE_IMPL__