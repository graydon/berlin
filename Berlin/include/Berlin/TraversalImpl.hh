/*$Id: TraversalImpl.hh,v 1.13 1999/10/13 21:32:31 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */
#ifndef _TraversalImpl_hh
#define _TraversalImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Traversal.hh"
#include "Berlin/TransformImpl.hh"
#include <vector>

class TraversalImpl : implements(Traversal)
{
  struct State
  {
    Graphic_var    graphic;
    Tag            tag;
    Region_var     allocation;
    TransformImpl *transformation;    
  };
  typedef vector<State> stack_t;
 public:
  TraversalImpl(Graphic_ptr, Region_ptr, Transform_ptr);
  TraversalImpl(const TraversalImpl &);
  ~TraversalImpl();
  virtual Region_ptr allocation();
  virtual Transform_ptr transformation();
  virtual CORBA::Boolean bounds(Vertex &, Vertex &, Vertex &);
  virtual CORBA::Boolean intersectsAllocation() = 0;
  virtual CORBA::Boolean intersectsRegion(Region_ptr) = 0;
  virtual void traverseChild(Graphic_ptr, Tag, Region_ptr, Transform_ptr);
  virtual void visit(Graphic_ptr) = 0;
  virtual order direction() = 0;
  virtual CORBA::Boolean ok() = 0;
  virtual void update();
 protected:
  void push(Graphic_ptr, Tag, Region_ptr, TransformImpl *);
  void pop();
  size_t size() { return stack.size();}
 private:
  stack_t stack;
};

#endif /* _TraversalImpl_h */
