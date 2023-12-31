/*$Id: TraversalImpl.hh,v 1.23 2001/04/18 06:07:25 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * http://www.berlin-consortium.org
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

#include <Warsaw/config.hh>
#include <Warsaw/Traversal.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Region.hh>
#include <Berlin/ServantBase.hh>
#include <Berlin/TransformImpl.hh>
#include <vector>

class RegionImpl;

//. TraversalImpl keeps a stack of context information for
//. the Graphic nodes it visits. Use push and pop to add/remove
//. new nodes to the stack. Items added via push are assumed to
//. be managed by the caller, i.e. they are not deleted in the pop
//. operation. In contrast, if you create a TraversalImpl via the
//. copy constructor, it creates a deep copy of raw pointer types,
//. and increments the ref count for ref counted objects.
//. The number of push and pop operations called on a single TraversalImpl
//. object therefor need to be balanced.
class TraversalImpl : public virtual POA_Warsaw::Traversal,
                      public virtual ServantBase
{
  struct State
  {
    State() : id(0), transformation(0) {}
    State(Warsaw::Graphic_ptr g, Warsaw::Tag i, Warsaw::Region_ptr a, TransformImpl *t)
      : graphic(g), id(i), allocation(a), transformation(t) {}
    Warsaw::Graphic_ptr      graphic;
    Warsaw::Tag              id;
    Warsaw::Region_ptr       allocation;
    TransformImpl           *transformation;    
  };
  typedef std::vector<State> stack_t;
public:
  TraversalImpl(Warsaw::Graphic_ptr, Warsaw::Region_ptr, Warsaw::Transform_ptr);
  TraversalImpl(const TraversalImpl &);
  ~TraversalImpl();
  TraversalImpl &operator = (const TraversalImpl &);
  virtual Warsaw::Region_ptr current_allocation();
  virtual Warsaw::Transform_ptr current_transformation();
  virtual Warsaw::Graphic_ptr current_graphic();
  virtual CORBA::Boolean bounds(Warsaw::Vertex &, Warsaw::Vertex &, Warsaw::Vertex &);
  virtual CORBA::Boolean intersects_allocation() = 0;
  virtual CORBA::Boolean intersects_region(Warsaw::Region_ptr) = 0;
  virtual void traverse_child(Warsaw::Graphic_ptr, Warsaw::Tag, Warsaw::Region_ptr, Warsaw::Transform_ptr) = 0;
  virtual void visit(Warsaw::Graphic_ptr) = 0;
  virtual Warsaw::Traversal::order direction() = 0;
  virtual CORBA::Boolean ok() = 0;
  virtual void update();
protected:
  //. push puts the actual trail values on a stack. They are *not* reference counted,
  //. it is assumed that pop is called in the same scope.
  //. Alternatively, values not removed from the stack are deallocated in the destructor.
  void push(Warsaw::Graphic_ptr, Warsaw::Tag, Warsaw::Region_ptr, TransformImpl *);
  void pop();
  size_t size() const { return _stack.size();}
  Warsaw::Region_ptr get_allocation(size_t i) { return _stack[i].allocation;}
  TransformImpl *get_transformation(size_t i) { return _stack[i].transformation;}
  Warsaw::Graphic_ptr get_graphic(size_t i) { return _stack[i].graphic;}
private:
  void clear();
  stack_t _stack;
};

#endif

