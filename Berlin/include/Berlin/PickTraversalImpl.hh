/*$Id: PickTraversalImpl.hh,v 1.29 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _PickTraversalImpl_hh
#define _PickTraversalImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Input.hh>
#include <Warsaw/Controller.hh>
#include <Warsaw/PickTraversal.hh>
#include <Warsaw/Transform.hh>
#include <Berlin/TraversalImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/Vertex.hh>
#include <Prague/Sys/Tracer.hh>

class PositionalFocus;

class PickTraversalImpl : public virtual POA_Warsaw::PickTraversal,
                          public TraversalImpl
{
  typedef std::vector<Warsaw::Controller_var> cstack_t;
  typedef std::vector<size_t> pstack_t;
public:
  PickTraversalImpl(Warsaw::Graphic_ptr, Warsaw::Region_ptr, Warsaw::Transform_ptr, PositionalFocus *);
  PickTraversalImpl(const PickTraversalImpl &);
  ~PickTraversalImpl();
  PickTraversalImpl &operator = (const PickTraversalImpl &);
  virtual Warsaw::PickTraversal_ptr _this();
  virtual Warsaw::Region_ptr current_allocation();
  virtual Warsaw::Transform_ptr current_transformation();
  virtual Warsaw::Graphic_ptr current_graphic();
  virtual void traverse_child(Warsaw::Graphic_ptr, Warsaw::Tag, Warsaw::Region_ptr, Warsaw::Transform_ptr);
  virtual void visit(Warsaw::Graphic_ptr);
  virtual Warsaw::Traversal::order direction();
  virtual CORBA::Boolean ok();
  virtual CORBA::Boolean intersects_allocation();
//   virtual CORBA::Boolean intersects_region(Warsaw::Region_ptr) = 0;
  virtual void enter_controller(Warsaw::Controller_ptr);
  virtual void leave_controller();
  virtual void hit() = 0;
  virtual CORBA::Boolean picked() = 0;
  virtual Warsaw::Focus_ptr get_focus();
  virtual CORBA::Boolean forward();
  virtual CORBA::Boolean backward();

  void pop_controller();
  Warsaw::Controller_ptr top_controller();
  const std::vector<Warsaw::Controller_var> &controllers() const { return _controllers;}
  void reinit();
protected:
  size_t current() const { return _cursor;}
private:
  cstack_t                   _controllers;
  pstack_t                   _positions;
  PositionalFocus           *_focus;
  size_t                     _cursor;
  Warsaw::PickTraversal_var __this;
};

//. remove one controller level from the top, it might have got out of scope
inline void PickTraversalImpl::pop_controller()
{
  Prague::Trace trace("PickTraversal::pop_controller");
  if (_controllers.size())
    {
      while (size() > _positions.back()) pop();
      _cursor = size() - 1;
      _controllers.pop_back();
      _positions.pop_back();
    }
}

inline void PickTraversalImpl::reinit()
{
  Prague::Trace trace("PickTraversal::reinit");
  _controllers.clear();
  _positions.clear();
  while (size() > 1)
    {
      Lease_var<TransformImpl> trafo(get_transformation(size() - 1));
      pop();
    }
  _cursor = 0;
}

inline Warsaw::Controller_ptr PickTraversalImpl::top_controller()
{
  return _controllers.size() ? Warsaw::Controller::_duplicate(_controllers.back()) : Warsaw::Controller::_nil();
}

#endif 
