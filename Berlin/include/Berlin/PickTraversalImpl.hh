/*$Id: PickTraversalImpl.hh,v 1.17 1999/11/06 20:23:07 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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

#include "Warsaw/config.hh"
#include "Warsaw/Event.hh"
#include "Warsaw/Controller.hh"
#include "Warsaw/PickTraversal.hh"
#include "Warsaw/Transform.hh"
#include "Warsaw/Focus.hh"
#include "Berlin/TraversalImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Logger.hh"
#include "Berlin/Vertex.hh"

class PickTraversalImpl : implements(PickTraversal), public TraversalImpl
{
  typedef vector<Controller_var> cstack_t;
  typedef vector<size_t> pstack_t;
 public:
  PickTraversalImpl(Graphic_ptr, Region_ptr, Transform_ptr, const Event::Pointer &, Focus_ptr);
  //. to be used when starting from root level
  ~PickTraversalImpl();
  void visit(Graphic_ptr g) { g->pick(PickTraversal_var(_this()));}
  order direction() { return down;}
  CORBA::Boolean ok() { return !mem;}
  CORBA::Boolean intersectsAllocation();
  CORBA::Boolean intersectsRegion(Region_ptr);
  void enterController(Controller_ptr);
  void leaveController();
  void hit();
  void popController();
  CORBA::Boolean picked() { return mem;}
  void grab() { focus->grab();}
  void ungrab() { focus->ungrab();}

  Controller_ptr topController();
  const vector<Controller_var> &controllerStack() const { return controllers;}
  PickTraversalImpl   *memento() { PickTraversalImpl *m = mem; mem = 0; return m;}
  void reset(const Event::Pointer &);
  void debug();
 private:
  PickTraversalImpl(const PickTraversalImpl &);
  //. to be used to create the memento
  cstack_t           controllers;
  pstack_t           positions;
  Event::Pointer     pointer;
  Focus_var          focus;
  PickTraversalImpl *mem;
};

inline void PickTraversalImpl::popController()
//. remove one controller level from the top, it might have got out of scope
{
  SectionLog log("PickTraversal::popController");
  if (controllers.size())
    {
      while (size() > positions.back()) pop();
      controllers.pop_back();
      positions.pop_back();
    }
}

inline Controller_ptr PickTraversalImpl::topController()
{
  return controllers.size() ? Controller::_duplicate(controllers.back()) : Controller::_nil();
}

inline void PickTraversalImpl::reset(const Event::Pointer &p)
//. pop all graphics up to the top most controller and set the pointer
//. so the traversal can be used to start over directly at the top
{
  SectionLog log("PickTraversal::reset");
  popController();
  pointer = p;
}

#endif /* _PickTraversalImpl_hh */
