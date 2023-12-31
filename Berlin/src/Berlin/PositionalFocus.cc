/*$Id: PositionalFocus.cc,v 1.27 2001/04/24 05:04:49 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Warsaw/config.hh>
#include <Warsaw/IO.hh>
#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Profiler.hh>
#include "Berlin/PositionalFocus.hh"
#include "Berlin/ScreenImpl.hh"
#include "Berlin/PickTraversalImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Provider.hh"
#include "Berlin/Console.hh"
#include "Berlin/Event.hh"
#include "Berlin/Vertex.hh"

using namespace Prague;
using namespace Warsaw;


//. implement a PickTraversal that keeps a fixed pointer to
//. a memento cache. As there is always exactly one picked
//. graphic, we are simply using a static pair of traversals
//. and switch between them as objects are picked
class PositionalFocus::Traversal : public PickTraversalImpl
{
public:
  Traversal(Warsaw::Graphic_ptr g, Warsaw::Region_ptr a, Warsaw::Transform_ptr t, PositionalFocus *f)
    : PickTraversalImpl(g, a, t, f), _memento(0), _picked(false) {}
  Traversal &operator = (const Traversal &t) { PickTraversalImpl::operator = (t); _pointer = t._pointer; _picked = false;}
  void pointer(const Warsaw::Input::Position &p) { _pointer = p;}
  void memento(Traversal *m) { _memento = m;}
  Traversal *memento() { return picked() ? _memento : 0;}
  virtual CORBA::Boolean intersects_region(Warsaw::Region_ptr);
  virtual void hit() { *_memento = *this, _picked = true;}
  virtual CORBA::Boolean picked() { return _picked;}
  void debug();
private:
  Warsaw::Input::Position _pointer;
  Traversal              *_memento;
  bool                    _picked;
};

CORBA::Boolean PositionalFocus::Traversal::intersects_region(Region_ptr region)
{
  const Transform::Matrix &matrix = get_transformation(current())->matrix();
  Coord d = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
  if (d == 0.) return false;
  Coord x = _pointer.x - matrix[0][3];
  Coord y = _pointer.y - matrix[1][3];
  Vertex local;
  local.x = (matrix[1][1] * x - matrix[0][1] * y)/d;
  local.y = (matrix[0][0] * y - matrix[1][0] * x)/d;
  Vertex lower, upper;
  region->bounds(lower, upper);
  return lower.x <= local.x && local.x <= upper.x && lower.y <= local.y && local.y <= upper.y;
}

void PositionalFocus::Traversal::debug()
{
  cout << "PositionalFocus::Traversal::debug : stack size = " << size() << '\n';
  Region_var r = current_allocation();
  Transform_var t = current_transformation();
  RegionImpl region(r, t);
  cout << "current allocation is " << region << endl;
  cout << "pointer is " << _pointer << endl;
  Vertex local = _pointer;
  Transform::Matrix matrix;
  t->store_matrix(matrix);
  cout << "current trafo \n" << matrix;
  t->inverse_transform_vertex(local);
  region.copy(r);
  cout << "local CS: current allocation is " << region << endl;
  cout << "local CS: pointer is " << local << endl;      
}

PositionalFocus::PositionalFocus(Input::Device d, Graphic_ptr g, Region_ptr r)
  : FocusImpl(d), _root(g), _pointer(new Pointer(Console::drawable())), _traversal(0), _grabbed(false)
{
  Trace trace("PositionalFocus::PositionalFocus");
  _traversal_cache[0] = new Traversal(_root, r, Transform::_nil(), this);
  _traversal_cache[1] = new Traversal(_root, r, Transform::_nil(), this);
  _traversal_cache[0]->memento(_traversal_cache[1]);
  _traversal_cache[1]->memento(_traversal_cache[0]);
  _traversal = _traversal_cache[0];
}

PositionalFocus::~PositionalFocus()
{
  Trace trace("PositionalFocus::~PositionalFocus");
  Impl_var<Traversal>::deactivate(_traversal_cache[0]);
  Impl_var<Traversal>::deactivate(_traversal_cache[1]);  
  delete _pointer;
}

void PositionalFocus::activate_composite()
{
  Trace trace("PositionalFocus::activate_composite");

}

void PositionalFocus::grab()
{
//   Prague::Guard<Mutex> guard(mutex);
  _grabbed = true;
}

void PositionalFocus::ungrab()
{
//   Prague::Guard<Mutex> guard(mutex);
  _grabbed = false;
}

void PositionalFocus::add_filter(Input::Filter_ptr)
{
  // not implemented
}

bool PositionalFocus::request(Controller_ptr c)
{
  return false; // not granted !
}

void PositionalFocus::restore(Region_ptr region)
{
  Vertex l, u;
  region->bounds(l, u);
  if (_pointer->intersects(l.x, u.x, l.y, u.y)) 
    _pointer->restore();
}

void PositionalFocus::damage(Region_ptr region)
{
  Trace trace("PositionalFocus::damage");
  Vertex l, u;
  region->bounds(l, u);
  if (_pointer->intersects(l.x, u.x, l.y, u.y))
    {
      _pointer->save();
      _pointer->draw();
    }
  Prague::Guard<Mutex> guard(_mutex);
  if (!_grabbed) return;
  Region_var allocation = _traversal->current_allocation();
  Transform_var transformation = _traversal->current_transformation();
  allocation->bounds(l, u);
  transformation->transform_vertex(l);
  transformation->transform_vertex(u);

  Lease_var<RegionImpl> bbox(Provider<RegionImpl>::provide());

  bbox->valid = true;
  bbox->lower.x = std::min(l.x, u.x);
  bbox->lower.y = std::min(l.y, u.y);
  bbox->upper.x = std::max(l.x, u.x);
  bbox->upper.y = std::max(l.y, u.y);
  if (bbox->intersects(region)) _traversal->update();
}

/*
 * the dispatching strategy is the following:
 * we keep a PickTraversal cached which points
 * to the controller currently holding focus.
 *
 * dispatching means to call traverse on this
 * controller which should, if the controller
 * or one of it's children 'hits', result in
 * a memento (traversal->memento()).
 *
 * if the traversal doesn't contain a memento,
 * it means that the controller should lose
 * focus, so we start over at the parent controller...
 */
void PositionalFocus::dispatch(Input::Event &event)
{
  Prague::Guard<Mutex> guard(_mutex);
  Trace trace("PositionalFocus::dispatch");
  Input::Position position;
  int pidx = Input::get_position(event, position);
  if (pidx == -1)
    {
      cerr << "PositionalFocus::dispatch error : non positional event" << endl;
      return;
    }
  /*
   * update the pointer object / image
   */
  _pointer->move(position.x, position.y);
  _traversal->pointer(position);
  if (!_grabbed)
    {
      _traversal->reinit();
      _root->traverse(Traversal_var(_traversal->_this()));
      Traversal *picked = _traversal->memento();
      if (!picked)
	{
	  cerr << "PositionalFocus::dispatch : no Controller found ! (position is " << position << ")" << endl;
	  return;
	}
      else _traversal = picked;
      /*
       * ...now do the [lose/receive]Focus stuff,...
       */
      std::vector<Controller_var>::const_iterator nf = _traversal->controllers().begin();
      cstack_t::iterator of = _controllers.begin();
      /*
       * ...skip the unchanged controllers,...
       */
      while (nf != _traversal->controllers().end() &&
	     of != _controllers.end() &&
	     (*nf)->is_identical(*of)) ++nf, ++of;
      /*
       * ...remove the old controllers in reverse order,...
       */
      for (cstack_t::reverse_iterator o = _controllers.rbegin(); o.base() != of; ++o)
	try { (*o)->lose_focus(device());}
	catch (const CORBA::OBJECT_NOT_EXIST &) {}
	catch (const CORBA::COMM_FAILURE &) {}
      _controllers.erase(of, _controllers.end());
      /*
       * ...add the new controllers,...
       */
      Focus_var __this = _this();
      for (; nf != picked->controllers().end(); ++nf)
	{
	  (*nf)->receive_focus (__this);
 	  _controllers.push_back(*nf);
	}
    }
  /*
   * ...and finally dispatch the event
   */
//   _traversal->debug();
//   Transform_var(_traversal->current_transformation())->inverse_transform_vertex(position);
//   event[pidx].attr.location(position);
//   cout << "distributing positional event at " << position << endl;
  _controllers.back()->handle_positional(PickTraversal_var(_traversal->_this()), event);
}
