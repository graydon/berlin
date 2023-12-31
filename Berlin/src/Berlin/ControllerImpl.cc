/*$Id: ControllerImpl.cc,v 1.31 2001/02/06 19:46:16 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Input.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/PickTraversal.hh>
#include <Warsaw/Focus.hh>
#include "Berlin/ControllerImpl.hh"
#include "Berlin/Event.hh"
#include <Prague/Sys/Tracer.hh>
#include <Babylon/Babylon.hh>

using namespace Prague;
using namespace Warsaw;
using namespace Babylon;

class ControllerImpl::Iterator : public virtual POA_Warsaw::ControllerIterator,
		                 public virtual ServantBase
{
public:
  Iterator(ControllerImpl *p, Tag c) : _parent(p), _cursor(c) { Trace trace("ControllerImpl::Iterator::Iterator"); _parent->_add_ref();}
  virtual ~Iterator() { Trace trace("ControllerImpl::Iterator::~Iterator"); _parent->_remove_ref();}
  virtual Warsaw::Controller_ptr child()
  {
    Trace trace("ControllerImpl::Iterator::child");
    Prague::Guard<Mutex> guard(_parent->_cmutex);
    if (_cursor > _parent->_children.size()) return Warsaw::Controller::_nil();
    return Warsaw::Controller::_duplicate(_parent->_children[_cursor]);
  }
  virtual void next() { _cursor++;}
  virtual void prev() { _cursor--;}
  virtual void insert(Controller_ptr child)
  {
    Trace trace("ControllerImpl::Iterator::insert");
    {
      Prague::Guard<Mutex> guard(_parent->_cmutex);
      if (_cursor > _parent->_children.size()) _cursor = _parent->_children.size();
      _parent->_children.insert(_parent->_children.begin() + _cursor, RefCount_var<Warsaw::Controller>::increment(child));
      child->set_parent_controller(Controller_var(_parent->_this()));
    }
    _parent->need_resize();
  }
  virtual void replace(Controller_ptr child)
  {
    Trace trace("ControllerImpl::Iterator::replace");
    {
      Prague::Guard<Mutex> guard(_parent->_cmutex);
      if (_cursor > _parent->_children.size()) return;
      Controller_var old = static_cast<Controller_ptr>(_parent->_children[_cursor]);
      if (!CORBA::is_nil(old))
	try
	  {
	    old->remove_parent_controller();
	  }
	catch(const CORBA::OBJECT_NOT_EXIST &) {}
	catch (const CORBA::COMM_FAILURE &) {}
      _parent->_children[_cursor] = RefCount_var<Warsaw::Controller>::increment(child);
      child->set_parent_controller(Controller_var(_parent->_this()));
    }
    _parent->need_resize();
  }
  virtual void remove()
  {
    Trace trace("ControllerImpl::Iterator::remove");
    {
      Prague::Guard<Mutex> guard(_parent->_cmutex);
      if (_cursor > _parent->_children.size()) return;
      ControllerImpl::clist_t::iterator i = _parent->_children.begin() + _cursor;
      try
	{
	  (*i)->remove_parent_controller();
	}
      catch (const CORBA::OBJECT_NOT_EXIST &) {}
      catch (const CORBA::COMM_FAILURE &) {}
      _parent->_children.erase(i);
    }
    _parent->need_resize();
  }
  virtual void destroy() { deactivate();}
private:
  ControllerImpl *_parent;
  size_t          _cursor;
};

ControllerImpl::ControllerImpl(bool t)
  : _telltale(0), _focus(0), _grabs(0), _transparent(t)
{
  Trace trace("ControllerImpl::ControllerImpl");
}

ControllerImpl::~ControllerImpl()
{
  Trace trace("ControllerImpl::~ControllerImpl");
}

void ControllerImpl::deactivate()
{
  Trace trace("ControllerImpl::deactivate");
  remove_parent_controller();
  ServantBase::deactivate(this);
}

void ControllerImpl::traverse(Traversal_ptr traversal)
{
  Trace trace("ControllerImpl::traverse");
  traversal->visit(Graphic_var(_this()));
}

void ControllerImpl::draw(DrawTraversal_ptr traversal)
{
  Trace trace("ControllerImpl::draw");
  MonoGraphic::traverse(traversal);
}

void ControllerImpl::pick(PickTraversal_ptr traversal)
{
  Trace trace("ControllerImpl::pick");
  if (traversal->intersects_allocation())
    {
      traversal->enter_controller(Controller_var(_this()));
      MonoGraphic::traverse(traversal);
      if (!_transparent && !traversal->picked()) traversal->hit();
      traversal->leave_controller();
    }
}

void ControllerImpl::append_controller(Controller_ptr c)
{
  Trace trace("ControllerImpl::append_controller");
  if (CORBA::is_nil(c) || !CORBA::is_nil(Controller_var(c->parent_controller()))) return;
  Prague::Guard<Mutex> guard(_cmutex);
  _children.push_back(RefCount_var<Warsaw::Controller>::increment(c));
  c->set_parent_controller(Controller_var(_this()));
}

void ControllerImpl::prepend_controller(Controller_ptr c)
{
  Trace trace("ControllerImpl::prepend_controller");
  if (CORBA::is_nil(c) || !CORBA::is_nil(Controller_var(c->parent_controller()))) return;
  Prague::Guard<Mutex> guard(_cmutex);
  _children.insert(_children.begin(), RefCount_var<Warsaw::Controller>::increment(c));
  c->set_parent_controller(Controller_var(_this()));
}

void ControllerImpl::remove_controller(Controller_ptr c)
{
  Trace trace("ControllerImpl::remove_controller");
  if (CORBA::is_nil(c) || !CORBA::is_nil(Controller_var(c->parent_controller()))) return;
  Prague::Guard<Mutex> guard(_cmutex);
  for (clist_t::iterator i = _children.begin(); i != _children.end(); ++i)
    if ((*i)->is_identical(c))
      {
	(*i)->remove_parent_controller();
	_children.erase(i);
	return;
      }
}

void ControllerImpl::set_parent_controller(Controller_ptr p)
{
  Trace trace("ControllerImpl::set_parent_controller");
  Prague::Guard<Mutex> guard(_pmutex);
  _parent = Warsaw::Controller::_duplicate(p);
}

void ControllerImpl::remove_parent_controller()
{
  Trace trace("ControllerImpl::remove_parent_controller");
  Prague::Guard<Mutex> guard(_pmutex);
  _parent = Warsaw::Controller::_nil();
}

Controller_ptr ControllerImpl::parent_controller()
{
  Trace trace("ControllerImpl::parent_controller");
  Prague::Guard<Mutex> guard(_pmutex);
  return Warsaw::Controller::_duplicate(_parent);
}

Warsaw::Controller::Iterator_ptr ControllerImpl::first_child_controller()
{
  Trace trace("ControllerImpl::first_child_controller");
  Iterator *iterator = new Iterator(this, 0);
  activate(iterator);
  return iterator->_this();
}

Warsaw::Controller::Iterator_ptr ControllerImpl::last_child_controller()
{
  Trace trace("ControllerImpl::last_child_controller");
  Prague::Guard<Mutex> guard(_cmutex);
  Iterator *iterator = new Iterator(this, _children.size() - 1);
  activate(iterator);
  return iterator->_this();
}

CORBA::Boolean ControllerImpl::request_focus(Controller_ptr c, Input::Device d)
{
  Trace trace("ControllerImpl::request_focus");  
  Controller_var parent = parent_controller();
  return CORBA::is_nil(parent) ? false : parent->request_focus(c, d);
}

CORBA::Boolean ControllerImpl::receive_focus(Focus_ptr f)
{
  Trace trace("ControllerImpl::receive_focus");
  set_focus(f->device());
  if (f->device() == 0) set(Warsaw::Controller::active);
  return true;
}

void ControllerImpl::lose_focus(Input::Device d)
{
  Trace trace("ControllerImpl::lose_focus");
  clear_focus(d);
  if (d == 0) clear(Warsaw::Controller::active);
}

CORBA::Boolean ControllerImpl::first_focus(Input::Device d)
{
  Trace trace("ControllerImpl::first_focus");
  /*
   * if we have children, ask them if they take the focus...
   */
  {
    Prague::Guard<Mutex> guard(_cmutex);
    for (clist_t::iterator i = _children.begin(); i != _children.end(); ++i)
      if ((*i)->first_focus(d)) return true;
  }
  /*
   * ...else we have to request it ourself
   */
  Controller_var parent = parent_controller();
  if (CORBA::is_nil(parent)) return false;
  return parent->request_focus(Controller_var(_this()), d);
}

CORBA::Boolean ControllerImpl::last_focus(Input::Device d)
{
  Trace trace("ControllerImpl::last_focus");
  /*
   * if we have children, ask them if they take the focus...
   */
  {
    Prague::Guard<Mutex> guard(_cmutex);
    for (clist_t::reverse_iterator i = _children.rbegin(); i != _children.rend(); ++i)
      if ((*i)->last_focus(d)) return true;
  }
  /*
   * ...else we have to request it ourself
   */
  Controller_var parent = parent_controller();
  if (CORBA::is_nil(parent)) return false;
  return parent->request_focus(Controller_var(_this()), d);
}

CORBA::Boolean ControllerImpl::next_focus(Input::Device d)
{
  Trace trace("ControllerImpl::next_focus");
  Warsaw::Controller_var parent = parent_controller();
  if (CORBA::is_nil(parent)) return false;
  /*
   * first locate the next controller in the control graph...
   */
  Warsaw::Controller::Iterator_var iterator = parent->first_child_controller();
  Warsaw::Controller_var next;
  /*
   * set the iterator to refer to 'this' child...
   */
  for (next = iterator->child(); !CORBA::is_nil(next) && !is_identical(next); iterator->next(), next = iterator->child());
  /*
   * 'this' not being contained in the parent's child list indicates an internal error
   */
  assert(!CORBA::is_nil(next));
  iterator->next();
  next = iterator->child();
  iterator->destroy();
  /*
   * ...now try to pass the focus to it...
   */
  if (!CORBA::is_nil(next)) return next->first_focus(d);
  /*
   * ...else pass up to the parent
   */
  else return parent->next_focus(d);
}

CORBA::Boolean ControllerImpl::prev_focus(Input::Device d)
{
  Trace trace("ControllerImpl::prev_focus");
  Warsaw::Controller_var parent = parent_controller();
  if (CORBA::is_nil(parent)) return false;
  /*
   * first locate the previous controller in the control graph...
   */
  Warsaw::Controller::Iterator_var iterator = parent->last_child_controller();
  Warsaw::Controller_var prev;
  /*
   * set the iterator to refer to 'this' child...
   */
  for (prev = iterator->child(); !CORBA::is_nil(prev) && !is_identical(prev); iterator->prev(), prev = iterator->child());
  /*
   * 'this' not being contained in the parent's child list indicates an internal error
   */
  assert(!CORBA::is_nil(prev));
  iterator->prev();
  prev = iterator->child();
  iterator->destroy();
  /*
   * ...now try to pass the focus to it...
   */
  if (!CORBA::is_nil(prev)) return prev->last_focus(d);
  /*
   * ...else pass up to the parent
   */
  else return parent->prev_focus(d);
}

void ControllerImpl::set(Warsaw::Telltale::Mask m)
{
  Trace trace("ControllerImpl::set");
  if (!CORBA::is_nil(_constraint)) _constraint->trymodify(Telltale_var(_this()), m, true);
  else modify(m, true);
}

void ControllerImpl::clear(Warsaw::Telltale::Mask m)
{
  Trace trace("ControllerImpl::clear");
  if (!CORBA::is_nil(_constraint)) _constraint->trymodify(Telltale_var(_this()), m, false);
  else modify(m, false);
}

CORBA::Boolean ControllerImpl::test(Warsaw::Telltale::Mask m)
{
  Prague::Guard<Mutex> guard(_mutex);
  return (_telltale & m) == m;
}

void ControllerImpl::modify(Warsaw::Telltale::Mask m, CORBA::Boolean on)
{
  unsigned long nf = on ? _telltale | m : _telltale & ~m;
  {
    Prague::Guard<Mutex> guard(_mutex);
    if (nf == _telltale) return;
    else _telltale = nf;
  }
  CORBA::Any any;
  any <<= nf;
  notify(any);
}

void ControllerImpl::constraint(TelltaleConstraint_ptr c)
{
  Prague::Guard<Mutex> guard(_mutex);
  _constraint = TelltaleConstraint::_duplicate(c);
}

TelltaleConstraint_ptr ControllerImpl::constraint()
{
  Prague::Guard<Mutex> guard(_mutex);
  return TelltaleConstraint::_duplicate(_constraint);
}

CORBA::Boolean ControllerImpl::handle_positional(PickTraversal_ptr traversal, const Input::Event &event)
{
  Trace trace("ControllerImpl::handle_positional");
  Input::Position position;
  if (Input::get_position(event, position) == -1)
    {
      cerr << "ControllerImpl::handle_positional fatal error : non positional event" << endl;
      return false;
    }
  if (event[0].attr._d() == Input::button)
    {
      const Input::Toggle &toggle = event[0].attr.selection();
      if (toggle.actuation == Input::Toggle::press) press(traversal, event);
      else if (toggle.actuation == Input::Toggle::release) release(traversal, event);
    }
  else if (event[0].attr._d() == Input::positional)
    {
      if (test(Warsaw::Controller::pressed)) drag(traversal, event);
      else move(traversal, event);
    }
  else other(event);
  return true;
}

CORBA::Boolean ControllerImpl::handle_non_positional(const Input::Event &event)
{
  Trace trace("ControllerImpl::handle_non_positional");
  if (event[0].dev != 0 || event[0].attr._d() != Input::key)
    {
      cerr << "ControllerImpl::handleNonPositional fatal error : unknown event" << endl;
      return false;
    }
  if (event[0].attr.selection().actuation != Input::Toggle::press) return false;
  key_press(event);
  return true;
}

bool ControllerImpl::inside(PickTraversal_ptr traversal)
  //. default implementation: use bounding box
{
  return traversal->intersects_allocation();
}

void ControllerImpl::move(PickTraversal_ptr, const Input::Event &)
{
}

void ControllerImpl::press(PickTraversal_ptr traversal, const Input::Event &)
{
  grab(traversal);
  request_focus(Controller_var(_this()), 0);
  set(Warsaw::Controller::pressed);
}

void ControllerImpl::drag(PickTraversal_ptr, const Input::Event &)
{
}

void ControllerImpl::release(PickTraversal_ptr traversal, const Input::Event &)
{
  clear(Warsaw::Controller::pressed);
  ungrab(traversal);
}

void ControllerImpl::double_click(PickTraversal_ptr, const Input::Event &)
{
}

void ControllerImpl::key_press(const Input::Event &event)
{
  Trace trace("ControllerImpl::key_press");
  const Input::Toggle &toggle = event[0].attr.selection();
  switch (toggle.number)
    {
    case Babylon::KEY_CURSOR_LEFT:          // left
      {
	prev_focus(event[0].dev);
	break;
      }
    case Babylon::UC_HORIZONTAL_TABULATION: // tab
    case Babylon::KEY_CURSOR_RIGHT:         // right
      {
	next_focus(event[0].dev);
	break;
      }
    default: break;
    }
}

void ControllerImpl::key_release(const Input::Event &)
{
}

void ControllerImpl::other(const Input::Event &)
{
}

void ControllerImpl::grab(Warsaw::PickTraversal_ptr traversal)
{
  Focus_var focus = traversal->get_focus();
  if (CORBA::is_nil(focus)) return;
  focus->grab();
  _grabs |= 1 << focus->device();
  update_state();
}

void ControllerImpl::ungrab(Warsaw::PickTraversal_ptr traversal)
{
  Focus_var focus = traversal->get_focus();
  if (CORBA::is_nil(focus)) return;
  focus->ungrab();
  _grabs &= ~(1 << focus->device());
  update_state();
}

void ControllerImpl::update_state()
{
}
