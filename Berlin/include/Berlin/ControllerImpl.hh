/*$Id: ControllerImpl.hh,v 1.21 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _ControllerImpl_hh
#define _ControllerImpl_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Controller.hh>
#include <Warsaw/Input.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/PickTraversal.hh>
#include <Berlin/SubjectImpl.hh>
#include <Berlin/MonoGraphic.hh>
#include <Berlin/RefCountVar.hh>
#include <vector>

class ControllerImpl : public virtual POA_Warsaw::Controller,
                       public MonoGraphic,
                       public SubjectImpl
{
  typedef std::vector<RefCount_var<Warsaw::Controller> > clist_t;
  class Iterator;
  friend class Iterator;
public:
  ControllerImpl(bool);
  virtual ~ControllerImpl();
  virtual void deactivate();

  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);

  virtual void append_controller(Warsaw::Controller_ptr);
  virtual void prepend_controller(Warsaw::Controller_ptr);
  virtual void remove_controller(Warsaw::Controller_ptr);
  virtual void set_parent_controller(Warsaw::Controller_ptr);
  virtual void remove_parent_controller();

  virtual Warsaw::Controller_ptr parent_controller();
  virtual Warsaw::Controller::Iterator_ptr first_child_controller();
  virtual Warsaw::Controller::Iterator_ptr last_child_controller();

  virtual CORBA::Boolean request_focus(Warsaw::Controller_ptr, Warsaw::Input::Device);
  virtual CORBA::Boolean receive_focus(Warsaw::Focus_ptr);
  virtual void lose_focus(Warsaw::Input::Device);

  virtual CORBA::Boolean first_focus(Warsaw::Input::Device);
  virtual CORBA::Boolean last_focus(Warsaw::Input::Device);
  virtual CORBA::Boolean next_focus(Warsaw::Input::Device);
  virtual CORBA::Boolean prev_focus(Warsaw::Input::Device);
  virtual CORBA::Boolean handle_positional(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual CORBA::Boolean handle_non_positional(const Warsaw::Input::Event &);

  virtual void set(Warsaw::Telltale::Mask);
  virtual void clear(Warsaw::Telltale::Mask);
  virtual CORBA::Boolean test(Warsaw::Telltale::Mask);
  virtual void modify(Warsaw::Telltale::Mask, CORBA::Boolean);
  virtual void constraint(Warsaw::TelltaleConstraint_ptr);
  virtual Warsaw::TelltaleConstraint_ptr constraint();
protected:
  virtual bool inside(Warsaw::PickTraversal_ptr);
  virtual void move(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void press(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void drag(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void release(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void double_click(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void key_press(const Warsaw::Input::Event &);
  virtual void key_release(const Warsaw::Input::Event &);
  virtual void other(const Warsaw::Input::Event &);
  void grab(Warsaw::PickTraversal_ptr);
  void ungrab(Warsaw::PickTraversal_ptr);
  bool grabbed(Warsaw::Input::Device d) { return _grabs & (1 << d);}
  void set_focus(Warsaw::Input::Device d) { _focus |= 1 << d; update_state();}
  void clear_focus(Warsaw::Input::Device d) { _focus &= ~(1 << d); update_state();}
  virtual void update_state();
private:
  Warsaw::Controller_var _parent;
  clist_t _children;
  CORBA::ULong _telltale;
  CORBA::ULong _focus;
  CORBA::ULong _grabs;
  bool _transparent;
  Warsaw::TelltaleConstraint_var _constraint;
  Prague::Mutex _pmutex; // for the parent link
  Prague::Mutex _cmutex; // for the children links
  Prague::Mutex _mutex;  // for the state
};

#endif 
