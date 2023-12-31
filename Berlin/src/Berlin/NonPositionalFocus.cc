/*$Id: NonPositionalFocus.cc,v 1.20 2001/04/18 06:07:26 stefan Exp $
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

#include <Prague/Sys/Profiler.hh>
#include <Prague/Sys/Tracer.hh>
#include "Berlin/NonPositionalFocus.hh"
#include "Berlin/ScreenImpl.hh"
#include "Berlin/PickTraversalImpl.hh"
#include "Berlin/RegionImpl.hh"

using namespace Prague;
using namespace Warsaw;

NonPositionalFocus::NonPositionalFocus(Input::Device d, Controller_ptr root)
  : FocusImpl(d)
{
  _controllers.push_back(Warsaw::Controller::_duplicate(root));
}
NonPositionalFocus::~NonPositionalFocus() {}
void NonPositionalFocus::activate_composite() { _controllers.back()->receive_focus(Focus_var(_this()));}
void NonPositionalFocus::add_filter(Input::Filter_ptr)
{
  // not implemented
}

bool NonPositionalFocus::request(Controller_ptr c)
{
  Trace trace("NonPositionalFocus::request");
  /*
   * brute force method:
   * construct stack of parent controllers and then
   * call lose/receiveFocus as appropriate...
   *
   * a refinement will test in the neighborhood of
   * the old controller holding the focus
   *       -stefan
   */
  std::vector<Controller_var> tmp;
  Controller_var p = Controller::_duplicate(c);
  while (!CORBA::is_nil(p))
    {
      tmp.insert(tmp.begin(), p);
      p = p->parent_controller();
    }
  cstack_t::iterator of = _controllers.begin();
  std::vector<Controller_var>::iterator nf = tmp.begin();
  /*
   * ...skip the unchanged controllers,...
   */
  while (nf != tmp.end() &&
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
  Focus_var __this = _this ();
  for (; nf != tmp.end(); ++nf)
    {
      (*nf)->receive_focus (__this);
      _controllers.push_back(Warsaw::Controller::_duplicate(*nf));
    }
  return true;
}

/*
 * Dispatch a non-positional event. Try the controllers in turn, until
 * one handles the event. Remove from the list non-existent
 * controllers.
 */
void NonPositionalFocus::dispatch(Input::Event &event)
{
  Trace trace("NonPositionalFocus::dispatch");
  CORBA::Boolean done = false;
  Prague::Guard<Mutex> guard(_mutex);
  for (int i = _controllers.size() - 1; i >= 0 && !done; --i)
    {
      try { done = _controllers [i]->handle_non_positional(event);}
      catch (const CORBA::OBJECT_NOT_EXIST &) { _controllers.resize (i);}
      catch (const CORBA::COMM_FAILURE &) { _controllers.resize (i);}
    }
}
