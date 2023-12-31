/*$Id: EventManager.cc,v 1.28 2000/11/17 19:40:00 stefan Exp $
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

#include "Berlin/EventManager.hh"
#include "Berlin/ScreenImpl.hh"
#include "Berlin/NonPositionalFocus.hh"
#include "Berlin/PositionalFocus.hh"
#include "Berlin/Vertex.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

inline void EventManager::activate(FocusImpl *focus)
{
  Prague::Trace trace("EventManager::activate");
  PortableServer::POA_var poa = focus->_default_POA();
  PortableServer::ObjectId *oid = poa->activate_object(focus);
  focus->_remove_ref();
  delete oid;
  focus->activate_composite();
}

inline void EventManager::deactivate(FocusImpl *focus)
{
  Prague::Trace trace("EventManager::deactivate");
  PortableServer::POA_var poa = focus->_default_POA();
  PortableServer::ObjectId *oid = poa->servant_to_id(focus);
  poa->deactivate_object(*oid);
  delete oid;
}

EventManager::EventManager(Controller_ptr root, Region_ptr allocation)
{
  Trace trace("EventManager::EventManager");
  _drawable = Console::drawable();
  FocusImpl *keyboard = new NonPositionalFocus(0, root);
  FocusImpl *mouse = new PositionalFocus(1, root, allocation);
  activate(keyboard);
  activate(mouse);
  _foci.push_back(keyboard);
  _foci.push_back(mouse);
}

EventManager::~EventManager()
{
  for (flist_t::iterator i = _foci.begin(); i != _foci.end(); i++) deactivate(*i);
}

bool EventManager::request_focus(Controller_ptr c, Input::Device d)
{
  Trace trace("EventManager::request_focus");
  if (d < _foci.size()) return _foci[d]->request(c);
  return false;
}

void EventManager::next_event()
{
  Trace trace("EventManager::next_event");
  Input::Event *e = Console::next_event();
  if (!e) return; // repair
  Input::Event_var event(e);
  /*
   * the first item determines which focus to send this event to
   */
  try { if (event->length()) _foci[event[0].dev]->dispatch(event);}
  catch (const CORBA::OBJECT_NOT_EXIST &) { cerr << "EventManager: warning: corrupt scene graph !" << endl;}
  catch (const CORBA::BAD_PARAM &) { cerr << "EventManager: caught bad parameter" << endl;}
}

void EventManager::restore(Region_ptr r)
{
  for (flist_t::iterator i = _foci.begin(); i != _foci.end(); i++) (*i)->restore(r);
}

void EventManager::damage(Region_ptr r)
{
  for (flist_t::iterator i = _foci.begin(); i != _foci.end(); i++) (*i)->damage(r);
}

