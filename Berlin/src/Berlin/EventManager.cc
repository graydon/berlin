/*$Id: EventManager.cc,v 1.5 1999/10/13 21:32:31 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#include "Berlin/Logger.hh"

EventManager::EventManager(ScreenImpl *s)
  : screen(s),
    focus(new FocusImpl(screen))
{}

EventManager::~EventManager() {}
void EventManager::requestFocus(Controller_ptr c) { focus->request(c);}
void EventManager::damage(Region_ptr r) { focus->damage(r);}
void EventManager::dispatch(const Event::Pointer &pointer) { focus->dispatch(pointer);}
void EventManager::dispatch(const Event::Key &) {}

