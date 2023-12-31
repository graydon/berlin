/*$Id: EventManager.hh,v 1.3 1999/10/13 21:32:31 gray Exp $
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
#ifndef _EventManager_hh
#define _EventManager_hh

#include "Warsaw/config.hh"
#include "Warsaw/Event.hh"
#include "Warsaw/Controller.hh"
#include <Berlin/ScreenImpl.hh>
#include <Berlin/FocusImpl.hh>
#include <Berlin/ImplVar.hh>

class EventManager
{
public:
  EventManager(ScreenImpl *);
  ~EventManager();
  void requestFocus(Controller_ptr);
  void damage(Region_ptr);
  void dispatch(const Event::Pointer &);
  void dispatch(const Event::Key &);
private:
  ScreenImpl *screen;
  Impl_var<FocusImpl>  focus;
};

#endif /* _EventManager_hh */
