/*$Id: EventManager.hh,v 1.16 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _EventManager_hh
#define _EventManager_hh

#include <Warsaw/config.hh>
#include <Warsaw/Input.hh>
#include <Warsaw/Controller.hh>
#include <Berlin/ScreenImpl.hh>
#include <Berlin/FocusImpl.hh>
#include <Berlin/Console.hh>
#include <Berlin/ImplVar.hh>
#include <vector>

//. synthesize events according to a global device and event descriptor map
//. for now, device 0 is the keyboard, device 1 the mouse
class EventManager
{
  typedef std::vector<FocusImpl *> flist_t;
public:
  EventManager(Warsaw::Controller_ptr, Warsaw::Region_ptr);
  ~EventManager();
  bool request_focus(Warsaw::Controller_ptr, Warsaw::Input::Device);
  void next_event();
  void restore(Warsaw::Region_ptr);
  void damage(Warsaw::Region_ptr);
private:
  static void activate(FocusImpl *);
  static void deactivate(FocusImpl *);
  Console::Drawable *_drawable;
  flist_t            _foci;
};

#endif 
