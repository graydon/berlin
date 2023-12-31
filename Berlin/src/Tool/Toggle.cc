/*$Id: Toggle.cc,v 1.7 2000/09/19 21:11:09 stefan Exp $
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
#include "Tool/Toggle.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

Toggle::Toggle(bool f) : ControllerImpl(f) {}
Toggle::~Toggle() {}

void Toggle::press(PickTraversal_ptr traversal, const Input::Event &event)
{
  Trace trace("Toggle::press");
  ControllerImpl::press(traversal, event);
  if (test(Warsaw::Controller::toggled)) clear(Warsaw::Controller::toggled);
  else set(Warsaw::Controller::toggled);
}

void Toggle::release(PickTraversal_ptr traversal, const Input::Event &event)
{
  Trace trace("Toggle::release");
  ControllerImpl::release(traversal, event);
}

void Toggle::keyPress(const Input::Event &event)
{
  Trace trace("Toggle::press");
  const Input::Toggle &toggle = event[0].attr.selection();
  if (toggle.number == 32) // space
    {
      if (test(Warsaw::Controller::toggled)) clear(Warsaw::Controller::toggled);
      else set(Warsaw::Controller::toggled);
    }
  else ControllerImpl::key_press(event);
}
