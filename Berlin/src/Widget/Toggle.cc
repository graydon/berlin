/*$Id: Toggle.cc,v 1.2 1999/11/06 20:23:08 stefan Exp $
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
#include "Widget/Toggle.hh"
#include "Berlin/Logger.hh"

using namespace Prague;

Toggle::Toggle(bool f) : ControllerImpl(f) {}
Toggle::~Toggle() {}

void Toggle::press(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  SectionLog section("Toggle::press");
  ControllerImpl::press(traversal, pointer);
  if (test(Telltale::chosen)) clear(Telltale::chosen);
  else set(Telltale::chosen);
}

void Toggle::release(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  SectionLog section("Toggle::release");
  ControllerImpl::release(traversal, pointer);
}
