/*$Id: Dragger.cc,v 1.3 1999/10/19 21:07:52 gray Exp $
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

#include "Berlin/Vertex.hh"
#include "Berlin/Logger.hh"
#include "Widget/Dragger.hh"

Dragger::Dragger(Command_ptr c)
  : ControllerImpl(false), command(Command::_duplicate(c))
{
}

Dragger::~Dragger()
{
}

void Dragger::press(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  ControllerImpl::press(traversal, pointer);
  offset = pointer->location;
}

void Dragger::drag(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  Vertex delta = pointer->location - offset;
  CORBA::Any any;
  any <<= delta;
  command->execute(any);
  offset += delta;
}

void Dragger::release(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  ControllerImpl::release(traversal, pointer);
}
