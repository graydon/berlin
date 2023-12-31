/*$Id: ButtonImpl.cc,v 1.4 1999/10/19 21:07:52 gray Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/Event.hh"
#include "Warsaw/PickTraversal.hh"
#include "Widget/ButtonImpl.hh"

void ButtonImpl::release(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  /*
   * once we have real focus management the command should be executed
   * if we have focus and the Telltale::toggle is to be released... -stefan
   */
  if (inside(traversal) && test(Telltale::toggle))
    {
      CORBA::Any dummy;
      execute(dummy);
    }
  ControllerImpl::release(traversal, pointer);
}
