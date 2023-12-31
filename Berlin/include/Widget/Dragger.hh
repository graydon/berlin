/*$Id: Dragger.hh,v 1.2 1999/10/19 21:07:52 gray Exp $
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
#ifndef _Dragger_hh
#define _Dragger_hh

#include "Warsaw/config.hh"
#include "Warsaw/Command.hh"
#include "Widget/Bevel.hh"
#include "Berlin/ControllerImpl.hh"

class Dragger : public ControllerImpl
{
public:
  Dragger(Command_ptr);
  virtual ~Dragger();
//protected:
  virtual void press(PickTraversal_ptr, const Event::Pointer *);
  virtual void drag(PickTraversal_ptr, const Event::Pointer *);
  virtual void release(PickTraversal_ptr, const Event::Pointer *);
private:
  Vertex offset;
  Command_var command;
};

#endif /* _Dragger_hh */
