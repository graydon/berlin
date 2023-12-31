/*$Id: Dragger.hh,v 1.3 2000/09/05 21:12:00 stefan Exp $
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
#ifndef _Dragger_hh
#define _Dragger_hh

#include <Warsaw/config.hh>
#include <Warsaw/Command.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/RefCountVar.hh>

class Dragger : public ControllerImpl
{
public:
  Dragger(Warsaw::Command_ptr);
  virtual ~Dragger();
//protected:
  virtual void press(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void drag(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void release(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
private:
  Warsaw::Vertex offset;
  Warsaw::Command_var command;
};

#endif
