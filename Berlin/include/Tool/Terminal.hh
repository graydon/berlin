/*$Id: Terminal.hh,v 1.4 2000/11/15 17:38:39 stefan Exp $
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
#ifndef _Terminal_hh
#define _Terminal_hh

#include <Warsaw/config.hh>
#include <Warsaw/StreamBuffer.hh>
#include <Berlin/ControllerImpl.hh>

class Terminal : public ControllerImpl
{
 public:
  Terminal(Warsaw::StreamBuffer_ptr b) : ControllerImpl(false), buffer(Warsaw::StreamBuffer::_duplicate(b)) {}
  ~Terminal() {}
// protected:
  virtual void key_press(const Warsaw::Input::Event &);
 private:
  Warsaw::StreamBuffer_var buffer;
};

#endif
