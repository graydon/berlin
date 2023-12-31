/*$Id: Event.hh,v 1.6 2000/10/20 17:45:01 stefan Exp $
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
#include <Warsaw/config.hh>
#include <Warsaw/Input.hh>

namespace Warsaw
{
namespace Input
{

inline int get_position(const Event &event, Input::Position &position)
{
  Input::Device device = event[0].dev;
  for (size_t i = 0; i != event.length(); i++)
    if (event[i].dev != device) return -1;
    else if (event[i].attr._d() == Input::positional)
      {
	position = event[i].attr.location();
	return i;
      }
  return -1;
}

};
};
