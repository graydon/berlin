/*$Id: AllocationImpl.hh,v 1.3 1999/05/31 19:46:27 gray Exp $
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
#ifndef _AllocationImpl_hh
#define _AllocationImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <vector>

class RegionImpl;
class TransformImpl;

class AllocationImpl : implements(Allocation)
{
  struct State
  {
    RegionImpl *allocation;
    TransformImpl *transformation;
    Screen_var root;
  };
  typedef vector<State> list_t;
public:
  AllocationImpl();
  ~AllocationImpl();
  void add(Region_ptr, Screen_ptr);
  CORBA::Long size();
  Allocation::Info *get(CORBA::Long);
private:
  list_t list;
};

#endif /* _AllocationImpl_hh */
