/*$Id: AllocationImpl.hh,v 1.9 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _AllocationImpl_hh
#define _AllocationImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/ServantBase.hh>
#include <Berlin/Provider.hh>
#include <vector>

class RegionImpl;
class TransformImpl;

class AllocationImpl : public virtual POA_Warsaw::Allocation,
                       public virtual ServantBase
{
  struct State
  {
    RegionImpl        *allocation;
    TransformImpl     *transformation;
    Warsaw::Screen_var root;
  };
  typedef std::vector<State> list_t;
  friend class Provider<AllocationImpl>;
public:
  AllocationImpl();
  ~AllocationImpl();
  void add(Warsaw::Region_ptr, Warsaw::Screen_ptr);
  CORBA::Long size();
  Warsaw::Allocation::Info *get(CORBA::Long);
  void clear();
private:
  bool _active : 1;
  list_t _list;
};

#endif 
