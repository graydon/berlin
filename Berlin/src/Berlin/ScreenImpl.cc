/*$Id: ScreenImpl.cc,v 1.31 2000/10/31 18:15:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#include "Berlin/ScreenImpl.hh"
#include "Berlin/ScreenManager.hh"
#include "Berlin/EventManager.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Console.hh"
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/Traversal.hh>
#include <iostream>

using namespace Prague;
using namespace Warsaw;

ScreenImpl::ScreenImpl()
  : ControllerImpl(false),
    __this(POA_Warsaw::Screen::_this()),
    _emanager(0),
    _smanager(0),
    _region(new RegionImpl())
{
  Trace trace("ScreenImpl::ScreenImpl");
  _region->valid = true;
  _region->lower.x = _region->lower.y = _region->lower.z = 0;
  _region->upper.x = Console::drawable()->width()/Console::drawable()->resolution(xaxis);
  _region->upper.y = Console::drawable()->height()/Console::drawable()->resolution(yaxis);
  _region->upper.z = 0;
}
ScreenImpl::~ScreenImpl() {}
void ScreenImpl::bind_managers(EventManager *e, ScreenManager *s)
{
  _emanager = e;
  _smanager = s;
}

void ScreenImpl::pick(PickTraversal_ptr traversal)
{
  Trace trace("ScreenImpl::pick");
  if (traversal->intersects_allocation())
    {
      traversal->enter_controller(__this);
      MonoGraphic::traverse(traversal);
      if (!traversal->picked()) traversal->hit();
      traversal->leave_controller();
    }
  else cout << "no intersection !" << endl;
}

void ScreenImpl::allocations(Allocation_ptr allocation)
{
  allocation->add(Region_var(_region->_this()), __this);
}

void ScreenImpl::damage(Region_ptr region) { _smanager->damage(region);}
bool ScreenImpl::request_focus(Controller_ptr c, Input::Device d) { return _emanager->request_focus(c, d);}
Region_ptr ScreenImpl::allocation() { return _region->_this();}
Coord ScreenImpl::width() { return _region->upper.x;}
Coord ScreenImpl::height() { return _region->upper.y;}
