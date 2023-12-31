/*$Id: ScreenImpl.cc,v 1.19 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#include "Berlin/Logger.hh"
#include "Drawing/openGL/GLDrawingKit.hh"

#include "Warsaw/Traversal.hh"
#include <iostream>

ScreenImpl::ScreenImpl(GLDrawingKit *d)
  : ControllerImpl(false), drawing(d)
{
  emanager = new EventManager(this);
  smanager = new ScreenManager(this, emanager, drawing);
  region = new RegionImpl;
  region->valid = true;
  region->lower.x = region->lower.y = region->lower.z = 0;
  region->upper.x = drawing->width(), region->upper.y = drawing->height(), region->upper.z = 0;
}

ScreenImpl::~ScreenImpl()
{
  delete smanager;
  delete emanager;
}

void ScreenImpl::pick(PickTraversal_ptr traversal)
{
  SectionLog section("ScreenImpl::pick");
  if (traversal->intersectsAllocation())
    {
      traversal->enterController(Controller_var(_this()));
      MonoGraphic::traverse(traversal);
      if (!traversal->picked()) traversal->hit();
      traversal->leaveController();
    }
  else cout << "no intersection !" << endl;
}

void ScreenImpl::allocations(Allocation_ptr allocation)
{
  allocation->add(Region_var(region->_this()), Screen_var(_this()));
}

void ScreenImpl::damage(Region_ptr region) { smanager->damage(region);}

CORBA::Boolean ScreenImpl::handle(PickTraversal_ptr traversal, const CORBA::Any &any)
{
  return false;
}

DrawingKit_ptr ScreenImpl::kit() { return drawing->_this();}

ScreenManager *ScreenImpl::manager() { return smanager;}
Region_ptr ScreenImpl::getRegion() {return region->_this();}

Coord ScreenImpl::width() { return region->upper.x;}
Coord ScreenImpl::height() { return region->upper.y;}
