/*$Id: ScreenManager.cc,v 1.60 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include "Berlin/ScreenManager.hh"
#include "Berlin/ScreenImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/EventManager.hh"
#include <Prague/Sys/FdSet.hh>
#include <Prague/Sys/Time.hh>
#include <Prague/Sys/Profiler.hh>
#include "Berlin/Logger.hh"
#include <Warsaw/IO.hh>

using namespace Prague;
using namespace Warsaw;

ScreenManager::ScreenManager(Graphic_ptr g, EventManager *em, DrawingKit_ptr d)
  : _screen(g), _emanager(em), _drawing(DrawingKit::_duplicate(d)), _drawable(Console::drawable())
{
}

ScreenManager::~ScreenManager() {}
void ScreenManager::damage(Region_ptr r)
{
  Trace trace("ScreenManager::damage");
  Prague::Guard<Mutex> guard(_mutex);
  _theDamage->merge_union(r);
  Console::wakeup();
}

void ScreenManager::repair()
{
  Trace trace("ScreenManager::repair");
  //   Profiler prf("ScreenManager::repair");
  _mutex.lock();
  _tmpDamage->copy(Region_var(_theDamage->_this()));
  _theDamage->clear();
  _mutex.unlock();
  _emanager->restore(Region_var(_tmpDamage->_this()));
  _traversal->init();
  _drawable->init();
  try { _screen->traverse(Traversal_var(_traversal->_this()));}
  catch (const CORBA::OBJECT_NOT_EXIST &) { cerr << "ScreenManager: warning: corrupt scene graph !" << endl;}
  catch (const CORBA::BAD_PARAM &) { cerr << "ScreenManager: caught bad parameter" << endl;}
  _drawable->finish();
  _traversal->finish();
  _drawing->flush();
  {
    //     Profiler prf("Drawable::flush");
    Vertex l,u;
    _tmpDamage->bounds(l,u);
    double xres = _drawing->resolution(xaxis);
    double yres = _drawing->resolution(yaxis);
    l.x *= xres;
    u.x *= xres;
    l.y *= yres;
    u.y *= yres;
    _drawable->flush(static_cast<long>(l.x), static_cast<long>(l.y), static_cast<long>(u.x - l.x), static_cast<long>(u.y - l.y));
  }
  _emanager->damage(Region_var(_tmpDamage->_this()));
}

void ScreenManager::run()
{
  _theDamage = new RegionImpl;
  _tmpDamage = new RegionImpl;
  _traversal = new DrawTraversalImpl(_screen,
				     Region_var(_tmpDamage->_this()),
				     Transform::_nil(),
				     _drawing);
  Prague::Time last;
  while (true)
    {
      _mutex.lock();
      bool haveDamage = _theDamage->defined();
      _mutex.unlock();
      if (haveDamage)
	{
	  Prague::Time current = Prague::Time::currentTime();
	  if (current > last + Prague::Time(33))
	    {
	      repair();
	      last = current;
	    }
	}
      _emanager->next_event();
    }
}
