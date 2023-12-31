/*$Id: ScreenManager.hh,v 1.10 1999/09/30 17:23:33 gray Exp $
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
#ifndef _ScreenManager_hh
#define _ScreenManager_hh

#include "Warsaw/config.hh"

extern "C" {
#include <ggi/ggi.h>
}

#include "Warsaw/Region.hh"
#include "Warsaw/Event.hh"
#include "Prague/Sys/Thread.hh"
#include <vector>

class GLDrawingKit;
class Pointer;
class ScreenImpl;
class EventManager;
class RegionImpl;

class ScreenManager
{
  typedef vector<RegionImpl *> dlist_t;
public:
  ScreenManager(ScreenImpl *, EventManager *, GLDrawingKit *);
  ~ScreenManager();
  void damage(Region_ptr);
  void repair();
  void nextEvent();
  void run();
private:
  long ptrPositionX;
  long ptrPositionY;
  ScreenImpl *screen;
  EventManager *emanager;
  GLDrawingKit *drawing;
  Pointer *pointer;
  ggi_visual_t visual;
  dlist_t damages;
  Prague::Mutex mutex;
};

#endif /* _ScreenManager_hh */
