/*$Id: DesktopImpl.hh,v 1.2 1999/11/06 20:23:07 stefan Exp $
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
#ifndef _DesktopImpl_hh
#define _DesktopImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Desktop.hh>
#include <Berlin/ControllerImpl.hh>

class WindowImpl;

class DesktopImpl : implements(Desktop), public ControllerImpl
{
 public:
  DesktopImpl();
  virtual ~DesktopImpl();
  virtual void body(Graphic_ptr) {}
  virtual Graphic_ptr body() { return CORBA::is_nil(stage) ? Stage::_nil() : Stage::_duplicate(stage);}
//   virtual void draw(DrawTraversal_ptr);

  Region_ptr bbox() { return stage->bbox();}
  long layers() { return stage->layers();}
  StageHandle_ptr layer(Index l) { return stage->layer(l);}
  void begin() { stage->begin();}
  void end() { stage->end();}

  StageHandle_ptr insert(Graphic_ptr g, const Vertex &p, const Vertex &s, Stage::Index l)
    {
      return stage->insert(g, p, s, l);
    }
  void remove(StageHandle_ptr h) { stage->remove(h);}

  void init(Stage_ptr);
 private:
  Stage_var stage;
};

#endif /* _DesktopImpl_hh */
