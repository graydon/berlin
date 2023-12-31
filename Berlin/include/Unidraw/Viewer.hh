/*$Id: Viewer.hh,v 1.2 2000/12/21 21:05:43 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _View_hh
#define _View_hh

#include <Warsaw/config.hh>
#include <Warsaw/FigureKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/UnidrawKit.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/ImplVar.hh>

class Viewer : public ControllerImpl
{
public:
  Viewer();
  virtual ~Viewer();
  void init(Unidraw::Editor_ptr, Unidraw::Model_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::FigureKit_ptr, Warsaw::ToolKit_ptr);
  virtual void append_graphic(Warsaw::Graphic_ptr);
  virtual void prepend_graphic(Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic::Iterator_ptr first_child_graphic();
  virtual Warsaw::Graphic::Iterator_ptr last_child_graphic();
protected:
  virtual void press(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void drag(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void move(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void release(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
private:
  void add(Warsaw::Coord, Warsaw::Coord);
  Unidraw::Editor_var      _editor;
  Unidraw::Tool_var        _active;
  Warsaw::Graphic_var      _root;
};

#endif
