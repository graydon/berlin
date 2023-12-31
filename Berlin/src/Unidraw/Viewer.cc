/*$Id: Viewer.cc,v 1.2 2000/12/21 21:05:44 stefan Exp $
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
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/IO.hh>
#include <Warsaw/PickTraversal.hh>
#include <Berlin/RefCountVar.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/Requestor.hh>
#include <Berlin/Event.hh>
#include "Unidraw/Viewer.hh"

using namespace Prague;
using namespace Warsaw;
using namespace Unidraw;

Viewer::Viewer() : ControllerImpl(false) {}
Viewer::~Viewer() {}

void Viewer::init(Editor_ptr editor, Model_ptr model, Coord width, Coord height, FigureKit_ptr figures, ToolKit_ptr tools)
{
  _editor = RefCount_var<Editor>::increment(editor);
  Requestor *requestor = new Requestor(0.5, 0.5, width, height);
  activate(requestor);
  _root = figures->group();
  requestor->body(_root);
  ToolKit::FrameSpec background;
  Color white = {1., 1., 1., 1.};
  background.foreground(white);
  body(Warsaw::Graphic_var(tools->frame(Warsaw::Graphic_var(requestor->_this()), 20., background, true)));
  if (!CORBA::is_nil(model))
    {
      Graphic_var view = model->create_view();
      _root->append_graphic(view);
    }
}

void Viewer::append_graphic(Graphic_ptr c)
{
  _root->append_graphic(c);
}

void Viewer::prepend_graphic(Graphic_ptr c)
{
  _root->prepend_graphic(c);
}

Warsaw::Graphic::Iterator_ptr Viewer::first_child_graphic()
{
  return _root->first_child_graphic();
}

Warsaw::Graphic::Iterator_ptr Viewer::last_child_graphic()
{
  return _root->last_child_graphic();
}

void Viewer::press(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  Trace trace("Viewer::press");
  bool ok = false;
  if (CORBA::is_nil(_active))
    {
      _active = _editor->current_tool();
      if (CORBA::is_nil(_active)) return;
      ok = _active->grasp(Controller_var(_this()), traversal, event);
    }
  else ok = _active->manipulate(traversal, event);
  if (!ok && !CORBA::is_nil(_active))
    {
      Unidraw::Command_var command = _active->effect(traversal, event);
      command->execute();
      _active = Unidraw::Tool::_nil();
    }
  ControllerImpl::press(traversal, event);
}

void Viewer::drag(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  Trace trace("Viewer::drag");
  if (CORBA::is_nil(_active)) return;
  bool ok = _active->manipulate(traversal, event);
  if (!ok)
    {
      Unidraw::Command_var command = _active->effect(traversal, event);
      command->execute();
      _active = Unidraw::Tool::_nil();
    }
}

void Viewer::move(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  Viewer::drag(traversal, event);
}

void Viewer::release(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  Trace trace("Viewer::release");
  if (CORBA::is_nil(_active))
    {
      ControllerImpl::release(traversal, event);
      return;
    }
  bool ok = _active->manipulate(traversal, event);
  if (!ok)
    {
      Unidraw::Command_var command = _active->effect(traversal, event);
      command->execute();
      _active = Unidraw::Tool::_nil();
    }
  ControllerImpl::release(traversal, event);
}
