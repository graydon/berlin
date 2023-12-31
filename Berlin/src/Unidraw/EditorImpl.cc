/*$Id: EditorImpl.cc,v 1.5 2001/04/18 06:07:28 stefan Exp $
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
#include <Warsaw/Choice.hh>
#include <Berlin/RefCountVar.hh>
#include "Unidraw/EditorImpl.hh"
#include "Unidraw/Viewer.hh"

using namespace Prague;
using namespace Warsaw;
using namespace Unidraw;

class EditorImpl::Observer : public ObserverImpl
{
public:
  Observer(EditorImpl *e) : _parent(e) { _parent->_add_ref();}
  ~Observer() { _parent->_remove_ref();}
  void update(const CORBA::Any &any)
  {
    Warsaw::Selection::Item *item;
    if (any >>= item)
      {
	if (item->toggled) _parent->_current = RefCount_var<Tool>::increment(_parent->_tools[item->id]);
      }
    else  std::cerr << "wrong message type" << std::endl;
  }
private:
  EditorImpl *_parent;
};

EditorImpl::EditorImpl(UnidrawKitImpl *unidraw)
  : _unidraw(unidraw),
    _tool_setter(new Observer(this))
{
  _unidraw->_add_ref();
  WidgetKit_var widgets = _unidraw->widgets();
  _choice = widgets->toolbar();
  Warsaw::Selection_var selection = _choice->state();
  selection->attach(Observer_var(_tool_setter->_this()));
}
EditorImpl::~EditorImpl()
{
  _unidraw->_remove_ref();
}
void EditorImpl::append_tool(Tool_ptr tool, Graphic_ptr graphic)
{
  Tag tag = _choice->append_item(graphic);
  _tools[tag] = RefCount_var<Tool>::increment(tool);
}
Unidraw::Tool_ptr EditorImpl::current_tool() { return Unidraw::Tool::_duplicate(_current);}
void EditorImpl::current_tool(Tool_ptr current) { _current = Unidraw::Tool::_duplicate(current);}
Controller_ptr EditorImpl::create_viewer(Unidraw::Model_ptr model, Warsaw::Coord width, Warsaw::Coord height)
{
  FigureKit_var figures = _unidraw->figures();
  ToolKit_var tools = _unidraw->tools();
  Viewer *viewer = new Viewer();
  activate(viewer);
  viewer->init(Editor_var(_this()), model, width, height, figures, tools);
  return viewer->_this();
}

Controller_ptr EditorImpl::toolbar()
{
  return Warsaw::Choice::_duplicate(_choice);
}
