/*$Id: UnidrawKitImpl.cc,v 1.5 2001/04/18 06:07:28 stefan Exp $
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
#include <Warsaw/resolve.hh>
#include "Unidraw/UnidrawKitImpl.hh"
#include "Unidraw/ViewImpl.hh"
#include "Unidraw/SelectTool.hh"
#include "Unidraw/EditorImpl.hh"

using namespace Prague;
using namespace Warsaw;
// using namespace Unidraw;

UnidrawKitImpl::UnidrawKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
UnidrawKitImpl::~UnidrawKitImpl() {}
void UnidrawKitImpl::bind(ServerContext_ptr context)
{
  Trace trace("UnidrawKitImpl::bind");
  KitImpl::bind(context);
  Warsaw::Kit::PropertySeq props;
  props.length(0);
  _figures = resolve_kit<FigureKit>(context, "IDL:Warsaw/FigureKit:1.0", props);
  _tools   = resolve_kit<ToolKit>(context, "IDL:Warsaw/ToolKit:1.0", props);
  _widgets = resolve_kit<WidgetKit>(context, "IDL:Warsaw/WidgetKit:1.0", props);
}

Unidraw::Tool_ptr UnidrawKitImpl::select_tool()
{
  Graphic_var box = _figures->rectangle(0., 0., 1., 1.);
  SelectTool *tool = new SelectTool(box);
  activate(tool);
  return tool->_this();
}

Unidraw::Editor_ptr UnidrawKitImpl::create_editor()
{
  EditorImpl *editor = new EditorImpl(this);
  activate(editor);
  return editor->_this();
}

Unidraw::View_ptr UnidrawKitImpl::create_view(Graphic_ptr g, Unidraw::Model_ptr m)
{
  UViewImpl *view = new UViewImpl(m);
  activate(view);
  view->body(g);
  return view->_this();
}

Warsaw::FigureKit_ptr UnidrawKitImpl::figures() { return RefCount_var<Warsaw::FigureKit>::increment(_figures);}
Warsaw::ToolKit_ptr UnidrawKitImpl::tools() { return RefCount_var<Warsaw::ToolKit>::increment(_tools);}
Warsaw::WidgetKit_ptr UnidrawKitImpl::widgets() { return RefCount_var<Warsaw::WidgetKit>::increment(_widgets);}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "UnidrawKitImpl"};
  return new KitFactoryImpl<UnidrawKitImpl>("IDL:Unidraw/UnidrawKit:1.0", properties, 1);
} 
