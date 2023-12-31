/*$Id: DesktopKitImpl.cc,v 1.30 2001/04/24 05:04:49 stefan Exp $
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

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/resolve.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/CommandImpl.hh>
#include <Berlin/RefCountVar.hh>
#include "Desktop/DesktopKitImpl.hh"
#include "Desktop/WindowImpl.hh"
#include "Desktop/Pulldown.hh"
#include "Desktop/Manipulator.hh"

using namespace Prague;
using namespace Warsaw;

DesktopKitImpl::DesktopKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
DesktopKitImpl::~DesktopKitImpl() {}
void DesktopKitImpl::bind(ServerContext_ptr context)
{
  Trace trace("DesktopKitImpl::bind");
  KitImpl::bind(context);
  CORBA::Object_var object = context->get_singleton("IDL:Warsaw/Desktop:1.0");
  _desktop = Desktop::_narrow(object);
  _desktop->increment();
  Warsaw::Kit::PropertySeq props;
  props.length(0);
  _layout = resolve_kit<LayoutKit>(context, "IDL:Warsaw/LayoutKit:1.0", props);
  _tool   = resolve_kit<ToolKit>(context, "IDL:Warsaw/ToolKit:1.0", props);
  _widget = resolve_kit<WidgetKit>(context, "IDL:Warsaw/WidgetKit:1.0", props);
}

Desktop_ptr DesktopKitImpl::desk()
{
  return RefCount_var<Warsaw::Desktop>::increment(_desktop);
}


Window_ptr DesktopKitImpl::shell(Controller_ptr g)
{
  Trace trace("DesktopKitImpl::shell");
  WindowImpl *window = new WindowImpl;
  activate(window);
  Window_var wptr = window->_this();
  Graphic::Requisition req;
  req.x.defined = true;
  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = _layout->fill();
  req.x.align = 0.;
  req.y.defined = true;
  req.y.minimum = 200.;
  req.y.natural = 200.;
  req.y.maximum = 200.;
  req.y.align = 0;
  Command_var mover = move(wptr);
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);
  RefCount_var<Graphic> tbframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> tbdragger = _tool->dragger(tbframe, mover);

  req.x.minimum = 200.;
  req.x.natural = 200.;
  req.x.maximum = 200.;
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var lresize = move_resize(wptr, 1.0, 0.0, Window::left|Window::bottom);
  RefCount_var<Graphic> lframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> ldragger = _tool->dragger(lframe, lresize);

  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = _layout->fill();
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var bresize = move_resize(wptr, 0.0, 0.0, Window::bottom);
  RefCount_var<Graphic> bframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> bdragger = _tool->dragger(bframe, bresize);

  req.x.minimum = 200.;
  req.x.natural = 200.;
  req.x.maximum = 200.;
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var rresize = move_resize(wptr, 0.0, 0.0, Window::right|Window::bottom);
  RefCount_var<Graphic> rframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> rdragger = _tool->dragger(rframe, rresize);

  RefCount_var<Graphic> vbox = _layout->vbox();
  RefCount_var<Graphic> hbox = _layout->hbox();
  hbox->append_graphic(ldragger);
  hbox->append_graphic(bdragger);
  hbox->append_graphic(rdragger);
  vbox->append_graphic(tbdragger);
  vbox->append_graphic(RefCount_var<Graphic>(_layout->align(g, 0., 0.)));
  vbox->append_graphic(hbox);
  RefCount_var<Graphic> background = _tool->rgb(vbox, 0.7, 0.7, 0.7);
  wptr->body(background);
  /*
   * FIXME: we need to take care to include the window control elements 
   *        into the control graph too...
   *        - stefan
   */
  wptr->append_controller(g);
  window->insert(_desktop);
  _desktop->append_controller(wptr);
  wptr->mapped(true);
  return wptr._retn();
}

Window_ptr DesktopKitImpl::transient(Controller_ptr g)
{
  Trace trace("DesktopKitImpl::transient");
  WindowImpl *window = new WindowImpl();
  activate(window);
  Window_var wptr = window->_this();
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);

  Graphic::Requisition req;
  req.x.defined = true;
  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = _layout->fill();
  req.x.align = 0.;
  req.y.defined = true;
  req.y.minimum = 200.;
  req.y.natural = 200.;
  req.y.maximum = 200.;
  req.y.align = 0;
  Command_var mover = move(wptr);
  RefCount_var<Graphic> tbframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> tbdragger = _tool->dragger(tbframe, mover);

  req.x.minimum = 200.;
  req.x.natural = 200.;
  req.x.maximum = 200.;
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var lresize = move_resize(wptr, 1.0, 0.0, Window::left|Window::bottom);
  RefCount_var<Graphic> lframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> ldragger = _tool->dragger(lframe, lresize);

  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = _layout->fill();
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var bresize = move_resize(wptr, 0.0, 0.0, Window::bottom);
  RefCount_var<Graphic> bframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> bdragger = _tool->dragger(bframe, bresize);

  req.x.minimum = 200.;
  req.x.natural = 200.;
  req.x.maximum = 200.;
  req.y.minimum = 40.;
  req.y.natural = 40.;
  req.y.maximum = 40.;
  Command_var rresize = move_resize(wptr, 0.0, 0.0, Window::right|Window::bottom);
  RefCount_var<Graphic> rframe = _tool->frame(RefCount_var<Graphic>(_layout->glue_requisition(req)), 10., spec, true);
  RefCount_var<Graphic> rdragger = _tool->dragger(rframe, rresize);

  RefCount_var<Graphic> vbox = _layout->vbox();
  RefCount_var<Graphic> hbox = _layout->hbox();
  hbox->append_graphic(ldragger);
  hbox->append_graphic(bdragger);
  hbox->append_graphic(rdragger);
  vbox->append_graphic(tbdragger);
  vbox->append_graphic(RefCount_var<Graphic>(_layout->align(g, 0., 0.)));
  vbox->append_graphic(hbox);
  RefCount_var<Graphic> background = _tool->rgb(vbox, 0.7, 0.7, 0.7);
  wptr->body(background);
  /*
   * FIXME: we need to take care to include the window control elements 
   *        into the control graph too...
   *        - stefan
   */
  wptr->append_controller(g);
  window->insert(_desktop);
  _desktop->append_controller(wptr);
  return wptr._retn();
}

Window_ptr DesktopKitImpl::pulldown(Controller_ptr g)
{
  Trace trace("DesktopKitImpl::pulldown");
  Pulldown *menu = new Pulldown();
  activate(menu);
  Window_var wptr = menu->_this();
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);

  RefCount_var<Graphic> outset = _tool->frame(g, 20., spec, true);
  RefCount_var<Graphic> background = _tool->rgb(outset, 0.7, 0.7, 0.7);
  wptr->body(background);
  wptr->append_controller(g);
  menu->insert(_desktop);
  _desktop->append_controller(wptr);
  return wptr._retn();
}

Command_ptr DesktopKitImpl::move(Warsaw::Window_ptr window)
{
  Manipulator *manipulator = new Mover(window);
  activate(manipulator);
  return manipulator->_this();
}

Command_ptr DesktopKitImpl::resize(Warsaw::Window_ptr window)
{
  Manipulator *manipulator = new Resizer(window);
  activate(manipulator);
  return manipulator->_this();
}

Command_ptr DesktopKitImpl::move_resize(Warsaw::Window_ptr window, Warsaw::Alignment x, Warsaw::Alignment y, CORBA::Short border)
{
  Manipulator *manipulator = new MoveResizer(window, _desktop, x, y, border);
  activate(manipulator);
  return manipulator->_this();
}

Command_ptr DesktopKitImpl::relayer(Warsaw::Window_ptr window)
{
  Manipulator *manipulator = new Relayerer(window);
  activate(manipulator);
  return manipulator->_this();
}

Command_ptr DesktopKitImpl::map(Warsaw::Window_ptr window, CORBA::Boolean flag)
{
  Manipulator *manipulator;
  if (flag) manipulator = new Mapper(window);
  else manipulator = new Unmapper(window);
  activate(manipulator);
  return manipulator->_this();
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "DesktopKitImpl"};
  return new KitFactoryImpl<DesktopKitImpl> ("IDL:Warsaw/DesktopKit:1.0", properties, 1);
}
