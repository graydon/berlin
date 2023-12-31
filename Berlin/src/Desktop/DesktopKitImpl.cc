/*$Id: DesktopKitImpl.cc,v 1.8 1999/11/06 20:23:08 stefan Exp $
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

#include "Desktop/DesktopKitImpl.hh"
#include "Desktop/WindowImpl.hh"
#include "Berlin/Logger.hh"
#include "Berlin/Plugin.hh"

DesktopKitImpl::DesktopKitImpl() {}
DesktopKitImpl::~DesktopKitImpl() {}

void DesktopKitImpl::bind(ServerContext_ptr sc)
{
  CloneableImpl::bind(sc);
  CORBA::Object_var object = context->getSingleton(interface(Desktop));
  desktop = Desktop::_narrow(object);
  
  lk = obtain(context, LayoutKit);
  wk = obtain(context, WidgetKit);
}

Desktop_ptr DesktopKitImpl::desk()
{
  return Desktop::_duplicate(desktop);
}


Window_ptr DesktopKitImpl::shell(Graphic_ptr g)
{
  SectionLog section("DesktopKitImpl::shell");
  WindowImpl *window = new WindowImpl;
  window->_obj_is_ready(_boa());
  Color gray = {0.5, 0.5, 0.5, 1.0};

  Graphic::Requisition req;
  req.x.defined = true;
  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = lk->fil();
  req.x.align = 0.;
  req.y.defined = true;
  req.y.minimum = 20.;
  req.y.natural = 20.;
  req.y.maximum = 20.;
  req.y.align = 0;
  Command_var move = window->move();
  Graphic_var tbframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var tbdragger = wk->dragger(tbframe, move);

  req.x.minimum = 20.;
  req.x.natural = 20.;
  req.x.maximum = 20.;
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var lresize = window->moveResize(1.0, 0.0, Window::left|Window::bottom);
  Graphic_var lframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var ldragger = wk->dragger(lframe, lresize);

  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = lk->fil();
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var bresize = window->moveResize(0.0, 0.0, Window::bottom);
  Graphic_var bframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var bdragger = wk->dragger(bframe, bresize);

  req.x.minimum = 20.;
  req.x.natural = 20.;
  req.x.maximum = 20.;
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var rresize = window->moveResize(0.0, 0.0, Window::right|Window::bottom);
  Graphic_var rframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var rdragger = wk->dragger(rframe, rresize);

  Graphic_var vbox = lk->vbox();
  Graphic_var hbox = lk->hbox();
  hbox->append(ldragger);
  hbox->append(bdragger);
  hbox->append(rdragger);
  vbox->append(tbdragger);
  vbox->append(g);
  vbox->append(hbox);
  window->body(vbox);
  window->insert(desktop, true);
  windows.push_back(window);
  desktop->appendController(Controller_var(window->_this()));
  return window->_this();
}

Window_ptr DesktopKitImpl::transient(Graphic_ptr g)
{
  SectionLog section("DesktopKitImpl::transient");
  WindowImpl *window = new WindowImpl;
  window->_obj_is_ready(_boa());
  Color gray = {0.5, 0.5, 0.5, 1.0};

  Graphic::Requisition req;
  req.x.defined = true;
  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = lk->fil();
  req.x.align = 0.;
  req.y.defined = true;
  req.y.minimum = 20.;
  req.y.natural = 20.;
  req.y.maximum = 20.;
  req.y.align = 0;
  Command_var move = window->move();
  Graphic_var tbframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var tbdragger = wk->dragger(tbframe, move);

  req.x.minimum = 20.;
  req.x.natural = 20.;
  req.x.maximum = 20.;
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var lresize = window->moveResize(1.0, 0.0, Window::left|Window::bottom);
  Graphic_var lframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var ldragger = wk->dragger(lframe, lresize);

  req.x.minimum = 0.;
  req.x.natural = 0.;
  req.x.maximum = lk->fil();
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var bresize = window->moveResize(0.0, 0.0, Window::bottom);
  Graphic_var bframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var bdragger = wk->dragger(bframe, bresize);

  req.x.minimum = 20.;
  req.x.natural = 20.;
  req.x.maximum = 20.;
  req.y.minimum = 4.;
  req.y.natural = 4.;
  req.y.maximum = 4.;
  Command_var rresize = window->moveResize(0.0, 0.0, Window::right|Window::bottom);
  Graphic_var rframe = wk->outset(Graphic_var(lk->glueRequisition(req)), gray, true);
  Graphic_var rdragger = wk->dragger(rframe, rresize);

  Graphic_var vbox = lk->vbox();
  Graphic_var hbox = lk->hbox();
  hbox->append(ldragger);
  hbox->append(bdragger);
  hbox->append(rdragger);
  vbox->append(tbdragger);
  vbox->append(g);
  vbox->append(hbox);
  window->body(vbox);
  window->insert(desktop, false);
  windows.push_back(window);
  desktop->appendController(Controller_var(window->_this()));
  return window->_this();
}

EXPORT_PLUGIN(DesktopKitImpl,interface(DesktopKit))
