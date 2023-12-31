/*$Id: DesktopImpl.cc,v 1.10 2001/01/09 21:35:10 tobias Exp $
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

#include <Babylon/Babylon.hh>
#include <Warsaw/config.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/Region.hh>
#include "Berlin/DesktopImpl.hh"
#include "Berlin/Vertex.hh"
#include "Berlin/Logger.hh"
#include "Berlin/ServerImpl.hh"

using namespace Prague;
using namespace Warsaw;
using namespace Layout;

DesktopImpl::DesktopImpl(Stage_ptr stage)
  : ControllerImpl(false), _stage(RefCount_var<Layout::Stage>::increment(stage))
{
  /*
   * Attention !!: this invokes _this(), which implicitely activates the desktop.
   */
  ControllerImpl::body(_stage);
}
DesktopImpl::~DesktopImpl() {}
void DesktopImpl::body(Warsaw::Graphic_ptr) {}
Warsaw::Graphic_ptr DesktopImpl::body() { return CORBA::is_nil(_stage) ? Layout::Stage::_nil() : Layout::Stage::_duplicate(_stage);}
Warsaw::Region_ptr DesktopImpl::bbox() { return _stage->bbox();}
CORBA::Long DesktopImpl::layers() { return _stage->layers();}
Layout::StageHandle_ptr DesktopImpl::layer(Layout::Stage::Index l) { return _stage->layer(l);}
void DesktopImpl::begin() { _stage->begin();}
void DesktopImpl::end() { _stage->end();}
Layout::StageHandle_ptr DesktopImpl::insert(Warsaw::Graphic_ptr g, const Warsaw::Vertex &p, const Warsaw::Vertex &s, Layout::Stage::Index l)
{
  return _stage->insert(g, p, s, l);
}

/*
 * little hack: stop the server when the <escape> key is hit
 */
void DesktopImpl::key_press(const Input::Event &event)
{
  Trace trace("DesktopImpl::key_press");
  const Input::Toggle &toggle = event[0].attr.selection();
  if (toggle.number == Babylon::UC_ESCAPE) ServerImpl::instance()->stop();
}
