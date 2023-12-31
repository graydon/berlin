/*$Id: DesktopImpl.cc,v 1.3 1999/11/06 20:23:08 stefan Exp $
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

#include "Berlin/DesktopImpl.hh"
#include "Berlin/Vertex.hh"
#include "Berlin/Logger.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/Transform.hh"
#include "Warsaw/Pencil.hh"
#include "Warsaw/Region.hh"

DesktopImpl::DesktopImpl() : ControllerImpl(false) {}
DesktopImpl::~DesktopImpl() {}
// void DesktopImpl::draw(DrawTraversal_ptr traversal)
// {  
//   Region_var allocation = traversal->allocation();
//   Vertex upper, lower;
//   allocation->bounds(lower, upper);
//   Transform_var transformation = traversal->transformation();
//   transformation->transformVertex(upper);
//   transformation->transformVertex(lower);
//   Style::Spec style;
//   style.length(1);
//   style[0].a = Style::fillcolor;
//   Color background;
//   background.red = 1.0;
//   background.green = 0.8;
//   background.blue = 1.0;
//   background.alpha = 1.0;
//   style[0].val <<= background;
//   Path path;
//   path.p.length(4);
//   path.p[0].x = lower.x, path.p[0].y = lower.y, path.p[0].z = 0.;
//   path.p[1].x = upper.x, path.p[1].y = lower.y, path.p[1].z = 0.;
//   path.p[2].x = upper.x, path.p[2].y = upper.y, path.p[2].z = 0.;
//   path.p[3].x = lower.x, path.p[3].y = upper.y, path.p[3].z = 0.;
//   DrawingKit_var dk = traversal->kit();
//   Pencil_var pen = dk->getPencil(style);
//   pen->drawPath(path);
// }

void DesktopImpl::init(Stage_ptr s)
{
  stage = Stage::_duplicate(s);
  ControllerImpl::body(stage);
}
