/*$Id: Filler.cc,v 1.4 1999/07/16 18:54:03 gray Exp $
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

#include <Widget/Filler.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Pencil.hh>
#include <Warsaw/DrawTraversal.hh>

void Filler::request(Requisition &requisition)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  child->request(requisition);
//   GraphicImpl::require(requisition.x, 0, infinity, 0, 0);
//   GraphicImpl::require(requisition.y, 0, infinity, 0, 0);
}

void Filler::traverse(Traversal_ptr traversal)
{
  traversal->visit(Graphic_var(_this()));
  Graphic_var child = body();
  if (!CORBA::is_nil(child))
    MonoGraphic::traverse(traversal);
}

void Filler::draw(DrawTraversal_ptr traversal)
{
  DrawingKit_var dk = traversal->kit();
  Style::Spec style;
  style.length(1);
  style[0].a = Style::fillcolor;
  style[0].val <<= color;
  Pencil_var pen = dk->getPencil(style);
  Vertex lower, upper, origin;
  traversal->bounds(lower, upper, origin);
  Path path;
  path.p.length(5);
  path.p[0].x = lower.x, path.p[0].y = lower.y, path.p[0].z = 0.;
  path.p[1].x = upper.x, path.p[1].y = lower.y, path.p[1].z = 0.;
  path.p[2].x = upper.x, path.p[2].y = upper.y, path.p[2].z = 0.;
  path.p[3].x = lower.x, path.p[3].y = upper.y, path.p[3].z = 0.;
  path.p[4].x = lower.x, path.p[4].y = lower.y, path.p[4].z = 0.;
  Transform_var transform = traversal->transformation();
  for (unsigned int i = 0; i != 5; i++) transform->transformVertex(path.p[i]);
  pen->drawPath(path);
  MonoGraphic::traverse(traversal);
}
