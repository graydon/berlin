/*$Id: Gauge.cc,v 1.2 1999/11/10 21:57:36 stefan Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/Transform.hh"
#include "Warsaw/Pencil.hh"
#include "Widget/Gauge.hh"
#include "Berlin/Color.hh"

void Gauge::request(Requisition &requisition)
{
  requisition.x.defined = true;
  requisition.x.natural = requisition.x.maximum = requisition.x.minimum = width;
  requisition.x.align = 0.;
  requisition.y.defined = true;
  requisition.y.natural = requisition.y.maximum = requisition.y.minimum = height;
  requisition.y.align = 0.;
}

void Gauge::draw(DrawTraversal_ptr traversal)
{
  DrawingKit_var dk = traversal->kit();
  Coord l = value->lower();
  Coord u = value->upper();
  Coord v = value->value();
  Path path;
  path.p.length(5);
  path.p[0].x = path.p[0].y = path.p[0].z = 0;
  path.p[2].x = width, path.p[2].y = height, path.p[2].z = 0;
  path.p[1].x = path.p[2].x, path.p[1].y = path.p[0].y, path.p[1].z = 0.;
  path.p[3].x = path.p[0].x, path.p[3].y = path.p[2].y, path.p[3].z = 0.;
  path.p[4] = path.p[0];
//   Transform_var transform = traversal->transformation();
//   for (unsigned int i = 0; i != path.p.length(); i++) transform->transformVertex(path.p[i]);
  Style::Spec style;
  style.length(1);
  style[0].a = Style::fillcolor;
  Color bg = brightness(color, -0.2);
  style[0].val <<= bg;
  Pencil_var pen = dk->getPencil(style);
  pen->drawPath(path);
  path.p[0].x = path.p[0].y = path.p[0].z = 0;
  path.p[2].x = v/(u-l)*width, path.p[2].y = height, path.p[2].z = 0;
  path.p[1].x = path.p[2].x, path.p[1].y = path.p[0].y, path.p[1].z = 0.;
  path.p[3].x = path.p[0].x, path.p[3].y = path.p[2].y, path.p[3].z = 0.;
  path.p[4] = path.p[0];
//   for (unsigned int i = 0; i != path.p.length(); i++) transform->transformVertex(path.p[i]);
  bg = brightness(color, -0.7);
  style[0].val <<= bg;
  pen = dk->getPencil(style);
  pen->drawPath(path);  
}

void Gauge::update(Subject_ptr, const CORBA::Any &)
{
  needRedraw();
}
