/*$Id: Gauge.cc,v 1.4 2000/10/20 17:45:01 stefan Exp $
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

#include <Warsaw/config.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Transform.hh>
#include <Berlin/Color.hh>
#include "Widget/Motif/Gauge.hh"

using namespace Warsaw;
using namespace Motif;

void Gauge::request(Warsaw::Graphic::Requisition &requisition)
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
  DrawingKit_var drawing = traversal->drawing();
  drawing->save();
  Coord l = value->lower();
  Coord u = value->upper();
  Coord v = value->value();
  Path path;
  path.length(5);
  path[0].x = path[0].y = path[0].z = 0;
  path[2].x = width, path[2].y = height, path[2].z = 0;
  path[1].x = path[2].x, path[1].y = path[0].y, path[1].z = 0.;
  path[3].x = path[0].x, path[3].y = path[2].y, path[3].z = 0.;
  path[4] = path[0];
//   Transform_var transform = traversal->transformation();
//   for (unsigned int i = 0; i != path.p.length(); i++) transform->transformVertex(path.p[i]);
  drawing->surface_fillstyle(DrawingKit::solid);
  drawing->foreground(brightness(color, -0.2));
  drawing->draw_path(path);
  path[0].x = path[0].y = path[0].z = 0;
  path[2].x = v/(u-l)*width, path[2].y = height, path[2].z = 0;
  path[1].x = path[2].x, path[1].y = path[0].y, path[1].z = 0.;
  path[3].x = path[0].x, path[3].y = path[2].y, path[3].z = 0.;
  path[4] = path[0];
//   for (unsigned int i = 0; i != path.p.length(); i++) transform->transformVertex(path.p[i]);
  drawing->foreground(brightness(color, -0.7));
  drawing->draw_path(path);  
  drawing->restore();
}

void Gauge::update(const CORBA::Any &)
{
  need_redraw();
}
