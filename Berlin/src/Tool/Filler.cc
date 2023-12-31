/*$Id: Filler.cc,v 1.2 2000/04/18 18:41:04 stefan Exp $
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

#include <Widget/Filler.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/DrawingKit.hh>
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
//   Style::Spec style;
//   style.length(1);
//   style[0].a = Style::fillcolor;
//   style[0].val <<= color;
  Vertex lower, upper, origin;
  traversal->bounds(lower, upper, origin);
  dk->drawRect(lower, upper);
  MonoGraphic::traverse(traversal);
}
