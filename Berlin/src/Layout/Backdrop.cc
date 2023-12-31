/*$Id: Backdrop.cc,v 1.3 1999/05/25 18:28:59 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on code from Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
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
#include "Berlin/RegionImpl.hh"
#include "Layout/Backdrop.hh"

static const Coord epsilon = 0.01;

Backdrop::Backdrop() { }
Backdrop::~Backdrop() { }

void Backdrop::request(Requisition &r)
{
  GraphicImpl::require(r.x, 0., GraphicImpl::infinity, 0., 0.);
  GraphicImpl::require(r.y, 0., GraphicImpl::infinity, 0., 0.);
  GraphicImpl::require(r.z, 0., GraphicImpl::infinity, 0., 0.);
}

void Backdrop::draw(DrawTraversal_ptr t)
{
  DrawTraversal_var traversal = t;
  // Get the origin of our allocation.
  Vertex origin;
  Region_var allocation = traversal->allocation();
  allocation->origin(origin);

  // Get the visible regions bounds.
  Vertex lower, upper;
  Painter_var painter = traversal->current_painter();
  Region_var visible = painter->visible();
  visible->bounds(lower, upper);

  // Figure out the lower coordinate to begin drawing.
  Graphic::Requisition requisition;
  GraphicImpl::init_requisition(requisition);
  offset->child->request(requisition);
  if (!requisition.x.defined)
    GraphicImpl::require(requisition.x,Coord(1),Coord(0),Coord(0),Alignment(0));
  if (!requisition.y.defined)
    GraphicImpl::require(requisition.y,Coord(1),Coord(0),Coord(0),Alignment(0));
  Coord w = requisition.x.natural;
  Coord h = requisition.y.natural;
  Coord lead_w = w * requisition.x.align;
  Coord lead_h = h * requisition.y.align;
  Long below_x = Long(((origin.x - lead_w - lower.x) + (w - epsilon)) / w);
  Long below_y = Long(((origin.y - lead_h - lower.y) + (h - epsilon)) / h);
  Coord x = origin.x - Coord(below_x) * w;
  Coord y = origin.y - Coord(below_y) * h;
  
  // Figure out how many times to draw.
  CORBA::ULong count_x = CORBA::ULong(((upper.x - x) + (w - epsilon)) / w);
  CORBA::ULong count_y = CORBA::ULong(((upper.y - y) + (h - epsilon)) / h);

  // Do the drawing.
  Coord current_x = x;
  for (CORBA::ULong i_x = 0; i_x < count_x; ++i_x)
    {
      Coord current_y = y;
      for (CORBA::ULong i_y = 0; i_y < count_y; ++i_y)
	{
	  RegionImpl* a = new RegionImpl;
	  a->valid = true;
	  a->lower.x = current_x - lead_w;
	  a->lower.y = current_y - lead_h;
	  a->upper.x = current_x - lead_w + w;
	  a->upper.y = current_y - lead_h + h;
	  a->xalign = requisition.x.align;
	  a->yalign = requisition.y.align;
	  t->traverse_child(offset, a);
	  CORBA::release(a);
	  current_y += h;
	}
      current_x += w;
    }
}

void Backdrop::traverse(Traversal_ptr t)
{
  if (t->op() == GraphicTraversal::draw)
    {
      if (!is_nil(offset->child))
	draw(t);
    }
  else
    MonoGraphic::traverse(t);
}
