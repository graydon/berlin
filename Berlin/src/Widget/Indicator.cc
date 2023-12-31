/*$Id: Indicator.cc,v 1.1 1999/08/26 14:06:40 gray Exp $
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

#include "Widget/Indicator.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/Subject.hh"
#include "Berlin/Color.hh"


Indicator::Indicator(const Color &c) : color(c) {}
Indicator::~Indicator() {}

void Indicator::attach(Telltale_ptr subject)
{
  if (!CORBA::is_nil(telltale)) telltale->detach(View_var(_this()));
  telltale = Telltale::_duplicate(subject);
  telltale->attach(_this());
}

void Indicator::update(Subject_ptr, const CORBA::Any &)
{
  needRedraw();
}

void Indicator::traverse(Traversal_ptr traversal)
{
  traversal->visit(Graphic_var(_this()));
  MonoGraphic::traverse(traversal);
}

void Indicator::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->allocation();
  Vertex u, l;
  allocation->bounds(l, u);
  Color background = color;
  if (!CORBA::is_nil(telltale) && telltale->test(Telltale::active))
    color = brightness(color, 0.5);
  cout << "Indicator::draw" << endl;
}
