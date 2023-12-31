/*$Id: Figure.cc,v 1.3 1999/04/26 21:57:41 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include "Figure/Figure.hh"
#include "Warsaw/DrawingKit.hh"
#include <iostream>

Figure::Figure(const  Style::Spec &sty) {
  myStyle = sty;
}

void Figure::request(Requisition &r)
{
    r.x.defined = false;
    r.y.defined = false;
}

Figure::~Figure() {}

Pencil_ptr Figure::getStyledPencil(DrawingKit_ptr dk) {
  // for the time being we have no advanced style inference or
  // inheritence algorithm, so it just passes in the style it was
  // constructed with.
  return dk->getPencil(myStyle);
}

