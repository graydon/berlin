/*$Id: Glue.cc,v 1.6 2000/09/19 21:11:08 stefan Exp $
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
#include "Layout/Glue.hh"

using namespace Warsaw;

Glue::Glue(Axis a, Coord natural, Coord stretch, Coord shrink, Alignment align)
{
  GraphicImpl::init_requisition(requisition);
  Warsaw::Graphic::Requirement *r = GraphicImpl::requirement(requisition, a);
  if (r != 0) GraphicImpl::require(*r, natural, stretch, shrink, align);
}

Glue::Glue(const Warsaw::Graphic::Requisition &r) { requisition = r;}
Glue::~Glue() {}

void Glue::request(Warsaw::Graphic::Requisition &r) { r = requisition;}

