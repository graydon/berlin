/*$Id: Requestor.cc,v 1.7 2000/09/19 21:11:06 stefan Exp $
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
#include "Berlin/Requestor.hh"

using namespace Warsaw;

Requestor::Requestor(Alignment xalign, Alignment yalign, Coord xspan, Coord yspan)
{
  GraphicImpl::default_requisition(_requisition);
  Warsaw::Graphic::Requirement *rx = GraphicImpl::requirement(_requisition, xaxis);
  Warsaw::Graphic::Requirement *ry = GraphicImpl::requirement(_requisition, yaxis);
  rx->align = xalign;
  ry->align = yalign;
  rx->natural = rx->maximum = rx->minimum = xspan;
  ry->natural = ry->maximum = ry->minimum = yspan; 
}

Requestor::Requestor(const Warsaw::Graphic::Requisition &r) : _requisition(r) {}
Requestor::~Requestor() {}
void Requestor::request(Warsaw::Graphic::Requisition &r) { r = _requisition;}
