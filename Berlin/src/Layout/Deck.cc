/*$Id: Deck.cc,v 1.8 1999/09/13 21:22:07 gray Exp $
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
#include "Layout/Deck.hh"
#include "Layout/LayoutManager.hh"
#include "Warsaw/Traversal.hh"
#include "Warsaw/Transform.hh"

Deck::Deck() : requested(false) {}
Deck::~Deck() {}

void Deck::request(Requisition &r)
{
  if (!requested)
    {
      GraphicImpl::initRequisition(requisition);
      long n = children.size();
      if (n > 0)
	{
	  Graphic::Requisition *r = childrenRequests();
	  LayoutAlign x(xaxis);
	  x.request(n, r, requisition);
	  LayoutAlign y(yaxis);
	  y.request(n, r, requisition);
	  pool.deallocate(r);
	}
      requested = true;
    }
  r = requisition;
}

void Deck::extension(const Allocation::Info &a, Region_ptr r)
{
  if (size_t n = children.size()) children[n - 1].first->extension(a, r);
}

void Deck::traverse(Traversal_ptr t)
{
  if (size_t n = children.size()) t->traverseChild(children[n - 1].first, children[n - 1].second, Region::_nil(), Transform::_nil());
}
