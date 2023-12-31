/*$Id: ShapeOf.cc,v 1.1 1999/02/19 14:06:22 gray Exp $
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
#include "Layout/ShapeOf.hh"

ShapeOf::ShapeOf(Graphic_ptr x, Graphic_ptr y, Graphic_ptr z)
{
  x = Graphic::_duplicate(x);
  y = Graphic::_duplicate(y);
  z = Graphic::_duplicate(z);
}

ShapeOf::~ShapeOf()
{
  CORBA::release(x);
  CORBA::release(y);
  CORBA::release(z);
}

void ShapeOf::request(Requisition &r)
{
  if (CORBA::is_nil(y) && CORBA::is_nil(z)) x->request(r);
  else
    {
      Graphic::Requisition req;
      GraphicImpl::initRequisition(req);
      if (!CORBA::is_nil(x))
	{
	  x->request(req);
	  r.x = req.x;
	}
      if (!CORBA::is_nil(y))
	{
	  y->request(req);
	  r.y = req.y;
	}
      if (CORBA::is_nil(z))
	{
	  z->request(req);
	  r.z = req.z;
	}
    }
}
