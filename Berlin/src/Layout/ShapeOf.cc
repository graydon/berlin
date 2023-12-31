/*$Id: ShapeOf.cc,v 1.4 2000/09/19 21:11:08 stefan Exp $
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
#include "Layout/ShapeOf.hh"

using namespace Warsaw;

ShapeOf::ShapeOf(Graphic_ptr x, Graphic_ptr y, Graphic_ptr z)
{
  x = Warsaw::Graphic::_duplicate(x);
  y = Warsaw::Graphic::_duplicate(y);
  z = Warsaw::Graphic::_duplicate(z);
}

ShapeOf::~ShapeOf()
{
  CORBA::release(x);
  CORBA::release(y);
  CORBA::release(z);
}

void ShapeOf::request(Warsaw::Graphic::Requisition &r)
{
  if (CORBA::is_nil(y) && CORBA::is_nil(z)) x->request(r);
  else
    {
      Warsaw::Graphic::Requisition req;
      GraphicImpl::init_requisition(req);
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
