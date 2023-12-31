/*$Id: ShapeOf.hh,v 1.1 1999/02/19 14:03:56 gray Exp $
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
#ifndef _ShapeOf_hh
#define _ShapeOf_hh

#include <Berlin/GraphicImpl.hh>

class ShapeOf : public GraphicImpl
{
public:
  ShapeOf(Graphic_ptr, Graphic_ptr, Graphic_ptr);
  virtual ~ShapeOf();

  virtual void request(Requisition &);
private:
  Graphic_ptr x;
  Graphic_ptr y;
  Graphic_ptr z;
};

#endif /* _ShapeOf_hh */
