/*$Id: ShapeOf.hh,v 1.3 2000/08/31 18:52:32 stefan Exp $
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
#ifndef _ShapeOf_hh
#define _ShapeOf_hh

#include <Berlin/GraphicImpl.hh>

class ShapeOf : public GraphicImpl
{
public:
  ShapeOf(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual ~ShapeOf();

  virtual void request(Warsaw::Graphic::Requisition &);
private:
  Warsaw::Graphic_ptr x;
  Warsaw::Graphic_ptr y;
  Warsaw::Graphic_ptr z;
};

#endif
