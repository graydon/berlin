/*$Id: Strut.hh,v 1.2 2000/09/19 21:11:05 stefan Exp $
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
#ifndef _Strut_hh
#define _Strut_hh

#include <Berlin/GraphicImpl.hh>

class Strut : public GraphicImpl
{
public:
  Strut(const Warsaw::Graphic::Requisition &r) : requisition(r) {}
  virtual void request(Warsaw::Graphic::Requisition &r) { r = requisition;}
private:
  Warsaw::Graphic::Requisition requisition;
};

#endif
