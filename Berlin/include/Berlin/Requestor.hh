/*$Id: Requestor.hh,v 1.6 2000/09/19 21:11:03 stefan Exp $
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
#ifndef _Requestor_hh
#define _Requestor_hh

#include <Berlin/MonoGraphic.hh>

class Requestor : public MonoGraphic
{
public:
  Requestor(Warsaw::Alignment xalign = .5, Warsaw::Alignment yalign = .5, Warsaw::Coord xspan = 1, Warsaw::Coord yspan = 1);
  Requestor(const Warsaw::Graphic::Requisition &);
  ~Requestor();
 
  virtual void request(Warsaw::Graphic::Requisition &);
protected:
  Warsaw::Graphic::Requisition _requisition;
};

#endif 
