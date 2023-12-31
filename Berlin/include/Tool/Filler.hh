/*$Id: Filler.hh,v 1.2 2000/04/18 18:41:04 stefan Exp $
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
#ifndef _Filler_hh
#define _Filler_hh

#include <Berlin/MonoGraphic.hh>

class Filler : public MonoGraphic
{
public:
  Filler(const Color &c) : color(c) {}
  ~Filler() {}

  virtual void request(Requisition &);
  virtual void traverse(Traversal_ptr);
  virtual void draw(DrawTraversal_ptr);
protected:
  Color color;
};

#endif /* _Filler_hh */
