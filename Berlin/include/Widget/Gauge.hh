/*$Id: Gauge.hh,v 1.1 1999/08/26 14:06:40 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _Gauge_hh
#define _Gauge_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Warsaw/BoundedValue.hh>
#include <Berlin/GraphicImpl.hh>

class Gauge : implements(View), public GraphicImpl
{
 public:
  Gauge(BoundedValue_ptr v, const Color &c) : value(BoundedValue::_duplicate(v)), color(c), width(200), height(20) {}
  ~Gauge() {}
  virtual void request(Requisition &);
  virtual void draw(DrawTraversal_ptr);
  virtual void update(Subject_ptr, const CORBA::Any &);
private:
  BoundedValue_var value;
  Color color;
  Coord width, height;
};

#endif /* _Gauge_hh */
