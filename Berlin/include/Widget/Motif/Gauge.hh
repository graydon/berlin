/*$Id: Gauge.hh,v 1.4 2000/09/12 19:23:41 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Motif_Gauge_hh
#define _Motif_Gauge_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Warsaw/BoundedValue.hh>
#include <Berlin/ViewImpl.hh>
#include <Berlin/GraphicImpl.hh>
#include <Berlin/RefCountVar.hh>

namespace Motif
{

class Gauge : public virtual ViewImpl,
	      public GraphicImpl
{
 public:
  Gauge(Warsaw::BoundedValue_ptr v, const Warsaw::Color &c)
    : value(RefCount_var<Warsaw::BoundedValue>::increment(v)), color(c), width(2000.), height(200.) {}
  ~Gauge() {}
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void update(const CORBA::Any &);
private:
  RefCount_var<Warsaw::BoundedValue> value;
  Warsaw::Color color;
  Warsaw::Coord width, height;
};

};

#endif
