/*$Id: ViewportImpl.hh,v 1.6 1999/06/15 20:54:19 gray Exp $
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
#ifndef _Viewport_hh
#define _Viewport_hh

#include "Warsaw/config.hh"
#include "Warsaw/Viewport.hh"
#include "Berlin/MonoGraphic.hh"

declare_corba_ptr_type(BoundedRange)
class RegionImpl;

class ViewportImpl : implements(Viewport), public MonoGraphic
{
  class Adjustment;
 public:
  ViewportImpl();
  ~ViewportImpl();
  void attachAdjustments();
  virtual void body(Graphic_ptr);

  virtual Transform_ptr transformation();
  virtual void request(Requisition &);

  virtual void traverse(Traversal_ptr);

  virtual void needResize();

  virtual BoundedRange_ptr adjustment(Axis);

  virtual void update(Subject_ptr, const CORBA::Any &);

  void scrollTo(Axis, Coord);
  Coord lower(Axis);
  Coord length(Axis);
  Coord offset(Axis);
  Coord visible(Axis);
protected:
  void allocateChild(Allocation::Info &);
  void cacheRequisition();
  void checkAllocation(Region_ptr);
  RegionImpl *bodyAllocation(Region_ptr);
  void scrollTransform(Transform_ptr);

  Coord       of[2];
  Coord       vi[2];
  Coord       lo[2]; 
  Coord       le[2];

  Adjustment *xadjustment;
  Adjustment *yadjustment;
  bool        requested;
  Requisition requisition;
};

#endif /* _Viewport_hh */
