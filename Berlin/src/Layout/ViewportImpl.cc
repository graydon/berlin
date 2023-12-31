/*$Id: ViewportImpl.cc,v 1.8 1999/09/30 17:23:34 gray Exp $
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

#include "Layout/ViewportImpl.hh"
#include "Warsaw/BoundedRange.hh"
#include "Warsaw/Transform.hh"
#include "Berlin/SubjectImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Math.hh"

using namespace Prague;

static const double epsilon = 10e-6;

class ViewportImpl::Adjustment : implements(BoundedRange), virtual public SubjectImpl
{
 public:
  Adjustment() : l(0.), u(0.), lv(0.), uv(0.), s(0.), p(0.) {}
  virtual ~Adjustment() {}
  virtual Coord lower();
  virtual void lower(Coord);
  virtual Coord upper();
  virtual void upper(Coord);
  virtual Coord step();
  virtual void step(Coord);
  virtual Coord page();
  virtual void page(Coord);
  virtual Coord lvalue();
  virtual void lvalue(Coord);
  virtual Coord uvalue();
  virtual void uvalue(Coord);
  virtual void forward();
  virtual void backward();
  virtual void fastforward();
  virtual void fastbackward();
  virtual void begin();
  virtual void end();
  virtual void adjust(Coord);
  void scrollTo(Coord);
 protected:
  Coord l, u, lv, uv;
  Coord s, p;
  Mutex myMutex;
};                                

Coord ViewportImpl::Adjustment::lower()
{
  MutexGuard guard(myMutex);
  return l;
}

void ViewportImpl::Adjustment::lower(Coord ll)
{
  MutexGuard guard(myMutex);
  if (ll == l) return;
  l = ll;
  if (lv < l) lv = l;
  if (uv < l) uv = l;
  CORBA::Any any;
  notify(any);
}

Coord ViewportImpl::Adjustment::upper()
{
  MutexGuard guard(myMutex);
  return u;
}

void ViewportImpl::Adjustment::upper(Coord uu)
{
  MutexGuard guard(myMutex);
  if (uu == u) return;
  u = uu;
  if (lv > u) lv = u;
  if (uv > u) uv = u;
  CORBA::Any any;
  notify(any);
}

Coord ViewportImpl::Adjustment::step()
{
  MutexGuard guard(myMutex);
  return s;
}

void ViewportImpl::Adjustment::step(Coord ss)
{
  MutexGuard guard(myMutex);
  s = ss;
}

Coord ViewportImpl::Adjustment::page()
{
  MutexGuard guard(myMutex);
  return p;
}

void ViewportImpl::Adjustment::page(Coord pp)
{
  MutexGuard guard(myMutex);
  p = pp;
}

void ViewportImpl::Adjustment::forward()
{
  MutexGuard guard(myMutex);
  Coord t = uv + s > u ? u - uv : s;
  if (t <= 0.) return;
  lv += t;
  uv += t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::backward()
{
  MutexGuard guard(myMutex);
  Coord t = lv - s < l ? lv - l : s;
  if (t <= 0.) return;
  lv -= t;
  uv -= t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::fastforward()
{
  MutexGuard guard(myMutex);
  Coord t = uv + p > u ? u - uv : p;
  if (t <= 0.) return;
  lv += t;
  uv += t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::fastbackward()
{
  MutexGuard guard(myMutex);
  Coord t = lv - p < l ? lv - l : p;
  if (t <= 0.) return;
  lv -= t;
  uv -= t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::begin()
{
  MutexGuard guard(myMutex);
  Coord t = lv - l;
  if (t == 0.) return;
  lv -= t;
  uv -= t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::end()
{
  MutexGuard guard(myMutex);
  Coord t = u - uv;
  if (t == 0.) return;
  lv += t;
  uv += t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::lvalue(Coord vv)
{
  MutexGuard guard(myMutex);
  if (vv > u) vv = u;
  else if (vv < l) vv = l;
  if (vv == lv) return;
  lv = vv;
  CORBA::Any any;
  notify(any);
}

Coord ViewportImpl::Adjustment::lvalue()
{
  MutexGuard guard(myMutex);
  return lv;
}

void ViewportImpl::Adjustment::uvalue(Coord vv)
{
  MutexGuard guard(myMutex);
  if (vv > u) vv = u;
  else if (vv < l) vv = l;
  if (vv == uv) return;
  uv = vv;
  CORBA::Any any;
  notify(any);
}

Coord ViewportImpl::Adjustment::uvalue()
{
  MutexGuard guard(myMutex);
  return uv;
}

void ViewportImpl::Adjustment::adjust(Coord d)
{
  MutexGuard guard(myMutex);
  Coord t =
    uv + d > u ? u - uv :
    lv + d < l ? lv - l : d;
  if (t == 0.) return;
  lv += t;
  uv += t;
  CORBA::Any any;
  notify(any);
}

void ViewportImpl::Adjustment::scrollTo(Coord vv)
{
  Coord delta = vv - lvalue();
  adjust(delta);
}

ViewportImpl::ViewportImpl()
{
  xadjustment = new Adjustment;
  xadjustment->_obj_is_ready(CORBA::BOA::getBOA());
  yadjustment = new Adjustment;
  yadjustment->_obj_is_ready(CORBA::BOA::getBOA());
}

ViewportImpl::~ViewportImpl()
{
  xadjustment->_dispose();
  yadjustment->_dispose();
}

void ViewportImpl::attachAdjustments()
{
  xadjustment->attach(_this());
  yadjustment->attach(_this());
}

void ViewportImpl::body(Graphic_ptr g)
{
  xadjustment->block(true);
  yadjustment->block(true);
  GraphicImpl::defaultRequisition(requisition);
  lo[xaxis] = lo[yaxis] = Coord(0);

  if (!CORBA::is_nil(g))
    {
      requested = true;
      MonoGraphic::request(requisition);

      Requirement &rx = requisition.x;
      Requirement &ry = requisition.y;

      vi[xaxis] = le[xaxis] = rx.defined ? rx.natural : Coord(0);
      vi[yaxis] = le[yaxis] = ry.defined ? ry.natural : Coord(0);

      lo[xaxis] = rx.defined ? - le[xaxis] * rx.align : Coord(0);
      lo[yaxis] = ry.defined ? - le[yaxis] * ry.align : Coord(0);
      xadjustment->lower(lo[xaxis]);
      yadjustment->lower(lo[yaxis]);
      xadjustment->upper(lo[xaxis] + le[xaxis]);
      yadjustment->upper(lo[yaxis] + le[yaxis]);
      if (rx.defined) xadjustment->scrollTo(-vi[xaxis] * rx.align);
      if (ry.defined) yadjustment->scrollTo(-vi[yaxis] * ry.align);
    }
  else
    {
      requested = false;
      vi[xaxis] = vi[yaxis] = Coord(0);
      lo[xaxis] = lo[yaxis] = Coord(0);
      le[xaxis] = le[yaxis] = Coord(0);
    }
  xadjustment->block(false);
  yadjustment->block(false);
}

Transform_ptr ViewportImpl::transformation() { return Transform::_nil();}

void ViewportImpl::request(Requisition &r)
{
  cacheRequisition();
  GraphicImpl::require(r.x, requisition.x.natural, GraphicImpl::infinity, requisition.x.natural, requisition.x.align);
  GraphicImpl::require(r.y, requisition.y.natural, GraphicImpl::infinity, requisition.y.natural, requisition.y.align);
  if(requisition.z.defined)
    GraphicImpl::require(r.z, requisition.z.natural, GraphicImpl::infinity, requisition.z.natural, requisition.z.align);
}

void ViewportImpl::traverse(Traversal_ptr t)
{
//   if (! (t->is_pick_op() && (! t->allocation_is_visible())))
//     {
//       Painter_var p = t->current_painter();
//       Region_var a = t->allocation();

//       t->begin_viewer(Viewport_ptr(this));

//       check_allocation(a);
//       Flags flags = push_environment(p, a);

//       if (is_not_nil(offset_->child_))
// 	{
// 	  t->traverse_child(offset_, Region_var(bodyAllocation(a)));
// 	}

//       pop_environment(p, flags);
//       if (t->is_pick_op() && is_nil(GlyphTraversal_var(t->picked())))
// 	{
// 	  t->hit();
// 	}
//       t->end_viewer();
//     }
}

void ViewportImpl::needResize()
{
  requested = false;
  MonoGraphic::needResize();
}

void ViewportImpl::update(Subject_ptr subject, const CORBA::Any &)
{
}

void ViewportImpl::allocateChild(Allocation::Info &info)
{
//   if (!CORBA::is_nil(info.clipping))
//     {
//       RegionImpl *region = new RegionImpl(info.allocation, info.transformation);  
//       region->_obj_is_ready(_boa());
//       info.clipping->mergeIntersect(region->_this());
//       region->_dispose();
//     }

  scrollTransform(info.transformation);
  RegionImpl *region = bodyAllocation(info.allocation);
  info.allocation->copy(region->_this());
  region->_dispose();
}

BoundedRange_ptr ViewportImpl::adjustment(Axis a)
{
  return a == xaxis ? xadjustment->_this() : yadjustment->_this();
}

void ViewportImpl::scrollTo(Axis a, Coord lower)
{
  of[a] = lower;
  needRedraw();
}

Coord ViewportImpl::lower(Axis a) { return lo[a];}
Coord ViewportImpl::length(Axis a) { return le[a];}
Coord ViewportImpl::offset(Axis a) { return of[a];}
Coord ViewportImpl::visible(Axis a) { return vi[a];}

void ViewportImpl::cacheRequisition()
{
  if (! requested)
    {
      requested = true;
      MonoGraphic::request(requisition);

      Requirement &rx = requisition.x;
      Requirement &ry = requisition.y;

      lo[xaxis] = rx.defined ? -(le[xaxis] * rx.align) : Coord(0);
      lo[yaxis] = ry.defined ? -(le[yaxis] * ry.align) : Coord(0);

      if (rx.defined) xadjustment->scrollTo(-vi[xaxis] * rx.align);
      if (ry.defined) yadjustment->scrollTo(-vi[yaxis] * ry.align);
    }
}

void ViewportImpl::checkAllocation(Region_ptr a)
{
  if (!CORBA::is_nil(a))
    {
      cacheRequisition();
      Region::Allotment xa, ya;
      Coord xlength, ylength;
      Requirement &rx = requisition.x;
      Requirement &ry = requisition.y;
      a->span(xaxis, xa);
      a->span(yaxis, ya);

      xlength = rx.natural;
// 	} else {
// 	    xlength = Math::min(  // Constrain span b/w min,max. 
// 	        Math::max(xspan.length, rx.minimum), rx.maximum
// 	    );
// 	}
      ylength = ry.natural;
// 	} else {
// 	    ylength = Math::min(
// 	        Math::max(yspan.length, ry.minimum), ry.maximum
// 	    );
// 	}
      if (! Math::equal(xlength, le[xaxis], epsilon))
	{
	  Coord margin = xlength - le[xaxis];
	  lo[xaxis] -= (margin * rx.align);
	  le[xaxis] = xlength;
	}
      if (! Math::equal(ylength, le[yaxis], epsilon))
	{
	  Coord margin = ylength - le[yaxis];
	  lo[yaxis] -= (margin * ry.align);
	  le[yaxis] = ylength;
	}
	
      if (! Math::equal(xa.end - xa.begin, vi[xaxis], epsilon))
	{
	  Coord margin = xa.end - xa.begin - vi[xaxis];
	  vi[xaxis] = xa.end - xa.begin;
	  xadjustment->scrollTo(of[xaxis] - margin * rx.align);
        }
      if (! Math::equal(ya.end - ya.begin, vi[yaxis], epsilon))
	{
	  Coord margin = ya.end - ya.begin - vi[yaxis];
	  vi[yaxis] = ya.end - ya.begin;
	  yadjustment->scrollTo(of[yaxis] - margin * ry.align);
        }
      CORBA::Any any;
      xadjustment->notify(any);
      yadjustment->notify(any);
    }
}

RegionImpl *ViewportImpl::bodyAllocation(Region_ptr a)
{
  if (!CORBA::is_nil(a))
    {
      RegionImpl *ca = new RegionImpl(a, 0);
      ca->_obj_is_ready(_boa());
      Region::Allotment xa, ya;
      a->span(xaxis, xa);
      a->span(yaxis, ya);
      Coord margin;
      margin = (le[xaxis] - (xa.end - xa.begin));
      ca->lower.x -= (margin * xa.align);
      ca->upper.x = ca->lower.x + le[xaxis];
	
      margin = (le[yaxis] - (ya.end - ya.begin));
      ca->lower.y -= (margin * ya.align);
      ca->upper.y = ca->lower.y + le[yaxis];
      return ca;
    }
  return 0;
}

void ViewportImpl::scrollTransform(Transform_ptr tx)
{
  Vertex v;
  Requirement &rx = requisition.x;
  Requirement &ry = requisition.y;
  v.x = (le[xaxis] - vi[xaxis]) * rx.align - of[xaxis] - lo[xaxis];
  v.y = (le[yaxis] - vi[yaxis]) * ry.align - of[yaxis] - lo[yaxis];
  v.z = Coord(0);
  tx->translate(v);
}	
