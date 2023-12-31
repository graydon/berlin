/*$Id: ViewportImpl.cc,v 1.25 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Berlin/SubjectImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/Math.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/PickTraversal.hh>
#include <Berlin/Provider.hh>
#include <Berlin/TransformImpl.hh>

using namespace Prague;
using namespace Warsaw;
using namespace Layout;

static const double epsilon = 10e-6;

class ViewportImpl::Adjustment : public virtual POA_Warsaw::BoundedRange,
		                 public SubjectImpl
{
 public:
  Adjustment();
  virtual ~Adjustment();
  virtual Warsaw::BoundedRange::Settings state();
  virtual void state(const Warsaw::BoundedRange::Settings &);
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
 protected:
  Warsaw::BoundedRange::Settings settings;
  Coord s, p;
  Mutex mutex;
};                                

ViewportImpl::Adjustment::Adjustment()
  : s(10.), p(10.)
{
  settings.lower = settings.upper = settings.lvalue = settings.uvalue = 0.;
}

ViewportImpl::Adjustment::~Adjustment()
{
}

Warsaw::BoundedRange::Settings ViewportImpl::Adjustment::state()
{
  Prague::Guard<Mutex> guard(mutex);
  return settings;
}

void ViewportImpl::Adjustment::state(const Warsaw::BoundedRange::Settings &s)
{
  Prague::Guard<Mutex> guard(mutex);
  settings = s;
}

Coord ViewportImpl::Adjustment::lower()
{
  Prague::Guard<Mutex> guard(mutex);
  return settings.lower;
}

void ViewportImpl::Adjustment::lower(Coord l)
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    if (l == settings.lower) return;
    settings.lower = l;
    settings.lvalue = std::max(settings.lvalue, settings.lower);
    settings.uvalue = std::max(settings.uvalue, settings.lower);
    any <<= settings;
  }
  notify(any);
}

Coord ViewportImpl::Adjustment::upper()
{
  Prague::Guard<Mutex> guard(mutex);
  return settings.upper;
}

void ViewportImpl::Adjustment::upper(Coord u)
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    if (settings.upper == u) return;
    settings.upper = u;
    settings.lvalue = std::min(settings.lvalue, settings.upper);
    settings.uvalue = std::min(settings.uvalue, settings.upper);
    any <<= settings;
  }
  notify(any);
}

Coord ViewportImpl::Adjustment::step()
{
  Prague::Guard<Mutex> guard(mutex);
  return s;
}

void ViewportImpl::Adjustment::step(Coord ss)
{
  Prague::Guard<Mutex> guard(mutex);
  s = ss;
}

Coord ViewportImpl::Adjustment::page()
{
  Prague::Guard<Mutex> guard(mutex);
  return p;
}

void ViewportImpl::Adjustment::page(Coord pp)
{
  Prague::Guard<Mutex> guard(mutex);
  p = pp;
}

void ViewportImpl::Adjustment::forward()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = std::min(s, settings.upper - settings.uvalue);
    if (t <= 0.) return;
    settings.lvalue += t;
    settings.uvalue += t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::backward()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = std::min(s, settings.lvalue - settings.lower);
    if (t <= 0.) return;
    settings.lvalue -= t;
    settings.uvalue -= t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::fastforward()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = std::min(p, settings.upper - settings.uvalue);
    if (t <= 0.) return;
    settings.lvalue += t;
    settings.uvalue += t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::fastbackward()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = std::min(p, settings.lvalue - settings.lower);
    if (t <= 0.) return;
    settings.lvalue -= t;
    settings.uvalue -= t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::begin()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = settings.lvalue - settings.lower;
    if (t == 0.) return;
    settings.lvalue -= t;
    settings.uvalue -= t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::end()
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = settings.upper - settings.uvalue;
    if (t == 0.) return;
    settings.lvalue += t;
    settings.uvalue += t;
    any <<= settings;
  }
  notify(any);
}

void ViewportImpl::Adjustment::lvalue(Coord lv)
{
  CORBA::Any any;
  {
    lv = std::min(std::max(settings.lower, lv), settings.upper);
    Prague::Guard<Mutex> guard(mutex);
    if (lv == settings.lvalue) return;
    settings.lvalue = lv;
    any <<= settings;
  }
  notify(any);
}

Coord ViewportImpl::Adjustment::lvalue()
{
  Prague::Guard<Mutex> guard(mutex);
  return settings.lvalue;
}


void ViewportImpl::Adjustment::uvalue(Coord uv)
{
  CORBA::Any any;
  {
    uv = std::min(std::max(settings.lower, uv), settings.upper);
    Prague::Guard<Mutex> guard(mutex);
    if (settings.uvalue == uv) return;
    settings.uvalue = uv;
    any <<= settings;
  }
  notify(any);
}

Coord ViewportImpl::Adjustment::uvalue()
{
  Prague::Guard<Mutex> guard(mutex);
  return settings.uvalue;
}


void ViewportImpl::Adjustment::adjust(Coord d)
{
  CORBA::Any any;
  {
    Prague::Guard<Mutex> guard(mutex);
    Coord t = std::min(std::max(d, settings.lower - settings.lvalue), settings.upper - settings.uvalue);
    if (t == 0.) return;
    settings.lvalue += t;
    settings.uvalue += t;
    any <<= settings;
  }
  notify(any);
}

ViewportImpl::ViewportImpl() : requested(false) {}
ViewportImpl::~ViewportImpl() {}

void ViewportImpl::body(Graphic_ptr g)
{
  MonoGraphic::body(g);
  need_resize();
  MonoGraphic::need_resize();
}

Transform_ptr ViewportImpl::transformation() { return Transform::_nil();}

void ViewportImpl::request(Warsaw::Graphic::Requisition &r)
{
  cache_requisition();
  GraphicImpl::require(r.x, requisition.x.natural, 0., requisition.x.natural, requisition.x.align);
  GraphicImpl::require(r.y, requisition.y.natural, 0., requisition.y.natural, requisition.y.align);
  if(requisition.z.defined)
    GraphicImpl::require(r.z, requisition.z.natural, 0., requisition.z.natural, requisition.z.align);
}

void ViewportImpl::traverse(Traversal_ptr traversal)
{
  Graphic_var child = body();
  if (!CORBA::is_nil(child) && traversal->intersects_allocation())
    {
      /*
       * first update the cached allocation and the adjustments
       */
      Region_var allocation = traversal->current_allocation();
      cache_allocation(allocation);
      traversal->visit(Graphic_var(_this()));
    }
}

void ViewportImpl::draw(DrawTraversal_ptr traversal)
{
  /*
   * now simply traverse the child with it's desired allocation
   * and a suitable offset
   */
  Region_var allocation = traversal->current_allocation();
  Transform_var transformation = traversal->current_transformation();

  Lease_var<RegionImpl> clipping(Provider<RegionImpl>::provide());
  clipping->copy(allocation);
  if (!CORBA::is_nil(transformation) && !transformation->identity())
    clipping->apply_transform(transformation);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();
  drawing->clipping(Region_var(clipping->_this()));

  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  Lease_var<RegionImpl> b(Provider<RegionImpl>::provide());
  body_allocation(allocation, b);
  region->copy(Region_var(b->_this()));

  Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
  transform->load_identity();

  region->normalize(Transform_var(transform->_this()));
  try { traversal->traverse_child (_child.peer, _child.localId, Region_var(region->_this()), Transform_var(transform->_this()));}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
  drawing->restore();
}

void ViewportImpl::pick(PickTraversal_ptr traversal)
{
  /*
   * now simply traverse the child with it's desired allocation
   * and a suitable offset
   */
  Region_var allocation = traversal->current_allocation();
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  Lease_var<RegionImpl> b(Provider<RegionImpl>::provide());
  body_allocation(allocation, b);
  region->copy(Region_var(b->_this()));

  Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
  transform->load_identity();

  region->normalize(Transform_var(transform->_this()));
  try { traversal->traverse_child (_child.peer, _child.localId, Region_var(region->_this()), Transform_var(transform->_this()));}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}

void ViewportImpl::need_resize()
{
  /*
   * set adjustment's outer range according to the body size
   */
  requested = false;
  cache_requisition();
  need_redraw();
}

void ViewportImpl::update(const CORBA::Any &)
{
  /*
   * we are only interested in changes concerning the outer range (body)
   * or the offset
   */
  Warsaw::BoundedRange::Settings x = xadjustment->state();
  Warsaw::BoundedRange::Settings y = yadjustment->state();
  bool damage = (x.lower != settings[xaxis].lower || y.lower != settings[yaxis].lower ||
		 x.upper != settings[xaxis].upper || y.upper != settings[yaxis].upper ||
		 x.lvalue != settings[xaxis].lvalue || y.lvalue != settings[yaxis].lvalue);
  settings[xaxis].lvalue = x.lvalue;
  settings[xaxis].uvalue = x.uvalue;
  settings[yaxis].lvalue = y.lvalue;
  settings[yaxis].uvalue = y.uvalue;
  if (damage) need_redraw();
}

void ViewportImpl::activate_composite()
{
  Adjustment *adjustment = new Adjustment;
  activate(adjustment);
  xadjustment = RefCount_var<BoundedRange>::increment(adjustment->_this(), false);
  xadjustment->attach(Observer_var(_this()));
  adjustment = new Adjustment;
  activate(adjustment);
  yadjustment = RefCount_var<BoundedRange>::increment(adjustment->_this(), false);
  yadjustment->attach(Observer_var(_this()));
}

void ViewportImpl::allocate_child(Allocation::Info &info)
{
  scroll_transform(info.transformation);
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  Lease_var<RegionImpl> b(Provider<RegionImpl>::provide());
  body_allocation(info.allocation, b);
  region->copy(Region_var(b->_this()));
  info.allocation->copy(Region_var(region->_this()));
}

BoundedRange_ptr ViewportImpl::adjustment(Axis a)
{
  return a == xaxis ? RefCount_var<BoundedRange>::increment(xadjustment) : RefCount_var<BoundedRange>::increment(yadjustment);
}

void ViewportImpl::cache_requisition()
//. retrieves requisition from body and updates adjustments
{
  if (!requested)
    {
      requested = true;
      MonoGraphic::request(requisition);
      Warsaw::Graphic::Requirement &rx = requisition.x;
      Warsaw::Graphic::Requirement &ry = requisition.y;

      settings[xaxis].lvalue = settings[xaxis].lower = rx.defined ? - rx.natural * rx.align : 0.;
      settings[xaxis].uvalue = settings[xaxis].upper = rx.defined ? settings[xaxis].lvalue + rx.natural : 0.;
      if (rx.defined)
	{
	  xadjustment->lower(settings[xaxis].lower);
	  xadjustment->upper(settings[xaxis].upper);
	}
      settings[yaxis].lvalue = settings[yaxis].lower = ry.defined ? - ry.natural * ry.align : 0.;
      settings[yaxis].uvalue = settings[yaxis].upper = ry.defined ? settings[yaxis].lvalue + ry.natural : 0.;
      if (ry.defined)
	{
	  yadjustment->lower(settings[yaxis].lower);
	  yadjustment->upper(settings[yaxis].upper);
	}
    }
}

void ViewportImpl::cache_allocation(Region_ptr allocation)
{
  if (!CORBA::is_nil(allocation))
    {
      Region::Allotment xa, ya;
      allocation->span(xaxis, xa);
      allocation->span(yaxis, ya);

      if (! Math::equal(xa.end - xa.begin, settings[xaxis].uvalue - settings[xaxis].lvalue, epsilon))
 	{
 	  settings[xaxis].uvalue = settings[xaxis].lvalue + xa.end - xa.begin;
 	  xadjustment->uvalue(settings[xaxis].uvalue);
	}
      if (! Math::equal(ya.end - ya.begin, settings[yaxis].uvalue - settings[yaxis].lvalue, epsilon))
 	{
 	  settings[yaxis].uvalue = ya.end - ya.begin;
 	  yadjustment->uvalue(settings[yaxis].uvalue);
	}
    }
}

void ViewportImpl::body_allocation(Region_ptr, RegionImpl *ca)
{
  /*
   * FIXME!! : this implementation ignores completely the body alignment...
   */
  ca->valid = true;
  ca->lower.x = -(settings[xaxis].lvalue - settings[xaxis].lower);
  ca->lower.y = -(settings[yaxis].lvalue - settings[yaxis].lower);
  ca->lower.z = 0.;
  ca->upper.x = -(settings[xaxis].lvalue - settings[xaxis].upper);
  ca->upper.y = -(settings[yaxis].lvalue - settings[yaxis].upper);
  ca->upper.z = 0.;
  ca->xalign = ca->yalign = ca->yalign = 0.;
}

void ViewportImpl::scroll_transform(Transform_ptr tx)
{
  Vertex v;
  v.x = settings[xaxis].lvalue - settings[xaxis].lower;
  v.y = settings[yaxis].lvalue - settings[yaxis].lower;
  v.z = 0.;
  tx->translate(v);
}
