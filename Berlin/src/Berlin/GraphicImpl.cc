/*$Id: GraphicImpl.cc,v 1.27 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
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
#include "Warsaw/config.hh"
#include "Warsaw/Traversal.hh"
#include "Warsaw/Screen.hh"
#include "Berlin/GraphicImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/AllocationImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/Math.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"

using namespace Prague;

static double tol = 0.05;

static bool rotated (Transform_ptr t)
{
  Transform::Matrix m;
  t->storeMatrix(m);
  return (!Math::equal(m[0][1], 0., tol) || !Math::equal(m[1][0], 0., tol));
}

static void setSpan(RegionImpl &r, Axis a, Coord origin, Coord length)
{
  Coord begin = origin;
  Coord end = begin + length;
  r.valid = true;
  switch (a)
    {
    case xaxis:
      r.lower.x = begin;
      r.upper.x = end;
      break;
    case yaxis:
      r.lower.y = begin;
      r.upper.y = end;
      break;
    case zaxis:
      r.lower.z = begin;
      r.upper.z = end;
      break;
    }
}

static double computeSqueeze(const Graphic::Requirement &r, Coord length)
{
  double f;
  Coord nat = r.natural;
  if (length > nat && r.maximum > nat)
    f = (length - nat) / (r.maximum - nat);
  else if (length < nat && r.minimum < nat)
    f = (nat - length) / (nat - r.minimum);
  else
    f = 0;
  return f;
}

static void computeAllocations(Axis a, Graphic::Requisition &total,
			       CORBA::ULong n, Graphic::Requisition *requests, Region &given,
			       RegionImpl* result)
{
  Graphic::Requirement *r;
  Region::Allotment s;
  r = GraphicImpl::requirement(total, a);
  given.span(a, s);
  Coord length = s.end - s.begin;
  bool growing = length > r->natural;
  bool shrinking = length < r->natural;
  double f = computeSqueeze(*r, length);
  Coord p = 0.0;
  for (CORBA::ULong i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], a);
      Coord cspan = r->natural;
      if (growing)
	cspan += Coord(f * (r->maximum - r->natural));
      else if (shrinking)
	cspan -= Coord(f * (r->natural - r->minimum));
      setSpan(result[i], a, p, cspan);
      p += cspan;
    }
}

static void compensate (double a, double &x, double &y)
{
  if (a > 0.0)
    {
      if (Math::equal(x, 0.0, 1.0e-6)) y = a * x;
      else
	{
	  double aspect = y/x;
	  if (aspect > a) y = a * x;
	  else if (aspect < a) x = y / a;
	}
    }
}

static void flexibleTransformRequest(Graphic::Requisition &req, Transform_ptr t)
{
  SectionLog section("flexibleTransformRequest");
  if (t->Identity()) return;
  if (t->Translation())
    {
      Transform::Matrix mat;
      t->storeMatrix(mat);
      Coord tx = mat[2][0];
      Coord ty = mat[2][1];

      req.x.align = -(-req.x.align * req.x.natural + tx) / req.x.natural;
      req.y.align = -(-req.y.align * req.y.natural + ty) / req.y.natural;
      return;
    }
  
  RegionImpl nat, maxi, mini;

  nat.xalign = req.x.align;
  nat.lower.x = -req.x.align * req.x.natural;
  nat.upper.x = nat.lower.x + req.x.natural;
  nat.yalign = req.y.align;
  nat.lower.y = -req.y.align * req.y.natural;
  nat.upper.y = nat.lower.y + req.y.natural;
  nat.lower.z = nat.upper.z = 0.0;
  nat.valid = true;

  maxi.xalign = req.x.align;
  maxi.lower.x = -req.x.align * req.x.maximum;
  maxi.upper.x = maxi.lower.x + req.x.maximum;
  maxi.yalign = req.y.align;
  maxi.lower.y = -req.y.align * req.y.maximum;
  maxi.upper.y = maxi.lower.y + req.y.maximum;
  maxi.lower.z = maxi.upper.z = 0.0;
  maxi.valid = true;

  mini.xalign = req.x.align;
  mini.lower.x = -req.x.align * req.x.minimum;
  mini.upper.x = mini.lower.x + req.x.minimum;
  mini.yalign = req.y.align;
  mini.lower.y = -req.y.align * req.y.minimum;
  mini.upper.y = mini.lower.y + req.y.minimum;
  mini.lower.z = mini.upper.z = 0.0;
  mini.valid = true;

  nat.applyTransform(t);
  maxi.applyTransform(t);
  mini.applyTransform(t);

  req.x.defined = true;
  req.x.natural = nat.upper.x - nat.lower.x;
  req.x.maximum = maxi.upper.x - maxi.lower.x;
  req.x.minimum = mini.upper.x - mini.lower.x;
  if (!Math::equal(req.x.natural, 0.0, 1.0e-6))
    req.x.align = -nat.lower.x / req.x.natural;
  else
    req.x.align = 0.0;

  req.y.defined = true;
  req.y.natural = nat.upper.y - nat.lower.y;
  req.y.maximum = maxi.upper.y - maxi.lower.y;
  req.y.minimum = mini.upper.y - mini.lower.y;
  if (!Math::equal(req.y.natural, 0.0, 1.0e-6))
    req.y.align = -nat.lower.y / req.y.natural;
  else
    req.y.align = 0.0;

  req.z.defined = false;
}

static void fixedTransformRequest(Graphic::Requisition &req, Transform_ptr t)
{
  SectionLog section("fixedTransformRequest");
  if (t->Identity()) return;
  if (t->Translation())
    {
      Transform::Matrix mat;
      t->storeMatrix(mat);
      Coord tx = mat[2][0];
      Coord ty = mat[2][1];

      req.x.align = (req.x.align * req.x.natural - tx) / req.x.natural;
      req.y.align = (req.y.align * req.y.natural - ty) / req.y.natural;
      return;
    }

  RegionImpl nat;

  nat.xalign = req.x.align;
  nat.lower.x = -req.x.align * req.x.natural;
  nat.upper.x = nat.lower.x + req.x.natural;
  nat.yalign = req.y.align;
  nat.lower.y = -req.y.align * req.y.natural;
  nat.upper.y = nat.lower.y + req.y.natural;
  nat.lower.z = nat.upper.z = Coord(0);
  nat.valid = true;
  nat.applyTransform(t);
  Coord xlead = -nat.lower.x;
  Coord xtrail = nat.upper.x;

  Coord ylead = -nat.lower.y;
  Coord ytrail = nat.upper.y;

  GraphicImpl::requireLeadTrail(req.x, xlead, xlead, xlead, xtrail, xtrail, xtrail);
  GraphicImpl::requireLeadTrail(req.y, ylead, ylead, ylead, ytrail, ytrail, ytrail);
}

/*****************************************************/

GraphicImpl::GraphicImpl() {}
GraphicImpl::~GraphicImpl() {}

Graphic_ptr GraphicImpl::body() { return Graphic::_nil();}
void GraphicImpl::body(Graphic_ptr) {}
void GraphicImpl::append(Graphic_ptr) {}
void GraphicImpl::prepend(Graphic_ptr) {}

void GraphicImpl::addParent(Graphic_ptr parent, Tag tag)
{
  MutexGuard guard(parentMutex);
  parents.push_back(edge_t(Graphic::_duplicate(parent), tag));
}

void GraphicImpl::removeParent(Graphic_ptr parent, Tag tag)
{
  MutexGuard guard(parentMutex);
  for (plist_t::iterator i = parents.begin(); i != parents.end(); i++)
    if ((*i).second == tag && (*i).first->_is_equivalent(parent))
      {
	parents.erase(i);
	break;
      }
}

/*
 * these are default implementations of the layout, picking and drawing protocol
 * which are *intended* to be overridden in children of Graphic, if you want
 * them to actually do anything remotely exciting.
 */

Transform_ptr GraphicImpl::transformation() { return Transform::_nil();}
void GraphicImpl::request(Requisition &) {}
void GraphicImpl::extension(const Allocation::Info &a, Region_ptr r) { GraphicImpl::defaultExtension(a, r);}
void GraphicImpl::shape(Region_ptr) {}

void GraphicImpl::traverse(Traversal_ptr traversal) { traversal->visit(Graphic_var(_this()));}
void GraphicImpl::draw(DrawTraversal_ptr) {}
void GraphicImpl::pick(PickTraversal_ptr) {}

void GraphicImpl::allocate(Tag, const Allocation::Info &) {}
void GraphicImpl::allocations(Allocation_ptr allocation)
{
  MutexGuard guard(parentMutex);
  CORBA::Long begin = allocation->size();
  for (plist_t::iterator i = parents.begin(); i != parents.end(); i++)
    {
      (*i).first->allocations(allocation);      
      CORBA::Long end = allocation->size();
      for (CORBA::Long j = begin; j != end; j++)
	{
	  const Allocation::Info_var info = allocation->get(j);
	  (*i).first->allocate((*i).second, info);
	}
      begin = end;
    }
#if 0
  for (CORBA::Long i = 0; i != allocation->size(); i++)
    {
      Allocation::Info_var info = allocation->get(i);
      Region_var r = info->allocation;
      Transform_var t = info->transformation;
      RegionImpl region(r, t);
      Logger::log(Logger::drawing) << "allocation at " << region << endl;
    }
#endif
}

/*
 * this is the method which causes DrawTraversals to get queued up to run over
 * the scene graph.  It does so by retrieving a list of all the current
 * graphic's Allocation::infos (places on the screen where the graphic appears)
 * and then sequentially extending the provided "damaged areas" with the
 * current graphic's own allocated region. As the damaged regions are extended,
 * they queue DrawTraversals in the ScreenManager, which will send them down to
 * redraw this node when an appropriate time comes 
 */
void GraphicImpl::needRedraw()
{
  SectionLog section("GraphicImpl::needRedraw");
  Impl_var<AllocationImpl> allocation(new AllocationImpl);
  allocations(Allocation_var(allocation->_this()));
  Impl_var<RegionImpl> region(new RegionImpl);
  CORBA::Long size = allocation->size();
  for (CORBA::Long i = 0; i < size; i++)
    {
      const Allocation::Info_var info = allocation->get(i);
      region->valid = false;
      extension(info, Region_var(region->_this()));
      if (region->valid) info->root->damage(Region_var(region->_this()));
    }
}

/* this does almost exactly the same as needRedraw(), only it extends the
 * parent-supplied damage region by a user-supplied subregion of the current
 * graphic. It does this by copying and then transforming the user-supplied
 * region to fit within the transformation of each Allocation::info received
 * from the parents 
 */
void GraphicImpl::needRedrawRegion(Region_ptr region)
{
  SectionLog section("GraphicImpl::needRedrawRegion");
  if (region->defined())
    {
      Impl_var<AllocationImpl> allocation(new AllocationImpl);
      allocations(Allocation_var(allocation->_this()));
      Impl_var<RegionImpl> dr(new RegionImpl);
      for (CORBA::Long i = 0; i < allocation->size(); i++)
	{
	  Allocation::Info_var info = allocation->get(i);
	  dr->copy(region);
	  dr->applyTransform(info->transformation);
	  info->root->damage(Region_var(dr->_this()));
	}
    }
}

void GraphicImpl::needResize()
{
  MutexGuard guard(parentMutex);
  for (plist_t::iterator i = parents.begin(); i != parents.end(); i++)
    (*i).first->needResize();
}

void GraphicImpl::initRequisition(Graphic::Requisition &r)
{
  r.x.defined = false;
  r.y.defined = false;
  r.z.defined = false;
  r.preserve_aspect = false;
}

void GraphicImpl::defaultRequisition(Graphic::Requisition &r)
{
  Coord zero = Coord(0.0);
  require(r.x, zero, zero, zero, zero);
  require(r.y, zero, zero, zero, zero);
  require(r.z, zero, zero, zero, zero);
  r.preserve_aspect = false;
}

void GraphicImpl::require(Graphic::Requirement &r, Coord natural, Coord stretch, Coord shrink, Coord alignment)
{
  r.defined = true;
  r.natural = natural;
  r.maximum = natural + stretch;
  r.minimum = natural - shrink;
  r.align = alignment;
}

void GraphicImpl::requireLeadTrail(Graphic::Requirement &r,
				   Coord natural_lead, Coord max_lead, Coord min_lead,
				   Coord natural_trail, Coord max_trail, Coord min_trail)
{
  Coord zero = Coord(0);
  r.defined = true;
  natural_lead = Math::max(min_lead, Math::min(max_lead, natural_lead));
  max_lead = Math::max(max_lead, natural_lead);
  min_lead = Math::min(min_lead, natural_lead);
  natural_trail = Math::max(min_trail, Math::min(max_trail, natural_trail));
  max_trail = Math::max(max_trail, natural_trail);
  min_trail = Math::min(min_trail, natural_trail);
  r.natural = natural_lead + natural_trail;
  if (natural_lead == zero)
    {
      r.minimum = min_trail;
      r.maximum = max_trail;
      r.align = Coord(0);
    }
  else if (natural_trail == zero)
    {
      r.minimum = min_lead;
      r.maximum = max_lead;
      r.align = Coord(1);
    }
  else
    {
      r.minimum = r.natural * Math::max(min_lead / natural_lead, min_trail / natural_trail);
      r.maximum = r.natural * Math::min(max_lead / natural_lead, max_trail / natural_trail);
      if (r.natural == zero)
	r.align = zero;
      else
	r.align = natural_lead / r.natural;
    }
}

Graphic::Requirement *GraphicImpl::requirement(Graphic::Requisition &r, Axis a)
{
  Graphic::Requirement *req;
  switch (a)
    {
    case xaxis: req = &r.x; break;
    case yaxis: req = &r.y; break;
    case zaxis: req = &r.z; break;
    default: req = 0; break;
    }
  return req;
}

void GraphicImpl::defaultExtension (const Allocation::Info &info, Region_ptr region)
{
  if (!CORBA::is_nil(info.allocation))
    {
      if (CORBA::is_nil(info.transformation))
	region->mergeUnion(info.allocation);
      else
	{
	  Impl_var<RegionImpl> tmp(new RegionImpl(info.allocation, info.transformation));
	  region->mergeUnion(Region_var(tmp->_this()));
	}
    }
}

void GraphicImpl::naturalAllocation (Graphic_ptr g, RegionImpl &nat)
{
  Requisition r;
  GraphicImpl::initRequisition(r);

  g->request(r);
  if (r.x.defined)
    {
      nat.xalign = r.x.align;
      nat.lower.x = -r.x.align * r.x.natural;
      nat.upper.x = nat.lower.x + r.x.natural;
      nat.valid = true;
    }
  if (r.y.defined)
    {
      nat.yalign = r.y.align;
      nat.lower.y = -r.y.align * r.y.natural;
      nat.upper.y = nat.lower.y + r.y.natural;
      nat.valid = true;
    }
  if (r.z.defined)
    {
      nat.lower.z = -r.z.align * r.z.natural;
      nat.upper.z = nat.lower.z + r.z.natural;
      nat.zalign = r.z.align;
      nat.valid = true;
    }
}

void GraphicImpl::transformRequest (Graphic::Requisition& req, Transform_ptr tx)
{
  if (CORBA::is_nil(tx) || tx->Identity()) return;
  if (Math::equal(req.x.natural, req.x.maximum, tol) &&
      Math::equal(req.y.natural, req.y.maximum, tol) &&
      Math::equal(req.x.natural, req.x.minimum, tol) &&
      Math::equal(req.y.natural, req.y.minimum, tol))
    fixedTransformRequest(req, tx);
  else
    flexibleTransformRequest(req, tx);
}

Vertex GraphicImpl::transformAllocate(RegionImpl &region, const Graphic::Requisition &req, Transform_ptr t)
{
  SectionLog section("GraphicImpl::transformAllocation");
  Vertex delta;
  delta.x = Coord(0); delta.y = Coord(0); delta.z = Coord(0);
  if (!rotated(t))
    {
      TransformImpl tx;
      tx.copy(t);
      tx.invert();
      region.applyTransform(&tx);
      region.xalign = req.x.align;
      region.yalign = req.y.align;
      region.zalign = req.z.align;
    }
  else
    {
      Vertex center;
      double x_len, y_len;
      center.x = (region.lower.x + region.upper.x) * 0.5;
      center.y = (region.lower.y + region.upper.y) * 0.5;

      Transform::Matrix m;
      t->storeMatrix(m);

      Graphic::Requisition r[2], total;
      GraphicImpl::initRequisition(r[0]);	
      GraphicImpl::initRequisition(r[1]);	
      GraphicImpl::initRequisition(total);	
   
      RegionImpl a[2];
      double a0 = -1; double a1 = -1;
      if (!Math::equal(m[0][0], 0.0, tol)) a0 = Math::abs(m[0][1] / m[0][0]);
      if (!Math::equal(m[1][0], 0.0, tol)) a1 = Math::abs(m[1][1] / m[1][0]);

      r[0].x.natural = Math::abs(req.x.natural*m[0][0]);
      r[0].x.maximum = Math::abs(req.x.maximum*m[0][0]);
      r[0].x.minimum = Math::abs(req.x.minimum*m[0][0]);
      r[0].x.defined = true;
      r[0].y.natural = Math::abs(req.x.natural*m[0][1]);
      r[0].y.maximum = Math::abs(req.x.maximum*m[0][1]);
      r[0].y.minimum = Math::abs(req.x.minimum*m[0][1]);
      r[0].y.defined = true;

      r[1].x.natural = Math::abs(req.y.natural*m[1][0]);
      r[1].x.maximum = Math::abs(req.y.maximum*m[1][0]);
      r[1].x.minimum = Math::abs(req.y.minimum*m[1][0]);
      r[1].x.defined = true;
      r[1].y.natural = Math::abs(req.y.natural*m[1][1]);
      r[1].y.maximum = Math::abs(req.y.maximum*m[1][1]);
      r[1].y.minimum = Math::abs(req.y.minimum*m[1][1]);
      r[1].y.defined = true;

      total.x.natural = r[0].x.natural + r[1].x.natural;
      total.x.maximum = r[0].x.maximum + r[1].x.maximum;
      total.x.minimum = r[0].x.minimum + r[1].x.minimum;
      total.x.defined = true;
      total.y.natural = r[0].y.natural + r[1].y.natural;
      total.y.maximum = r[0].y.maximum + r[1].y.maximum;
      total.y.minimum = r[0].y.minimum + r[1].y.minimum;
      total.y.defined = true;

      computeAllocations(xaxis, total, 2, r, region, a);
      computeAllocations(yaxis, total, 2, r, region, a);

      double x0, y0, x1, y1;
      x0 = a[0].upper.x - a[0].lower.x;
      y0 = a[0].upper.y - a[0].lower.y;
      x1 = a[1].upper.x - a[1].lower.x;
      y1 = a[1].upper.y - a[1].lower.y;
      compensate(a0, x0, y0);
      compensate(a1, x1, y1);

      x_len = sqrt(x0*x0 + y0*y0)/sqrt(m[0][0]*m[0][0]+m[0][1]*m[0][1]);
      y_len = sqrt(x1*x1 + y1*y1)/sqrt(m[1][0]*m[1][0]+m[1][1]*m[1][1]);

      region.xalign = req.x.align;
      region.yalign = req.y.align;
      region.zalign = req.z.align;

      t->inverseTransformVertex(center);
      delta.x = Coord(center.x - x_len*0.5 - region.lower.x);
      delta.y = Coord(center.y - y_len*0.5 - region.lower.y);

      region.upper.x = Coord(region.lower.x + x_len);
      region.upper.y = Coord(region.lower.y + y_len);
    }
  return delta;
}
