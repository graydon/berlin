/*$Id: LayoutManager.cc,v 1.11 2001/04/24 05:04:49 stefan Exp $
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
#include <Berlin/GraphicImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/Math.hh>
#include "Layout/LayoutManager.hh"
#include <Warsaw/IO.hh>

using namespace Warsaw;

// class LayoutManager

LayoutManager::LayoutManager() {}
LayoutManager::~LayoutManager() {}

void LayoutManager::set_span(RegionImpl *r, Axis a, Coord origin, Coord length, Alignment align)
{
  Coord begin = origin - length * align;
  Coord end = begin + length;
  switch (a)
    {
    case xaxis:
      r->lower.x = begin;
      r->upper.x = end;
      r->xalign = align;
      break;
    case yaxis:
      r->lower.y = begin;
      r->upper.y = end;
      r->yalign = align;
      break;
    case zaxis:
      r->lower.z = begin;
      r->upper.z = end;
      r->zalign = align;
      break;
    }
}

// class LayoutAlign

LayoutAlign::LayoutAlign(Axis a) : axis(a) {}
LayoutAlign::~LayoutAlign() {}
LayoutManager *LayoutAlign::clone() { return new LayoutAlign(axis);}

void LayoutAlign::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  Graphic::Requirement *r, *rr = GraphicImpl::requirement(result, axis);
  rr->defined = false;

  Coord natural_lead = -GraphicImpl::infinity;
  Coord natural_trail = -GraphicImpl::infinity;
  Coord min_lead = -GraphicImpl::infinity;  
  Coord max_lead = GraphicImpl::infinity;  
  Coord min_trail = -GraphicImpl::infinity;  
  Coord max_trail = GraphicImpl::infinity;

  for (int i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], axis);
      if (r->defined)
	{
	  //. maximum of the natural child lengths becomes natural length
	  natural_lead = Math::max(natural_lead, r->natural * r->align);
	  //. smallest children's maximum length becomes maximum length
	  max_lead = Math::min(max_lead, r->maximum * r->align);
	  //. largest children's minimum length becomes minimum length
	  min_lead = Math::max(min_lead, r->minimum * r->align);
	  //. maximum of the natural child lengths becomes natural length
	  natural_trail = Math::max(natural_trail, r->natural * (1. - r->align));
	  //. smallest children's maximum length becomes maximum length
	  max_trail = Math::min(max_trail, r->maximum * (1. - r->align));
	  //. largest children's minimum length becomes minimum length
	  min_trail = Math::max(min_trail, r->minimum * (1. - r->align));
	  rr->defined = true;
	}
    }

  if (rr->defined)
    GraphicImpl::require_lead_trail(*rr, natural_lead, max_lead, min_lead, natural_trail, max_trail, min_trail);
}

void LayoutAlign::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  Graphic::Requirement* r;
  Region::Allotment a;
  given->span(axis, a);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], axis);
      if (r->defined)
	{
	  Coord length = Math::max(Math::min(a.end - a.begin, r->maximum), r->minimum);
	  set_span(result[i], axis, a.begin + a.align*(a.end-a.begin), length, r->align);
	}
      else set_span(result[i], axis, Coord(0), Coord(0), Coord(0));
    }
}

// class LayoutCenter

LayoutCenter::LayoutCenter(Axis a, Alignment align) : axis(a), alignment(align) {}
LayoutCenter::~LayoutCenter() {}
LayoutManager *LayoutCenter::clone() { return new LayoutCenter(axis, alignment);}

void LayoutCenter::request(long, Graphic::Requisition*, Graphic::Requisition &result)
{
  Graphic::Requirement *r = GraphicImpl::requirement(result, axis);
  r->align = alignment;
}

void LayoutCenter::allocate(long, Graphic::Requisition *requests, Region_ptr, LayoutManager::Allocations result)
{
  Region::Allotment a;
  result[0]->span(axis, a);
  Graphic::Requirement *r = GraphicImpl::requirement(requests[0], axis);
  if (r->defined)
    {
      Coord length = a.end - a.begin;
      Coord n = Math::min(r->maximum, Math::max(r->minimum, length));
      set_span(result[0], axis, a.begin + a.align * length + (r->align - alignment) * n, length, r->align);
    }
}

// class LayoutFixed

LayoutFixed::LayoutFixed(Axis a, Coord s) : axis(a), size(s) {}
LayoutFixed::~LayoutFixed() {}
LayoutManager *LayoutFixed::clone() { return new LayoutFixed(axis, size);}

void LayoutFixed::request(long, Graphic::Requisition *, Graphic::Requisition &result)
{
  Graphic::Requirement *r = GraphicImpl::requirement(result, axis);
  r->natural = size;
  r->maximum = size;
  r->minimum = size;
  if (!r->defined)
    {
      r->defined = true;
      r->align = Alignment(0);
    }
}

void LayoutFixed::allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations result)
{
  Region::Allotment a;
  result[0]->span(axis, a);
  set_span(result[0], axis, a.begin + a.align * (a.end - a.begin), size, a.align);
}

// class LayoutMargin

LayoutMargin::LayoutMargin(Coord m)
  : lnatural(m), lstretch(0), lshrink(0),
    rnatural(m), rstretch(0), rshrink(0),
    tnatural(m), tstretch(0), tshrink(0),
    bnatural(m), bstretch(0), bshrink(0)
{}

LayoutMargin::LayoutMargin(Coord h, Coord v)
  : lnatural(h), lstretch(0), lshrink(0),
    rnatural(h), rstretch(0), rshrink(0),
    tnatural(v), tstretch(0), tshrink(0),
    bnatural(v), bstretch(0), bshrink(0)
{}

LayoutMargin::LayoutMargin(Coord l, Coord r, Coord t, Coord b)
  : lnatural(l), lstretch(0), lshrink(0),
    rnatural(r), rstretch(0), rshrink(0),
    tnatural(t), tstretch(0), tshrink(0),
    bnatural(b), bstretch(0), bshrink(0)
{}

LayoutMargin::LayoutMargin(Coord lm, Coord lS, Coord ls,
			   Coord rm, Coord rS, Coord rs,
			   Coord tm, Coord tS, Coord ts,
			   Coord bm, Coord bS, Coord bs)
  : lnatural(lm), lstretch(lS), lshrink(ls),
    rnatural(rm), rstretch(rS), rshrink(rs),
    tnatural(tm), tstretch(tS), tshrink(ts),
    bnatural(bm), bstretch(bS), bshrink(bs)
{}

LayoutMargin::~LayoutMargin() {}

LayoutManager *LayoutMargin::clone()
{
  return new LayoutMargin(lnatural, lstretch, lshrink,
			  rnatural, rstretch, rshrink,
			  tnatural, tstretch, tshrink,
			  bnatural, bstretch, bshrink);
}

void LayoutMargin::request(long, Graphic::Requisition *, Graphic::Requisition &result)
{
  Graphic::Requirement &rx = result.x;
  if (!rx.defined)
    {
      rx.defined = true;
      rx.natural = Coord(0);
      rx.maximum = Coord(0);
      rx.minimum = Coord(0);
    }

  Coord dx = lnatural + rnatural;
  rx.natural += dx;
  rx.maximum += dx + (lstretch + rstretch);
  rx.minimum += dx - (lshrink + rshrink);

  Graphic::Requirement &ry = result.y;
  if (!ry.defined)
    {
      ry.defined = true;
      ry.natural = Coord(0);
      ry.maximum = Coord(0);
      ry.minimum = Coord(0);
    }

  Coord dy = bnatural + tnatural;
  ry.natural += dy;
  ry.maximum += dy + (bstretch + tstretch);
  ry.minimum += dy - (bshrink + tshrink);
  requisition = result;
}

void LayoutMargin::allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations result)
{
  allocate_axis(xaxis, lnatural, lstretch, lshrink, rnatural, rstretch, rshrink, result);
  allocate_axis(yaxis, tnatural, tstretch, tshrink, bnatural, bstretch, bshrink, result);
}

void LayoutMargin::allocate_axis(Axis axis, Coord natural_lead, Coord stretch_lead, Coord shrink_lead,
				 Coord natural_trail, Coord stretch_trail, Coord shrink_trail,
				 LayoutManager::Allocations result)
{
  Region::Allotment a;
  result[0]->span(axis, a);
  Graphic::Requirement *r = GraphicImpl::requirement(requisition, axis);
  Coord lead = span(a.end - a.begin, *r, natural_lead, stretch_lead, shrink_lead);
  Coord trail = span(a.end - a.begin, *r, natural_trail, stretch_trail, shrink_trail);
  Coord length = a.end - a.begin - (lead + trail);
  Coord origin = a.begin + a.align * (a.end - a.begin) + ((1 - r->align) * lead - r->align * trail);
  set_span(result[0], axis, origin, length, a.align);
}

Coord LayoutMargin::span(Coord span, Graphic::Requirement &total, Coord natural, Coord stretch, Coord shrink)
{
  Coord extra = span - total.natural;
  Coord result = natural;
  Coord ss = 0.;
  Coord total_stretch = total.maximum - total.natural;
  Coord total_shrink = total.natural - total.minimum;
  if (extra > 0 && total_stretch > 0) ss = stretch / total_stretch;
  else if (extra < 0 && total_shrink > 0) ss = shrink / total_shrink;
  return result + ss * extra;
}

// class LayoutNatural

LayoutNatural::LayoutNatural(Axis a, Coord n) : axis(a), natural(n) {}
LayoutNatural::~LayoutNatural() {}
LayoutManager *LayoutNatural::clone() { return new LayoutNatural(axis, natural);}

void LayoutNatural::request(long, Graphic::Requisition *, Graphic::Requisition &result)
{
  Graphic::Requirement *r = GraphicImpl::requirement(result, axis);
  r->defined = true;
  r->natural = natural;
}

void LayoutNatural::allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations) {} // leave it as is

// class LayoutSuperpose

LayoutSuperpose::LayoutSuperpose(LayoutManager *f, LayoutManager *s) : first(f), second(s), third(0) {}
LayoutSuperpose::LayoutSuperpose(LayoutManager *f, LayoutManager *s, LayoutManager *t) : first(f), second(s), third(t) {}
LayoutSuperpose::~LayoutSuperpose()
{
  delete first;
  delete second;
  delete third;
}

LayoutManager *LayoutSuperpose::clone()
{
  return new LayoutSuperpose(first ? first->clone() : 0, second ? second->clone() : 0, third ? third->clone() : 0);
}

void LayoutSuperpose::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  if (first) first->request(n, requests, result);
  if (second) second->request(n, requests, result);
  if (third) third->request(n, requests, result);
}

void LayoutSuperpose::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  if (first) first->allocate(n, requests, given, result);
  if (second) second->allocate(n, requests, given, result);
  if (third) third->allocate(n, requests, given, result);
}

// class LayoutTile

LayoutTile::LayoutTile(Axis a) : axis(a) {}
LayoutTile::~LayoutTile() {}
LayoutManager *LayoutTile::clone() { return new LayoutTile(axis);}

void LayoutTile::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  compute_request(axis, Alignment(0), n, requests, result);
  requisition = result;
}

void LayoutTile::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  compute_allocations(axis, requisition, false, n, requests, given, result);
}

void LayoutTile::compute_request(Axis a, Alignment align, long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  Graphic::Requirement *r;
  Coord natural = Coord(0), min_size = Coord(0), max_size = Coord(0);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], a);
      if (r->defined)
	{
	  natural += r->natural;
	  max_size += r->maximum;
	  min_size += r->minimum;
	}
    }
  r = GraphicImpl::requirement(result, a);
  r->defined = true;
  r->natural = natural;
  r->maximum = max_size;
  r->minimum = min_size;
  r->align = align;
}

void LayoutTile::compute_allocations(Axis axis, Graphic::Requisition &total, bool first_aligned,
				     long n, Graphic::Requisition *requests, Region_ptr given,
				     LayoutManager::Allocations result)
{
  Graphic::Requirement *r = GraphicImpl::requirement(total, axis);
  Region::Allotment a;
  given->span(axis, a);
  Coord length = compute_length(*r, a);
  bool growing = length > r->natural;
  bool shrinking = length < r->natural;
  Coord f = compute_squeeze(*r, length);
  Coord p = a.begin + a.align * (a.end - a.begin);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], axis);
      if (r->defined)
	{
	  Coord cspan = r->natural;
	  if (growing) cspan += f * (r->maximum - r->natural);
	  else if (shrinking) cspan -= f * (r->natural - r->minimum);
	  if (first_aligned && i == 0) p -= r->align * cspan;
	  set_span(result[i], axis, p + cspan * r->align, cspan, r->align);
	  p += cspan;
        }
      else set_span(result[i], axis, p, Coord(0), Coord(0));
    }
}

Coord LayoutTile::compute_length(const Graphic::Requirement &r, const Region::Allotment &a)
{
  Coord length = a.end - a.begin;
  Coord s_a = a.align;
  Coord r_a = r.align;
  if (r_a == 0) length *= (1 - s_a);
  else if (r_a == 1) length *= s_a;
  else length *= Math::min(s_a / r_a, (1 - s_a) / (1 - r_a));
  return length;
}

Coord LayoutTile::compute_squeeze(const Graphic::Requirement &r, Coord length)
{
  Coord f;
  Coord nat = r.natural;
  if (length > nat && r.maximum > nat)
    f = (length - nat) / (r.maximum - nat);
  else if (length < nat && r.minimum < nat)
    f = (nat - length) / (nat - r.minimum);
  else f = 0.0;
  return f;
}

// class LayoutTileReversed

LayoutTileReversed::LayoutTileReversed(Axis a) { axis = a;}
LayoutTileReversed::~LayoutTileReversed() {}
LayoutManager *LayoutTileReversed::clone() { return new LayoutTileReversed(axis);}

void LayoutTileReversed::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  LayoutTile::compute_request(axis, Alignment(1), n, requests, result);
  requisition = result;
}

void LayoutTileReversed::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  compute_reversed_allocations(axis, requisition, false, n, requests, given, result);
}

void LayoutTileReversed::compute_reversed_allocations(Axis axis, Graphic::Requisition &total, bool first_aligned,
						      long n, Graphic::Requisition *requests, Region_ptr given,
						      LayoutManager::Allocations result)
{
  Graphic::Requirement *r;
  Region::Allotment a;
  r = GraphicImpl::requirement(total, axis);
  given->span(axis, a);
  Coord length = LayoutTile::compute_length(*r, a);
  bool growing = length > r->natural;
  bool shrinking = length < r->natural;
  Coord f = LayoutTile::compute_squeeze(*r, length);
  Coord p = a.begin + a.align * (a.end - a.begin);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], axis);
      if (r->defined)
	{
	  Coord cspan = r->natural;
	  if (growing) cspan += f * (r->maximum - r->natural);
	  else if (shrinking) cspan -= f * (r->natural - r->minimum);
	  if (first_aligned && i == 0) p += (1 - r->align) * cspan;
	  p -= cspan;
	  set_span(result[i], axis, p + r->align * cspan, cspan, r->align);
        }
      else set_span(result[i], axis, p, Coord(0), Coord(0));
    }
}

// class LayoutTileFirstAligned

LayoutTileFirstAligned::LayoutTileFirstAligned(Axis a) { axis = a;}
LayoutTileFirstAligned::~LayoutTileFirstAligned() {}
LayoutManager *LayoutTileFirstAligned::clone() { return new LayoutTileFirstAligned(axis);}

void LayoutTileFirstAligned::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  compute_request_first_aligned(axis, n, requests, result);
  requisition = result;
}

void LayoutTileFirstAligned::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  LayoutTile::compute_allocations(axis, requisition, true, n, requests, given, result);
}

void LayoutTileFirstAligned::compute_request_first_aligned(Axis a, long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  Graphic::Requirement *r;
  Coord natural_lead = Coord(0);
  Coord min_lead = Coord(0);
  Coord max_lead = Coord(0);
  Coord natural_trail = Coord(0);
  Coord min_trail = Coord(0);
  Coord max_trail = Coord(0);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], a);
      if (r->defined)
	{
	  if (i == 0)
	    {
	      Alignment a = r->align;
	      Alignment aa = 1 - a;
	      natural_lead = a * r->natural;
	      max_lead = a * r->maximum;
	      min_lead = a * r->minimum;
	      natural_trail = aa * r->natural;
	      max_trail = aa * r->maximum;
	      min_trail = aa * r->minimum;
            }
	  else
	    {
	      natural_trail += r->natural;
	      max_trail += r->maximum;
	      min_trail += r->minimum;
            }
        }
    }
  r = GraphicImpl::requirement(result, a);
  GraphicImpl::require_lead_trail(*r, natural_lead, max_lead, min_lead, natural_trail, max_trail, min_trail);
}

// class LayoutTileReversedFirstAligned

LayoutTileReversedFirstAligned::LayoutTileReversedFirstAligned(Axis a) { axis = a;}
LayoutTileReversedFirstAligned::~LayoutTileReversedFirstAligned() {}
LayoutManager *LayoutTileReversedFirstAligned::clone() { return new LayoutTileReversedFirstAligned(axis);}

void LayoutTileReversedFirstAligned::request(long n, Graphic::Requisition *requests, Graphic::Requisition &result)
{
  LayoutTileFirstAligned::compute_request_first_aligned(axis, n, requests, result);
  requisition = result;
}

void LayoutTileReversedFirstAligned::allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result)
{
  LayoutTileReversed::compute_reversed_allocations(axis, requisition, true, n, requests, given, result);
}

// class LayoutVariable

LayoutVariable::LayoutVariable(Axis a, Coord S, Coord s) : axis(a), stretch(S), shrink(s) {}
LayoutVariable::~LayoutVariable() {}
LayoutManager *LayoutVariable::clone() { return new LayoutVariable(axis, stretch, shrink);}

void LayoutVariable::request(long, Graphic::Requisition *, Graphic::Requisition &result)
{
  Graphic::Requirement *r = GraphicImpl::requirement(result, axis);
  r->maximum = r->natural + stretch;
  r->minimum = r->natural - shrink;
}

void LayoutVariable::allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations) {}    // leave it as is
