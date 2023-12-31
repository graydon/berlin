/*$Id: Compositor.cc,v 1.6 2000/09/19 21:11:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#include "Text/Compositor.hh"
#include <Berlin/RegionImpl.hh>
#include <Berlin/GraphicImpl.hh>
#include <Warsaw/DrawingKit.hh>
#include <Berlin/Math.hh>

using namespace Warsaw;

void Compositor::set_span(RegionImpl *r, Axis a, Coord origin, Coord length, Alignment align)
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

Coord Compositor::compute_length(const Graphic::Requirement &r, const Region::Allotment &a)
{
  Coord length = a.end - a.begin;
  Coord s_a = a.align;
  Coord r_a = r.align;
  if (r_a == 0) length *= (1 - s_a);
  else if (r_a == 1) length *= s_a;
  else length *= Math::min(s_a / r_a, (1 - s_a) / (1 - r_a));
  return length;
}

Coord Compositor::compute_squeeze(const Graphic::Requirement &r, Coord length)
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

void LRCompositor::request(long n, Graphic::Requisition *requests, DrawingKit_ptr, Graphic::Requisition &result)
{
  Warsaw::Graphic::Requirement *r;
  /*
   * tile horizontally
   */
  Coord natural = 0., min_size = 0., max_size = 0.;
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], xaxis);
      if (r->defined)
	{
	  natural += r->natural;
	  max_size += r->maximum;
	  min_size += r->minimum;
	}
    }
  r = GraphicImpl::requirement(result, xaxis);
  r->defined = true;
  r->natural = natural;
  r->maximum = max_size;
  r->minimum = min_size;
  r->align = 0.;
  /*
   * align vertically
   */
  Coord natural_lead = 0.;
  Coord natural_trail = 0.;
  Coord min_lead, max_lead, min_trail, max_trail;
  
  min_lead = -GraphicImpl::infinity;
  max_lead = GraphicImpl::infinity;
  min_trail = -GraphicImpl::infinity;
  max_trail = GraphicImpl::infinity;
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], yaxis);
      if (r->defined)
	{
	  Coord r_nat = r->natural;
	  Coord r_max = r->maximum;
	  Coord r_min = r->minimum;
	  Coord r_align = r->align;
	  Coord r_inv_align = Coord(1) - r_align;
	  natural_lead = Math::max(natural_lead, Coord(r_nat * r_align));
	  max_lead = Math::min(max_lead, Coord(r_max * r_align));
	  min_lead = Math::max(min_lead, Coord(r_min * r_align));
	  natural_trail = Math::max(natural_trail, Coord(r_nat * r_inv_align));
	  max_trail = Math::min(max_trail, Coord(r_max * r_inv_align));
	  min_trail = Math::max(min_trail, Coord(r_min * r_inv_align));
	}
    }
  r = GraphicImpl::requirement(result, yaxis);
  GraphicImpl::require_lead_trail(*r, natural_lead, max_lead, min_lead, natural_trail, max_trail, min_trail);

  requisition = result;
}

void LRCompositor::allocate(long n, Graphic::Requisition *requests, DrawingKit_ptr, Region_ptr given, Allocations result)
{
  Warsaw::Graphic::Requirement* r;
  Warsaw::Region::Allotment a;
  /*
   * tile horizontally
   */
  r = GraphicImpl::requirement(requisition, xaxis);
  given->span(xaxis, a);
  Coord length = compute_length(*r, a);
  bool growing = length > r->natural;
  bool shrinking = length < r->natural;
  double f = compute_squeeze(*r, length);
  Coord p = a.begin + a.align * (a.end - a.begin);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], xaxis);
      if (r->defined)
	{
	  Coord cspan = r->natural;
	  if (growing) cspan += f * (r->maximum - r->natural);
	  else if (shrinking) cspan -= f * (r->natural - r->minimum);
	  set_span(result[i], xaxis, p + cspan * r->align, cspan, r->align);
	  p += cspan;
        }
      else set_span(result[i], xaxis, p, 0., 0.);
    }
  /*
   * align vertically
   */
  given->span(yaxis, a);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], yaxis);
      if (r->defined)
	{
	  Coord length = Math::max(Math::min(a.end - a.begin, r->maximum), r->minimum);
	  set_span(result[i], yaxis, a.begin + a.align*(a.end-a.begin), length, r->align);
	}
      else set_span(result[i], yaxis, 0., 0., 0.);
    }
}

void TBCompositor::request(long n, Graphic::Requisition *requests, DrawingKit_ptr, Graphic::Requisition &result)
{
  Warsaw::Graphic::Requirement *r;
  /*
   * tile vertically
   */
  Coord natural = 0., min_size = 0., max_size = 0.;
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], yaxis);
      if (r->defined)
	{
	  natural += r->natural;
	  max_size += r->maximum;
	  min_size += r->minimum;
	}
    }
  r = GraphicImpl::requirement(result, yaxis);
  r->defined = true;
  r->natural = natural;
  r->maximum = max_size;
  r->minimum = min_size;
  r->align = 0.;
  /*
   * align horizontally
   */
  Coord natural_lead = 0.;
  Coord natural_trail = 0.;
  Coord min_lead, max_lead, min_trail, max_trail;
  
  min_lead = -GraphicImpl::infinity;
  max_lead = GraphicImpl::infinity;
  min_trail = -GraphicImpl::infinity;
  max_trail = GraphicImpl::infinity;
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], xaxis);
      if (r->defined)
	{
	  Coord r_nat = r->natural;
	  Coord r_max = r->maximum;
	  Coord r_min = r->minimum;
	  Coord r_align = r->align;
	  Coord r_inv_align = Coord(1) - r_align;
	  natural_lead = Math::max(natural_lead, Coord(r_nat * r_align));
	  max_lead = Math::min(max_lead, Coord(r_max * r_align));
	  min_lead = Math::max(min_lead, Coord(r_min * r_align));
	  natural_trail = Math::max(natural_trail, Coord(r_nat * r_inv_align));
	  max_trail = Math::min(max_trail, Coord(r_max * r_inv_align));
	  min_trail = Math::max(min_trail, Coord(r_min * r_inv_align));
	}
    }
  r = GraphicImpl::requirement(result, xaxis);
  GraphicImpl::require_lead_trail(*r, natural_lead, max_lead, min_lead, natural_trail, max_trail, min_trail);

  requisition = result;
}

void TBCompositor::allocate(long n, Graphic::Requisition *requests, DrawingKit_ptr, Region_ptr given, Allocations result)
{
  Warsaw::Graphic::Requirement* r;
  Warsaw::Region::Allotment a;
  /*
   * tile vertically
   */
  r = GraphicImpl::requirement(requisition, yaxis);
  given->span(yaxis, a);
  Coord length = compute_length(*r, a);
  bool growing = length > r->natural;
  bool shrinking = length < r->natural;
  double f = compute_squeeze(*r, length);
  Coord p = a.begin + a.align * (a.end - a.begin);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], yaxis);
      if (r->defined)
	{
	  Coord cspan = r->natural;
	  if (growing) cspan += f * (r->maximum - r->natural);
	  else if (shrinking) cspan -= f * (r->natural - r->minimum);
	  set_span(result[i], yaxis, p + cspan * r->align, cspan, r->align);
	  p += cspan;
        }
      else set_span(result[i], yaxis, p, 0., 0.);
    }
  /*
   * align horizontally
   */
  given->span(xaxis, a);
  for (long i = 0; i < n; i++)
    {
      r = GraphicImpl::requirement(requests[i], xaxis);
      if (r->defined)
	{
	  Coord length = Math::max(Math::min(a.end - a.begin, r->maximum), r->minimum);
	  set_span(result[i], xaxis, a.begin + a.align*(a.end-a.begin), length, r->align);
	}
      else set_span(result[i], xaxis, 0., 0., 0.);
    }
}
