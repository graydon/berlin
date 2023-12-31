/*$Id: GridImpl.cc,v 1.12 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#include "Layout/GridImpl.hh"
#include "Layout/LayoutManager.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Math.hh"
#include "Warsaw/Traversal.hh"

static void setSpan(GridImpl::Span &s, Coord origin, Coord length, Alignment align)
{
  Coord begin = origin - length * align;
  Coord end = begin + length;
  s.lower = begin;
  s.upper = end;
  s.align = align;
}

static void spansToRegion(GridImpl::Span &x_span, GridImpl::Span &y_span, RegionImpl *r)
{
  r->valid = true;
  r->lower.x = x_span.lower;
  r->upper.x = x_span.upper;
  r->xalign = x_span.align;
  r->lower.y = y_span.lower;
  r->upper.y = y_span.upper;
  r->yalign = y_span.align;
}

static void offsetRegion(RegionImpl *r, Coord dx, Coord dy)
{
  r->lower.x += dx;
  r->upper.x += dx;
  r->lower.y += dy;
  r->upper.y += dy;
}

class LayoutAlignRequest
{
public:
  LayoutAlignRequest();
  
  void margin(Coord margin);
  
  void accumulate(const Graphic::Requirement &r);
  void requirement(Graphic::Requirement &r) const;
 
protected:
  Coord natural_lead;
  Coord min_lead;
  Coord max_lead;
  Coord natural_trail;
  Coord min_trail;
  Coord max_trail;
};
 
LayoutAlignRequest::LayoutAlignRequest()
  : natural_lead(0), min_lead(-GraphicImpl::infinity), max_lead(GraphicImpl::infinity),
    natural_trail(0), min_trail(-GraphicImpl::infinity), max_trail(GraphicImpl::infinity)
{
}
 
void LayoutAlignRequest::margin(Coord margin)
{
  natural_lead += margin;
  min_lead -= margin;
  max_lead += margin;
  natural_trail += margin;
  min_trail -= margin;
  max_trail += margin;
}
 
void LayoutAlignRequest::accumulate(const Graphic::Requirement &r)
{
  if (r.defined)
    {
      Coord r_nat = r.natural;
      Coord r_max = r.maximum;
      Coord r_min = r.minimum;
      Coord r_align = r.align;
      Coord r_inv_align = Coord(1) - r_align;
      natural_lead = Math::max(natural_lead, Coord(r_nat * r_align));
      max_lead = Math::min(max_lead, Coord(r_max * r_align));
      min_lead = Math::max(min_lead, Coord(r_min * r_align));
      natural_trail = Math::max(natural_trail, Coord(r_nat * r_inv_align));
      max_trail = Math::min(max_trail, Coord(r_max * r_inv_align));
      min_trail = Math::max(min_trail, Coord(r_min * r_inv_align));
    }
}
 
void LayoutAlignRequest::requirement(Graphic::Requirement &r) const
{
  GraphicImpl::requireLeadTrail(r, natural_lead, max_lead, min_lead, natural_trail, max_trail, min_trail);
}
 
class LayoutTileRequest
{
public:
  LayoutTileRequest();
  
  void space(Coord space);
  void flexible();
 
  void accumulate(const Graphic::Requirement &r);
  void requirement(Graphic::Requirement &r) const;
 
  Coord natural;
  Coord min_size;
  Coord max_size;
};
 
LayoutTileRequest::LayoutTileRequest() : natural(0), min_size(0), max_size(0) {}

void LayoutTileRequest::space(Coord space)
{
  natural += space;
  max_size += space;
  min_size += space;
}
 
void LayoutTileRequest::flexible()
{
  max_size = GraphicImpl::infinity;
}
 
void LayoutTileRequest::accumulate(const Graphic::Requirement &r)
{
  if (r.defined)
    {
      natural += r.natural;
      max_size += r.maximum;
      min_size += r.minimum;
    }
}
 
void LayoutTileRequest::requirement(Graphic::Requirement &r) const
{
  r.defined = true;
  r.natural = natural;
  r.maximum = max_size;
  r.minimum = min_size;
  r.align = 0.;
}

class LayoutTileAllocate
{
public:
  LayoutTileAllocate(Axis, Graphic::Requisition &, bool, Region_ptr);

  static Coord computeLength(const Graphic::Requirement &, const Region::Allotment &);
  static double computeSqueeze(const Graphic::Requirement &, Coord);

  void nextSpan(const Graphic::Requirement &, GridImpl::Span &);

private:
  bool first_aligned;
  bool growing;
  bool shrinking;
  Coord f;
  Coord p;
  long i;
};

LayoutTileAllocate::LayoutTileAllocate(Axis axis, Graphic::Requisition &total, bool fa, Region_ptr given)
{
  first_aligned = fa;
  Graphic::Requirement *r;
  Region::Allotment a;
  r = GraphicImpl::requirement(total, axis);
  given->span(axis, a);
  Coord length = computeLength(*r, a);
  growing = length > r->natural;
  shrinking = length < r->natural;
  f = computeSqueeze(*r, length);
  p = a.begin + a.align * (a.end - a.begin);
  i = 0;
}

Coord LayoutTileAllocate::computeLength(const Graphic::Requirement &r, const Region::Allotment &a)
{
  Coord length = a.end - a.begin;
  Coord a_a = a.align;
  Coord r_a = r.align;
  if (r_a == 0) length *= (1 - a_a);
  else if (r_a == 1) length *= a_a;
  else length *= Math::min(a_a / r_a, (1 - a_a) / (1 - r_a));
  return length;
}
 
double LayoutTileAllocate::computeSqueeze(const Graphic::Requirement &r, Coord length)
{
  double f;
  Coord nat = r.natural;
  if (length > nat && r.maximum > nat) f = (length - nat) / (r.maximum - nat);
  else if (length < nat && r.minimum < nat) f = (nat - length) / (nat - r.minimum);
  else f = 0.0;
  return f;
}

void LayoutTileAllocate::nextSpan(const Graphic::Requirement &r, GridImpl::Span &s)
{
  if (r.defined)
    {
      Coord cspan = r.natural;
      if (growing) cspan += f * (r.maximum - r.natural);
      else if (shrinking) cspan -= f * (r.natural - r.minimum);
      if (first_aligned && (i == 0)) p -= r.align * cspan;
      setSpan(s, p + cspan * r.align, cspan, r.align);
      p += cspan;
    }
  else setSpan(s, p, Coord(0), Alignment(0));
  ++i;
}

GridImpl::GridImpl(const Grid::Index &upper)
{
  dimensions[xaxis].init(upper.col, upper.row);
  dimensions[yaxis].init(upper.row, upper.col);
  cursor.col = cursor.row = 0;
  requested = false;
  GridImpl::initRequisition(requisition);
}

GridImpl::~GridImpl() {}

void GridImpl::append(Graphic_ptr g)
{
  replace(g, cursor);
  
  if (++cursor.col >= dimensions[xaxis].size())
    {
      long count = dimensions[yaxis].size();
      cursor.row = (cursor.row + 1) % count;
      cursor.col = 0;
    }
}

void GridImpl::prepend(Graphic_ptr g)
{
  if (--cursor.col < 0)
    {
      long count = dimensions[yaxis].size();
      cursor.row = (cursor.row - 1 + count) % count;
      cursor.col = dimensions[xaxis].size() - 1;
    }
  replace(g, cursor);
}

void GridImpl::request(Requisition &r)
{
  cacheRequest();
  r = requisition;
}

void GridImpl::traverse(Traversal_ptr traversal)
{
  Grid::Range range;
  range.lower.col = 0;
  range.upper.col = dimensions[xaxis].size();
  range.lower.row = 0;
  range.upper.row = dimensions[yaxis].size();
  
  traverseRange(traversal, range);
}

void GridImpl::needResize()
{
  requested = false;
  GraphicImpl::needResize();
}

void GridImpl::replace(Graphic_ptr g, const Grid::Index &i)
{
  Graphic_var old = dimensions[xaxis].children[i.col][i.row];
  if (!CORBA::is_nil(old)) old->removeParent(Graphic_var(_this()), index2tag(i));
  dimensions[xaxis].children[i.col][i.row] = g;
  dimensions[yaxis].children[i.row][i.col] = Graphic::_duplicate(g);
  g->addParent(Graphic_var(_this()), index2tag(i));
}

Grid::Index GridImpl::find(Traversal_ptr traversal)
{
  Grid::Range range;
  range.lower.col = 0;
  range.upper.col = dimensions[xaxis].size();
  range.lower.row = 0;
  range.upper.row = dimensions[yaxis].size();
  
  return findRange(traversal, range);
}

void GridImpl::allocateCell(Region_ptr given, const Grid::Index &i, Region_ptr a)
{
  Span *xspans = fullAllocate(xaxis, given);
  Span *yspans = fullAllocate(yaxis, given);
  Impl_var<RegionImpl> region(new RegionImpl);
  spansToRegion(xspans[i.col], yspans[i.row], region);
  a->copy(Region_var(region->_this()));
  delete [] xspans;
  delete [] yspans;
}

void GridImpl::requestRange(Graphic::Requisition &r, const Grid::Range &a)
{
  cacheRequest();
  
  partialRequest(xaxis, a.lower.col, a.upper.col, r.x);
  partialRequest(yaxis, a.lower.row, a.upper.row, r.y);
}

void GridImpl::traverseRange(Traversal_ptr traversal, const Grid::Range &a)
{
  Region_var given = traversal->allocation();
  if (!CORBA::is_nil(given))
    {
      if (traversal->intersectsAllocation())
	traverseWithAllocation(traversal, given, a);
    }
  else
    traverseWithoutAllocation(traversal, a);
}

Grid::Index GridImpl::findRange(Traversal_ptr traversal, const Grid::Range &a)
{
  Region_var given = traversal->allocation();
  Span *xspans = fullAllocate(xaxis, given);
  Span *yspans = fullAllocate(yaxis, given);
  Vertex lower;//, upper;
//   e->bounds(lower, upper);
  Coord x = lower.x;
  Coord y = lower.y;
  // If the point is outside the range find the outermost cell.
  long c, r;
  for (c = a.lower.col; c < (a.upper.col - 1); c++)
    if (x <= xspans[c].upper) break;
  for (r = a.lower.row; r < (a.upper.row - 1); r++)
    if (y <= yspans[r].upper) break;
  Grid::Index index;
  index.col = c;
  index.row = r;
  delete [] xspans;
  delete [] yspans;
  return index;
}

void GridImpl::rangePosition(Region_ptr given, const Grid::Range &a, Vertex &pos)
{
  Span *xspans = fullAllocate(xaxis, given);
  Span *yspans = fullAllocate(yaxis, given);
  pos.x = xspans[0].lower - xspans[a.lower.col].lower;
  pos.y = yspans[0].lower - yspans[a.lower.row].lower;
  pos.z = 0.;
  delete [] xspans;
  delete [] yspans;
}

Grid::Index GridImpl::upper()
{
  Grid::Index upper;
  upper.col = dimensions[xaxis].size();
  upper.row = dimensions[yaxis].size();
  return upper;
}

void GridImpl::allocate(Tag tag, const Allocation::Info &info)
{
  Impl_var<TransformImpl> tx(new TransformImpl);
  allocateCell(info.allocation, tag2index(tag), info.allocation);
  Impl_var<RegionImpl> region(new RegionImpl(info.allocation));
  region->normalize(tx);
  info.allocation->copy(Region_var(region->_this()));
  info.transformation->premultiply(Transform_var(tx->_this()));
}

void GridImpl::cacheRequest()
{
  if (!requested)
    {
      fullRequest(xaxis, yaxis);
      fullRequest(yaxis, xaxis);
      requested = true;
    }
}

void GridImpl::partialRequest(Axis axis, long begin, long end, Graphic::Requirement &r)
{
  GridDimension &d = dimensions[axis];
  LayoutTileRequest tile;
  for (long i = begin; i < end; i++)
    tile.accumulate(d.requirements[i]);
  tile.requirement(r);
}

void GridImpl::fullRequest(Axis axis, Axis direction)
{
  GridDimension &d = dimensions[axis];
  LayoutTileRequest tile;
  for (int i = 0; i < d.size(); i++)
    {
      LayoutAlignRequest align;
      for (vector<Graphic_var>::iterator j = d.children[i].begin(); j != d.children[i].end(); j++)
	if (!CORBA::is_nil(*j))
	  {
	    Graphic::Requisition r;
	    GraphicImpl::initRequisition(r);
	    GraphicImpl::defaultRequisition(r);
	    (*j)->request(r);
	    align.accumulate(axis == xaxis ? r.x : r.y);
	  }
      Graphic::Requirement &r = d.requirements[i];
      align.requirement(r);
      tile.accumulate(r);
    }
  Graphic::Requirement &r = *GraphicImpl::requirement(requisition, axis);
  tile.requirement(r);
}

GridImpl::Span *GridImpl::fullAllocate(Axis axis, Region_ptr given)
{
  GridDimension &d = dimensions[axis];
  Span *spans = new Span[d.size()];
  LayoutTileAllocate allocate(axis, requisition, false, given);
  for (int i = 0; i < d.size(); i++)
    allocate.nextSpan(d.requirements[i], spans[i]);
  return spans;
}

void GridImpl::traverseWithAllocation(Traversal_ptr t, Region_ptr given, const Grid::Range &range)
{
  Span *xspans = fullAllocate(xaxis, given);
  Span *yspans = fullAllocate(yaxis, given);
  Coord dx = xspans[0].lower - xspans[range.lower.col].lower;
  Coord dy = yspans[0].lower - yspans[range.lower.row].lower;
  Impl_var<TransformImpl> tx(new TransformImpl);
  Impl_var<RegionImpl> region(new RegionImpl);
  GridDimension &d = dimensions[yaxis];
  Grid::Index i;
  for (i.row = range.lower.row; i.row != range.upper.row; i.row++)
    for (i.col = range.lower.col; i.col != range.upper.col; i.col++)
      {
	tx->loadIdentity();
	spansToRegion(xspans[i.col], yspans[i.row], region);
	offsetRegion(region, dx, dy);
	region->normalize(tx);
	t->traverseChild(d.children[i.row][i.col], index2tag(i), Region_var(region->_this()), Transform_var(tx->_this()));
      }
  delete [] xspans;
  delete [] yspans;
}

void GridImpl::traverseWithoutAllocation(Traversal_ptr t, const Grid::Range &range)
{
  GridDimension &d = dimensions[yaxis];
  Grid::Index i;
  for (i.row = range.lower.row; i.row != range.upper.row; i.row++)
    for (i.col = range.lower.col; i.col != range.upper.col; i.col++)
      t->traverseChild(d.children[i.row][i.col], index2tag(i), Region_var(Region::_nil()), Transform_var(Transform::_nil()));
}

SubGridImpl::SubGridImpl(Grid_ptr grid, const Grid::Range &r)
 : child(grid), range(r) {}

SubGridImpl::~SubGridImpl() {}
void SubGridImpl::request(Requisition &r) { child->requestRange(r, range);}

void SubGridImpl::traverse(Traversal_ptr t)
{
  t->traverseChild(child, 0, Region_var(Region::_nil()), Transform_var(Transform::_nil()));
}

