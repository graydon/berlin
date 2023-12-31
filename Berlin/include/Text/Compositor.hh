/*$Id: Compositor.hh,v 1.8 2000/09/19 21:11:05 stefan Exp $
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
#ifndef _Compositor_hh
#define _Compositor_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Region.hh>

class RegionImpl;

class Compositor
//. this is a strategy object for adjusting text layouts to compensate for font
//. misses or hinting. It plays a very similar role to a LayoutManager.
{
public:
  typedef RegionImpl **Allocations;
  virtual ~Compositor() {}
  virtual void request(long n, Warsaw::Graphic::Requisition *requests, Warsaw::DrawingKit_ptr dk, Warsaw::Graphic::Requisition &result) = 0;
  virtual void allocate(long n, Warsaw::Graphic::Requisition *requests, Warsaw::DrawingKit_ptr dk, Warsaw::Region_ptr given, Allocations result) = 0;
  static void set_span(RegionImpl *r, Warsaw::Axis a, Warsaw::Coord origin, Warsaw::Coord length, Warsaw::Alignment align);
  static Warsaw::Coord compute_length(const Warsaw::Graphic::Requirement &, const Warsaw::Region::Allotment &);
  static Warsaw::Coord compute_squeeze(const Warsaw::Graphic::Requirement &, Warsaw::Coord);
};

class LRCompositor : public Compositor
//. left to right compositor -- aligns vertically, tiles left to right
//. for now it ignores the DrawingKit parameter and does no compensation
//. no line breaking
{
public:
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::DrawingKit_ptr, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::DrawingKit_ptr, Warsaw::Region_ptr, Allocations);    
private:
  Warsaw::Graphic::Requisition requisition;
};

class TBCompositor : public Compositor
//. top to button compositor -- aligns horicontally, tiles top to bottom
//. for now it ignores the DrawingKit parameter and does no compensation
//. no line breaking
{
public:
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::DrawingKit_ptr, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::DrawingKit_ptr, Warsaw::Region_ptr, Allocations);    
private:
  Warsaw::Graphic::Requisition requisition;
};

#endif
