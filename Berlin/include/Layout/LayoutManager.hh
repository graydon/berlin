/*$Id: LayoutManager.hh,v 1.11 2000/11/11 14:23:04 velco Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _LayoutManager_hh
#define _LayoutManager_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Region.hh>

class RegionImpl;

class LayoutManager
{
public:
  typedef RegionImpl **Allocations;

  LayoutManager();
  virtual ~LayoutManager();
  virtual LayoutManager *clone() = 0;

  virtual void request(long n, Warsaw::Graphic::Requisition *requests, Warsaw::Graphic::Requisition &result) = 0;
  virtual void allocate(long n, Warsaw::Graphic::Requisition *requests, Warsaw::Region_ptr given, LayoutManager::Allocations result) = 0;

  static void set_span(RegionImpl *r, Warsaw::Axis a, Warsaw::Coord origin, Warsaw::Coord length, Warsaw::Alignment align);
};

class LayoutAlign : public LayoutManager
//. LayoutAlign -- align positions along an axis
{
public:
  LayoutAlign(Warsaw::Axis);
  virtual ~LayoutAlign();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
};

class LayoutCenter : public LayoutManager
//. LayoutCenter -- center positions along an axis
{
public:
  LayoutCenter(Warsaw::Axis, Warsaw::Alignment a);
  virtual ~LayoutCenter();

  virtual LayoutManager* clone();
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Alignment alignment;
};

class LayoutFixed : public LayoutManager
//. LayoutFixed -- set size along an axis
{
public:
  LayoutFixed(Warsaw::Axis, Warsaw::Coord);
  virtual ~LayoutFixed();

  virtual LayoutManager* clone();
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Coord size;
};

class LayoutVariable : public LayoutManager
//. LayoutVariable -- allow flexibility along an axis
{
public:
  LayoutVariable(Warsaw::Axis, Warsaw::Coord stretch, Warsaw::Coord shrink);
  virtual ~LayoutVariable();

  virtual LayoutManager* clone();
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Coord stretch;
  Warsaw::Coord shrink;
};

class LayoutNatural : public LayoutManager
//. LayoutNatural -- set the natural size along an axis
{
public:
  LayoutNatural(Warsaw::Axis, Warsaw::Coord);
  virtual ~LayoutNatural();

  virtual LayoutManager* clone();
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Coord natural;
};

class LayoutMargin : public LayoutManager
//. LayoutMargin -- leave a margin around the sides
{
public:
  LayoutMargin(Warsaw::Coord);
  LayoutMargin(Warsaw::Coord, Warsaw::Coord);
  LayoutMargin(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  LayoutMargin(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
	       Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
	       Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual ~LayoutMargin();

  virtual LayoutManager* clone();
  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Coord lnatural, lstretch, lshrink;
  Warsaw::Coord rnatural, rstretch, rshrink;
  Warsaw::Coord bnatural, bstretch, bshrink;
  Warsaw::Coord tnatural, tstretch, tshrink;
  Warsaw::Graphic::Requisition requisition;

  void allocate_axis(Warsaw::Axis, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
		     Warsaw::Coord, Warsaw::Coord, LayoutManager::Allocations);
  static Warsaw::Coord span(Warsaw::Coord, Warsaw::Graphic::Requirement &, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
};

class LayoutSuperpose : public LayoutManager
//. LayoutSuperpose - composite layout manager
{
public:
  LayoutSuperpose(LayoutManager *, LayoutManager *);
  LayoutSuperpose(LayoutManager *, LayoutManager *, LayoutManager *);
  virtual ~LayoutSuperpose();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  LayoutManager *first;
  LayoutManager *second;
  LayoutManager *third;
};

class LayoutTile : public LayoutManager
//. LayoutTile -- side-by-side, first-to-last along an axis
{
public:
  LayoutTile(Warsaw::Axis);
  virtual ~LayoutTile();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);

  static void compute_request(Warsaw::Axis, Warsaw::Alignment, long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  static void compute_allocations(Warsaw::Axis, Warsaw::Graphic::Requisition &, bool, long, Warsaw::Graphic::Requisition *,
				  Warsaw::Region_ptr, LayoutManager::Allocations);
  static Warsaw::Coord compute_length(const Warsaw::Graphic::Requirement &, const Warsaw::Region::Allotment &);
  static Warsaw::Coord compute_squeeze(const Warsaw::Graphic::Requirement &, Warsaw::Coord);
private:
  Warsaw::Axis axis;
  Warsaw::Graphic::Requisition requisition;
};

class LayoutTileReversed : public LayoutManager
//. LayoutTileReversed -- side-by-side, last-to-first
{
public:
  LayoutTileReversed(Warsaw::Axis);
  virtual ~LayoutTileReversed();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
  static void compute_reversed_allocations(Warsaw::Axis, Warsaw::Graphic::Requisition &, bool, long, Warsaw::Graphic::Requisition *,
					   Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Graphic::Requisition requisition;
};


class LayoutTileFirstAligned : public LayoutManager
//. LayoutTileFirstAligned -- like Tile but use first element's origin
{
public:
  LayoutTileFirstAligned(Warsaw::Axis);
  virtual ~LayoutTileFirstAligned();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
  static void compute_request_first_aligned(Warsaw::Axis, long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
private:
  Warsaw::Axis axis;
  Warsaw::Graphic::Requisition requisition;
};

class LayoutTileReversedFirstAligned : public LayoutManager
//. LayoutTileReversedFirstAligned -- like TileReversed
//. but use first element's origin
{
public:
  LayoutTileReversedFirstAligned(Warsaw::Axis);
  virtual ~LayoutTileReversedFirstAligned();
  virtual LayoutManager *clone();

  virtual void request(long, Warsaw::Graphic::Requisition *, Warsaw::Graphic::Requisition &);
  virtual void allocate(long, Warsaw::Graphic::Requisition *, Warsaw::Region_ptr, LayoutManager::Allocations);
private:
  Warsaw::Axis axis;
  Warsaw::Graphic::Requisition requisition;
};

#endif
