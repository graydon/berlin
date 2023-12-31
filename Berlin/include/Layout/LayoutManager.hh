/*$Id: LayoutManager.hh,v 1.5 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _LayoutManager_hh
#define _LayoutManager_hh

#include "Warsaw/config.hh"
#include "Warsaw/Graphic.hh"
#include "Warsaw/Region.hh"

class RegionImpl;

class LayoutManager
{
public:
  typedef RegionImpl **Allocations;

  LayoutManager();
  virtual ~LayoutManager();
  virtual LayoutManager *clone() = 0;

  virtual void request(long n, Graphic::Requisition *requests, Graphic::Requisition &result) = 0;
  virtual void allocate(long n, Graphic::Requisition *requests, Region_ptr given, LayoutManager::Allocations result) = 0;

  static void setSpan(RegionImpl *r, Axis a, Coord origin, Coord length, Alignment align);
};

class LayoutAlign : public LayoutManager
//. LayoutAlign -- align positions along an axis
{
public:
  LayoutAlign(Axis, bool = false);
  virtual ~LayoutAlign();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  bool relaxed;
};

class LayoutCenter : public LayoutManager
//. LayoutCenter -- center positions along an axis
{
public:
  LayoutCenter(Axis, Alignment a);
  virtual ~LayoutCenter();

  virtual LayoutManager* clone();
  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Alignment alignment;
};

class LayoutFixed : public LayoutManager
//. LayoutFixed -- set size along an axis
{
public:
  LayoutFixed(Axis, Coord size);
  virtual ~LayoutFixed();

  virtual LayoutManager* clone();
  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Coord size;
};

class LayoutVariable : public LayoutManager
//. LayoutVariable -- allow flexibility along an axis
{
public:
  LayoutVariable(Axis, Coord stretch, Coord shrink);
  virtual ~LayoutVariable();

  virtual LayoutManager* clone();
  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Coord stretch;
  Coord shrink;
};

class LayoutNatural : public LayoutManager
//. LayoutNatural -- set the natural size along an axis
{
public:
  LayoutNatural(Axis, Coord natural);
  virtual ~LayoutNatural();

  virtual LayoutManager* clone();
  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Coord natural;
};

class LayoutMargin : public LayoutManager
//. LayoutMargin -- leave a margin around the sides
{
public:
  LayoutMargin(Coord);
  LayoutMargin(Coord, Coord);
  LayoutMargin(Coord, Coord, Coord, Coord);
  LayoutMargin(Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord);
  virtual ~LayoutMargin();

  virtual LayoutManager* clone();
  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Coord lnatural, lstretch, lshrink;
  Coord rnatural, rstretch, rshrink;
  Coord bnatural, bstretch, bshrink;
  Coord tnatural, tstretch, tshrink;
  Graphic::Requisition requisition;

  void allocateAxis(Axis, Coord, Coord, Coord, Coord, Coord, Coord, LayoutManager::Allocations);
  static Coord span(Coord, Graphic::Requirement &, Coord, Coord, Coord);
};

class LayoutSuperpose : public LayoutManager
//. LayoutSuperpose - composite layout manager
{
public:
  LayoutSuperpose(LayoutManager *, LayoutManager *);
  LayoutSuperpose(LayoutManager *, LayoutManager *, LayoutManager *);
  virtual ~LayoutSuperpose();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  LayoutManager *first;
  LayoutManager *second;
  LayoutManager *third;
};

class LayoutTile : public LayoutManager
//. LayoutTile -- side-by-side, first-to-last along an axis
{
public:
  LayoutTile(Axis);
  virtual ~LayoutTile();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);

  static void computeRequest(Axis, Alignment, long, Graphic::Requisition *, Graphic::Requisition &);
  static void computeAllocations(Axis, Graphic::Requisition &, bool, long, Graphic::Requisition *,
				 Region_ptr, LayoutManager::Allocations);
  static Coord computeLength(const Graphic::Requirement &, const Region::Allotment &);
  static float computeSqueeze(const Graphic::Requirement &, Coord);
private:
  Axis axis;
  Graphic::Requisition requisition;
};

class LayoutTileReversed : public LayoutManager
//. LayoutTileReversed -- side-by-side, last-to-first
{
public:
  LayoutTileReversed(Axis);
  virtual ~LayoutTileReversed();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
  static void computeReversedAllocations(Axis, Graphic::Requisition &, bool, long, Graphic::Requisition *,
					 Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Graphic::Requisition requisition;
};


class LayoutTileFirstAligned : public LayoutManager
//. LayoutTileFirstAligned -- like Tile but use first element's origin
{
public:
  LayoutTileFirstAligned(Axis);
  virtual ~LayoutTileFirstAligned();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
  static void computeRequestFirstAligned(Axis, long, Graphic::Requisition *, Graphic::Requisition &);
private:
  Axis axis;
  Graphic::Requisition requisition;
};

class LayoutTileReversedFirstAligned : public LayoutManager
//. LayoutTileReversedFirstAligned -- like TileReversed
//. but use first element's origin
{
public:
  LayoutTileReversedFirstAligned(Axis);
  virtual ~LayoutTileReversedFirstAligned();
  virtual LayoutManager *clone();

  virtual void request(long, Graphic::Requisition *, Graphic::Requisition &);
  virtual void allocate(long, Graphic::Requisition *, Region_ptr, LayoutManager::Allocations);
private:
  Axis axis;
  Graphic::Requisition requisition;
};

#endif /* _LayoutManager_hh */
