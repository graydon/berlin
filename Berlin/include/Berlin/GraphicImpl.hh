/*$Id: GraphicImpl.hh,v 1.13 1999/09/30 17:23:33 gray Exp $
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
#ifndef _GraphicImpl_hh
#define _GraphicImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Graphic.hh"
#include "Berlin/CloneableImpl.hh"
#include "Prague/Sys/Thread.hh"
#include <vector>
#include <algorithm>

declare_corba_ptr_type(Region)
class RegionImpl;

class GraphicImpl : implements(Graphic)
{
 protected:
  typedef pair<Graphic_var, Tag> edge_t;
  typedef vector<edge_t> plist_t;
 public:
  static const Coord infinity = 10e6;
  GraphicImpl();
  virtual ~GraphicImpl();

  virtual Graphic_ptr body();
  virtual void body(Graphic_ptr);
  virtual void append(Graphic_ptr);
  virtual void prepend(Graphic_ptr);
  virtual void addParent(Graphic_ptr, Tag);
  virtual void removeParent(Graphic_ptr, Tag);

  virtual Transform_ptr transformation();
  virtual void request(Requisition &);
  virtual void extension(const Allocation::Info &, Region_ptr);
  virtual void shape(Region_ptr);

  virtual void traverse(Traversal_ptr);
  virtual void draw(DrawTraversal_ptr);
  virtual void pick(PickTraversal_ptr);

  virtual void allocate(Tag, const Allocation::Info &);
  virtual void allocations(Allocation_ptr);
  virtual void needRedraw();
  virtual void needRedrawRegion(Region_ptr);
  virtual void needResize();

  static void initRequisition(Graphic::Requisition &);
  static void defaultRequisition(Graphic::Requisition &);
  static void require(Graphic::Requirement &, Coord, Coord, Coord, Coord);
  static void requireLeadTrail(Graphic::Requirement &, Coord, Coord, Coord, Coord, Coord, Coord);
  static Graphic::Requirement *requirement(Graphic::Requisition &, Axis);
  static void defaultExtension(const Allocation::Info &, Region_ptr);
  static void naturalAllocation(Graphic_ptr, RegionImpl &);
  static void transformRequest(Graphic::Requisition &, Transform_ptr);
  static Vertex transformAllocate(RegionImpl &, const Graphic::Requisition &, Transform_ptr);
protected:
  plist_t parents;
  Prague::Mutex parentMutex;
};

#endif /* _GraphicImpl_hh */
