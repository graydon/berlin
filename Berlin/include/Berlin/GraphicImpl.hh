/*$Id: GraphicImpl.hh,v 1.28 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _GraphicImpl_hh
#define _GraphicImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Prague/Sys/Thread.hh>
#include <vector>
#include <algorithm>
#include <functional>
#include "Berlin/RefCountBaseImpl.hh"
#include "Berlin/IdentifiableImpl.hh"

class RegionImpl;
class AllocationImpl;

class GraphicImpl : public virtual POA_Warsaw::Graphic,
                    public virtual RefCountBaseImpl,
                    public virtual IdentifiableImpl
{
 protected:
  //. An edge is a reference to a graphic within a composite.
  //. Because a graphic might appear twice within the same composite,
  //. a graphic itself is insufficient to identify its position within
  //. its parent.
  struct Edge
  {
    Warsaw::Graphic_var peer;
    Warsaw::Tag         peerId;
    Warsaw::Tag         localId;
  };
  typedef std::vector<Edge> glist_t;
  struct localId_eq : public std::unary_function<Edge, bool>
    {
      localId_eq(Warsaw::Tag t) : id(t) {}
      bool operator()(const Edge &e) const { return e.localId == id; }
      Warsaw::Tag id;
    };
 public:
  static const Warsaw::Coord infinity = 10e6;
  GraphicImpl();
  virtual ~GraphicImpl();
  virtual void deactivate();

  virtual Warsaw::Graphic_ptr body();
  virtual void body(Warsaw::Graphic_ptr);
  virtual void append_graphic(Warsaw::Graphic_ptr);
  virtual void prepend_graphic(Warsaw::Graphic_ptr);
  virtual void remove_graphic(Warsaw::Tag);
  virtual void remove_child_graphic(Warsaw::Tag);
  virtual Warsaw::Tag add_parent_graphic(Warsaw::Graphic_ptr, Warsaw::Tag);
  virtual void remove_parent_graphic(Warsaw::Tag);
  virtual Warsaw::Graphic::Iterator_ptr first_child_graphic();
  virtual Warsaw::Graphic::Iterator_ptr last_child_graphic();

  virtual Warsaw::Transform_ptr transformation();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void shape(Warsaw::Region_ptr);

  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);

  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);
  virtual void allocations(Warsaw::Allocation_ptr);
  virtual void need_redraw();
  virtual void need_redraw_region(Warsaw::Region_ptr);
  virtual void need_resize();

  static void init_requisition(Warsaw::Graphic::Requisition &);
  static void default_requisition(Warsaw::Graphic::Requisition &);
  static void require(Warsaw::Graphic::Requirement &, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  static void require_lead_trail(Warsaw::Graphic::Requirement &,
				 Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  static Warsaw::Graphic::Requirement *requirement(Warsaw::Graphic::Requisition &, Warsaw::Axis);
  static void default_extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  static void natural_allocation(Warsaw::Graphic_ptr, RegionImpl &);
  static void transform_request(Warsaw::Graphic::Requisition &, Warsaw::Transform_ptr);
  static Warsaw::Vertex transform_allocate(RegionImpl &, const Warsaw::Graphic::Requisition &, Warsaw::Transform_ptr);
private:
  Warsaw::Tag unique_parent_id();
  glist_t              _parents;
  Prague::Mutex        _mutex;
};

class GraphicIteratorImpl : public virtual POA_Warsaw::GraphicIterator,
		            public virtual ServantBase
{
public:
  virtual void destroy() { deactivate();}
};

#endif
