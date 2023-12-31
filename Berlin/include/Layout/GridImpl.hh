/*$Id: GridImpl.hh,v 1.13 2001/04/18 06:07:26 stefan Exp $
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
#ifndef _GridImpl_hh
#define _GridImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Grid.hh>
#include <Berlin/GraphicImpl.hh>
#include <vector>

class GridImpl;

class GridImpl : public virtual POA_Layout::Grid,
		 public GraphicImpl
{
  struct Dimension
  {
    void init(long count, long n)
    {
      children.resize(count);
      for (std::vector<std::vector<Warsaw::Graphic_var> >::iterator i = children.begin(); i != children.end(); ++i)
	(*i).resize(n);
      requirements.resize(count);
    }
    CORBA::Long size() { return children.size();}
    std::vector<std::vector<Warsaw::Graphic_var> > children;
    std::vector<Warsaw::Graphic::Requirement> requirements;
  };
public:
  struct Span
  {
    Warsaw::Coord lower;
    Warsaw::Coord upper;
    Warsaw::Alignment align;
  };
  GridImpl(const Layout::Grid::Index &upper);
  ~GridImpl();

  virtual void append_graphic(Warsaw::Graphic_ptr);
  virtual void prepend_graphic(Warsaw::Graphic_ptr);

  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void need_resize();
  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);

  virtual void replace(Warsaw::Graphic_ptr, const Layout::Grid::Index &);
  virtual Layout::Grid::Index find(Warsaw::Traversal_ptr);
  virtual void allocate_cell(Warsaw::Region_ptr, const Layout::Grid::Index &, Warsaw::Region_ptr);
  virtual void request_range(Warsaw::Graphic::Requisition &, const Layout::Grid::Range &);
  virtual void traverse_range(Warsaw::Traversal_ptr, const Layout::Grid::Range &);
  virtual Layout::Grid::Index find_range(Warsaw::Traversal_ptr, const Layout::Grid::Range &);
  virtual void range_position(Warsaw::Region_ptr, const Layout::Grid::Range &, Warsaw::Vertex &);
  virtual Layout::Grid::Index upper();

 private:
  Warsaw::Tag index_to_tag(const Layout::Grid::Index &index) { return (index.col << 16) + index.row;}
  Layout::Grid::Index tag_to_index(Warsaw::Tag tag)
    {
      Layout::Grid::Index index;
      index.col = tag >> 16;
      index.row = tag & 0xffff;
      return index;
    }
  void cache_request();
  void partial_request(Warsaw::Axis axis, long lower, long, Warsaw::Graphic::Requirement &);
  void full_request(Warsaw::Axis, Warsaw::Axis);
  Span *full_allocate(Warsaw::Axis, Warsaw::Region_ptr);
  void traverse_with_allocation(Warsaw::Traversal_ptr, Warsaw::Region_ptr, const Layout::Grid::Range &);
  void traverse_without_allocation(Warsaw::Traversal_ptr, const Layout::Grid::Range &);

  Dimension _dimensions[2];
  Layout::Grid::Index _cursor;

  bool _requested;
  Warsaw::Graphic::Requisition _requisition;
};

class SubGridImpl : public GraphicImpl
{
public:
  SubGridImpl(Layout::Grid_ptr, const Layout::Grid::Range &);
  ~SubGridImpl();

  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void traverse(Warsaw::Traversal_ptr);
private:
  Layout::Grid_var _child;
  Layout::Grid::Range _range;
};

#endif
