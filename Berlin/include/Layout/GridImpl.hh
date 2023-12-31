/*$Id: GridImpl.hh,v 1.6 1999/09/13 21:22:06 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on code from Fresco.
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
#ifndef _GridImpl_hh
#define _GridImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Grid.hh>
#include <Berlin/GraphicImpl.hh>
#include <Berlin/CloneableImpl.hh>
#include <vector>

class GridImpl;

struct GridDimension
{
  void init(long count, long n)
    {
      children.resize(count);
      for (vector<vector<Graphic_var> >::iterator i = children.begin(); i != children.end(); i++)
	(*i).resize(n);
      requirements.resize(count);
    }
  CORBA::Long size() { return children.size();}
  vector<vector<Graphic_var> > children;
  vector<Graphic::Requirement> requirements;
};

class GridImpl : implements(Grid), public GraphicImpl
{
  struct Span
  {
    Coord lower;
    Coord upper;
    Alignment align;
  };
public:
  GridImpl(const Index &upper);
  ~GridImpl();

  virtual void append(Graphic_ptr);
  virtual void prepend(Graphic_ptr);

  virtual void request(Requisition &);
  virtual void traverse(Traversal_ptr);
  virtual void needResize();
  virtual void allocate(Tag, const Allocation::Info &);

  virtual void replace(Graphic_ptr, const Grid::Index &i);
  virtual Grid::Index find(Traversal_ptr);
  virtual void allocateCell(Region_ptr, const Grid::Index &, Region_ptr);
  virtual void requestRange(Graphic::Requisition &, const Grid::Range &);
  virtual void traverseRange(Traversal_ptr, const Grid::Range &);
  virtual Grid::Index findRange(Traversal_ptr, const Grid::Range &);
  virtual void rangePosition(Region_ptr, const Grid::Range &, Vertex &);
  virtual Grid::Index upper();

 private:
  Tag  index2tag(Grid::Index index)
    {
      return index.col << 16 + index.row;
    }
  Grid::Index tag2index(Tag tag)
    {
      Grid::Index index;
      index.col = tag >> 16;
      index.row = tag & 0xffff;
      return index;
    }
  void cacheRequest();
  void partialRequest(Axis axis, long lower, long, Graphic::Requirement &);
  void fullRequest(Axis, Axis);
  Span *fullAllocate(Axis, Region_ptr);
  void traverseWithAllocation(Traversal_ptr, Region_ptr, const Grid::Range &);
  void traverseWithoutAllocation(Traversal_ptr, const Grid::Range &);

  GridDimension dimensions[2];

  Grid::Index cursor;

  bool requested;
  Requisition requisition;
};

class SubGridImpl : public GraphicImpl
{
public:
  SubGridImpl(Grid_ptr, const Grid::Range &);
  ~SubGridImpl();

  virtual void request(Requisition &);
  virtual void traverse(Traversal_ptr);
private:
  Grid_var child;
  Grid::Range range;
};

#endif /* _GridImpl_hh */
