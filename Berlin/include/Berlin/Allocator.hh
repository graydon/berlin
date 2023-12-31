/*$Id: Allocator.hh,v 1.7 1999/10/13 21:32:31 gray Exp $
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
#ifndef _Allocator_hh
#define _Allocator_hh

#include <Berlin/MonoGraphic.hh>
#include <Berlin/ImplVar.hh>

class Allocator : public MonoGraphic
{
public:
  Allocator();
  virtual ~Allocator();

  virtual void request(Requisition &);

  virtual void traverse(Traversal_ptr);

  virtual void allocate(Tag, const Allocation::Info &);
  virtual void needResize();
protected:
  bool requested;
  Graphic::Requisition requisition;
  Impl_var<RegionImpl> natural;
  Impl_var<RegionImpl> extension;
  
  void updateRequisition();
  void needDamage(RegionImpl *, Allocation_ptr);
};

class TransformAllocator : public Allocator
{
public:
  TransformAllocator(Alignment, Alignment, Alignment, Alignment, Alignment, Alignment);
  ~TransformAllocator();

  virtual void request(Requisition &);
  virtual void traverse(Traversal_ptr);
  virtual void allocate(Tag, const Allocation::Info &);
protected:
  Alignment xparent, yparent, zparent;
  Alignment xchild, ychild, zchild;

  void computeDelta(const Vertex &, const Vertex &, Vertex &);
};

#endif /* _Allocator_hh */
