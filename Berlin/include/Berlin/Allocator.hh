/*$Id: Allocator.hh,v 1.13 2000/09/19 21:11:02 stefan Exp $
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
#ifndef _Allocator_hh
#define _Allocator_hh

#include <Berlin/ImplVar.hh>
#include <Berlin/MonoGraphic.hh>

//. An Allocator is a graphic that always gives its child
//. an allocation that matches the child's requisition.
//. This functionality is useful as a gateway between
//. figure objects, which ignore their allocation, and
//. layout objects.
class Allocator : public MonoGraphic
{
public:
  Allocator();
  virtual ~Allocator();

  virtual void request(Warsaw::Graphic::Requisition &);

  virtual void traverse(Warsaw::Traversal_ptr);

  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);
  virtual void need_resize();
// private:
  bool _requested : 1;
  bool _allocated : 1;
  Warsaw::Graphic::Requisition _requisition;
  Impl_var<RegionImpl> _natural;
  Impl_var<RegionImpl> _extension;
  
  void cache_requisition();
  void cache_allocation();
  void need_damage(RegionImpl *, Warsaw::Allocation_ptr);
  static void natural_allocation(const Warsaw::Graphic::Requisition &, RegionImpl &);
};

class TransformAllocator : public Allocator
//. A TransformAllocator maps its allocate to a translation
//. during traversal and always gives its child the child's
//. natural allocation.  This functionality is useful
//. as a gateway between layout objects and figure objects
//. (which ignore their allocation).
{
public:
  TransformAllocator(Warsaw::Alignment, Warsaw::Alignment, Warsaw::Alignment, Warsaw::Alignment, Warsaw::Alignment, Warsaw::Alignment);
  ~TransformAllocator();

  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);
protected:
  Warsaw::Alignment _xparent, _yparent, _zparent;
  Warsaw::Alignment _xchild, _ychild, _zchild;

  void compute_delta(const Warsaw::Vertex &, const Warsaw::Vertex &, Warsaw::Vertex &);
};

#endif 
