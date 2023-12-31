/*$Id: Composition.hh,v 1.5 2000/09/19 21:11:05 stefan Exp $
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
#ifndef _Composition_hh
#define _Composition_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Warsaw/TextKit.hh>
#include <Berlin/PolyGraphic.hh>
#include <map>

class Compositor;

class Composition : public PolyGraphic
{
 public:
  Composition(Warsaw::DrawingKit_ptr, Compositor *);
  virtual ~Composition();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void need_resize();
  virtual void need_resize(Warsaw::Tag);
  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);
 protected:
  RegionImpl **children_allocations(Warsaw::Region_ptr);
  Warsaw::DrawingKit_var canonicalDK;
  Compositor  *compositor;
  bool requested;
  Warsaw::Graphic::Requisition requisition;
};

#endif
