/*$Id: Placement.hh,v 1.8 2000/08/31 18:52:32 stefan Exp $
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
#ifndef _Placement_hh
#define _Placement_hh

#include <Berlin/ImplVar.hh>
#include <Berlin/MonoGraphic.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/TransformImpl.hh>

class LayoutManager;
class RegionImpl;

class Placement : public MonoGraphic
{
public:
  Placement(LayoutManager *);
  virtual ~Placement();

  virtual void request(Warsaw::Graphic::Requisition &);

  virtual void traverse(Warsaw::Traversal_ptr);

  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);

private:
  LayoutManager *layout;
  Impl_var<RegionImpl> region;
};

class LayoutLayer : public MonoGraphic
{
public:
  LayoutLayer(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual ~LayoutLayer();
  virtual void traverse(Warsaw::Traversal_ptr);
private:
  Warsaw::Graphic_var under;
  Warsaw::Graphic_var over;
};

#endif
