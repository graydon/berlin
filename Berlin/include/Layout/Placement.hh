/*$Id: Placement.hh,v 1.5 1999/10/07 15:11:10 gray Exp $
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
#ifndef _Placement_hh
#define _Placement_hh

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

  virtual void request(Requisition &);

  virtual void traverse(Traversal_ptr);

  virtual void allocate(Tag, const Allocation::Info &);

private:
  LayoutManager *layout;
  RegionImpl *region;
};

class LayoutLayer : public MonoGraphic
{
public:
  LayoutLayer(Graphic_ptr, Graphic_ptr, Graphic_ptr);
  virtual ~LayoutLayer();
  virtual void traverse(Traversal_ptr);
private:
  Graphic_var under;
  Graphic_var over;
};

#endif /* _Placement_hh */
