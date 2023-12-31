/*$Id: PolyGraphic.hh,v 1.14 1999/09/30 17:23:33 gray Exp $
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
#ifndef _PolyGraphic_hh
#define _PolyGraphic_hh

#include <Berlin/GraphicImpl.hh>
#include <Berlin/Pool.hh>
#include <vector>

class PolyGraphic : public GraphicImpl
{
  typedef vector<edge_t> clist_t;
public:
  PolyGraphic();
  virtual ~PolyGraphic();

  virtual void append(Graphic_ptr);
  virtual void prepend(Graphic_ptr);

  virtual void needResize();
  virtual void needResize(Tag);
protected:
  CORBA::Long numChildren();
  Tag tag();
  CORBA::Long index(Tag); 
  Graphic::Requisition *childrenRequests();
  void deallocateRequisitions(Graphic::Requisition *);
  void childExtension(size_t, const Allocation::Info &, Region_ptr);
// private:
  static Pool<Requisition> pool;
  clist_t children;
  Prague::Mutex childMutex;
};

#endif /* _PolyGraphic_hh */
