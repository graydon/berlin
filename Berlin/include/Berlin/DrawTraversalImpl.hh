/*$Id: DrawTraversalImpl.hh,v 1.10 1999/10/13 21:32:31 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _DrawTraversal_hh
#define _DrawTraversal_hh

#include <Warsaw/config.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Berlin/TraversalImpl.hh>
#include <vector>

declare_corba_ptr_type(DrawingKit)
declare_corba_ptr_type(Drawable)
declare_corba_ptr_type(Region)

class DrawTraversalImpl : implements(DrawTraversal), virtual public TraversalImpl
{
public:
  DrawTraversalImpl(Graphic_ptr, Region_ptr, Transform_ptr, DrawingKit_ptr);
  DrawTraversalImpl(const DrawTraversalImpl &);
  virtual ~DrawTraversalImpl();
  virtual CORBA::Boolean intersectsAllocation();
  virtual CORBA::Boolean intersectsRegion(Region_ptr);
  virtual void traverseChild(Graphic_ptr, Tag, Region_ptr, Transform_ptr);
  virtual void visit(Graphic_ptr);
  virtual order direction() { return up;}
  virtual CORBA::Boolean ok() { return true;}
  virtual DrawingKit_ptr kit();
private:
  DrawingKit_var drawingkit;
  Drawable_var drawable;
  Region_var clipping;
};

#endif /* _DrawTraversalImpl_hh */
