/*$Id: Box.hh,v 1.4 1999/09/13 21:22:06 gray Exp $
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
#ifndef _Box_hh
#define _Box_hh

#include <Berlin/PolyGraphic.hh>
#include <Layout/LayoutManager.hh>

class Box : public PolyGraphic
{
public:
  Box(LayoutManager *);
  virtual ~Box();

  virtual void request(Requisition &);
  virtual void extension(const Allocation::Info &, Region_ptr);

  virtual void traverse(Traversal_ptr);
  virtual void needResize();
  virtual void needResize(Tag);
  virtual void allocate(Tag, const Allocation::Info &);

protected:
  RegionImpl **childrenAllocations(Region_ptr);
  void traverseWithAllocation(Traversal_ptr, Region_ptr);
  void traverseWithoutAllocation(Traversal_ptr);
private:
  LayoutManager *layout;
  bool requested;
  Graphic::Requisition requisition;
};

class BoxAlignElements : public Box
{
public:
  BoxAlignElements(LayoutManager *, Axis, Alignment);
  virtual ~BoxAlignElements();

  virtual void append(Graphic_ptr);
  virtual void prepend(Graphic_ptr);
private:
  Axis axis;
  Alignment alignment;
};

class HBox : public Box
{
public:
  HBox() : Box(new LayoutSuperpose(new LayoutTile(xaxis), new LayoutAlign(yaxis))) {}
};

class VBox : public Box
{
public:
  VBox() : Box(new LayoutSuperpose(new LayoutTile(yaxis), new LayoutAlign(xaxis))) {}
};

class HBoxFirstAligned : public Box
{
public:
  HBoxFirstAligned() : Box(new LayoutSuperpose(new LayoutTileFirstAligned(xaxis), new LayoutAlign(yaxis))) {}
};

class VBoxFirstAligned : public Box
{
public:
  VBoxFirstAligned() : Box(new LayoutSuperpose(new LayoutTileReversedFirstAligned(yaxis), new LayoutAlign(xaxis))) {}
};

class HBoxAlignElements : public BoxAlignElements
{
public:
  HBoxAlignElements(double align)
    : BoxAlignElements(new LayoutSuperpose(new LayoutTile(xaxis), new LayoutAlign(yaxis)), yaxis, align) {}
};

class VBoxAlignElements : public BoxAlignElements
{
public:
  VBoxAlignElements(double align)
    : BoxAlignElements(new LayoutSuperpose(new LayoutTileReversed(yaxis), new LayoutAlign(xaxis)), xaxis, align) {}
};

class Overlay : public Box
{
public:
  Overlay() : Box(new LayoutSuperpose(new LayoutAlign(xaxis, true), new LayoutAlign(yaxis, true))) {}
};

#endif /* _Box_hh */
