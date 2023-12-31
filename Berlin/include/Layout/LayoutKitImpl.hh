/*$Id: LayoutKitImpl.hh,v 1.12 1999/10/21 20:23:51 gray Exp $
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
#ifndef _LayoutKitImpl_hh
#define _LayoutKitImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/LayoutKit.hh"
#include "Berlin/CloneableImpl.hh"
#include <vector>

class GraphicImpl;

class LayoutKitImpl : lcimplements(LayoutKit), virtual public CloneableImpl
{
public:
  LayoutKitImpl();
  ~LayoutKitImpl();
  virtual Coord fil();
  virtual void fil(Coord);
  virtual Graphic_ptr clipper(Graphic_ptr);
//   virtual Graphic_ptr create_backdrop();
//   virtual AutoScroll_ptr create_auto_scroll(Adjustment_ptr x_adjustment, Adjustment_ptr y_adjustment);
//   virtual FullyVisibleConstraint_ptr create_fully_visible_constraint(Float usable, Float align);
//   virtual Scrollable* scroll_box(Axis a);
  virtual Viewport_ptr scrollable(Graphic_ptr);
  virtual Stage_ptr createStage();
  virtual Grid_ptr fixedGrid(const Grid::Index &);
  virtual Graphic_ptr fixedRange(Grid_ptr g, const Grid::Range &);
  virtual Graphic_ptr hbox();
  virtual Graphic_ptr vbox();
  virtual Graphic_ptr hboxFirstAligned();
  virtual Graphic_ptr vboxFirstAligned();
  virtual Graphic_ptr hboxAlignElements(Alignment);
  virtual Graphic_ptr vboxAlignElements(Alignment);
  virtual Graphic_ptr overlay();
  virtual Graphic_ptr deck();
  virtual Graphic_ptr back(Graphic_ptr, Graphic_ptr);
  virtual Graphic_ptr front(Graphic_ptr, Graphic_ptr);
  virtual Graphic_ptr between(Graphic_ptr, Graphic_ptr, Graphic_ptr);
  virtual Graphic_ptr glue(Axis, Coord, Coord, Coord, Alignment);
  virtual Graphic_ptr glueRequisition(const Graphic::Requisition &);
  virtual Graphic_ptr hfil();
  virtual Graphic_ptr hglueFil(Coord);
  virtual Graphic_ptr hglue(Coord, Coord, Coord);
  virtual Graphic_ptr hglueAligned(Coord, Coord, Coord, Alignment);
  virtual Graphic_ptr hspace(Coord);
  virtual Graphic_ptr vfil();
  virtual Graphic_ptr vglueFil(Coord);
  virtual Graphic_ptr vglue(Coord, Coord, Coord);
  virtual Graphic_ptr vglueAligned(Coord, Coord, Coord, Alignment);
  virtual Graphic_ptr vspace(Coord);
  virtual Graphic_ptr shapeOf(Graphic_ptr g);
  virtual Graphic_ptr shapeOfXY(Graphic_ptr, Graphic_ptr);
  virtual Graphic_ptr shapeOfXYZ(Graphic_ptr, Graphic_ptr, Graphic_ptr);
//   virtual Graphic_ptr strut(Font_ptr f, Coord natural, Coord stretch, Coord shrink);
//   virtual Graphic_ptr hstrut(Coord right_bearing, Coord left_bearing, Coord natural, Coord stretch, Coord shrink);
//   virtual Graphic_ptr vstrut(Coord ascent, Coord descent, Coord natural, Coord stretch, Coord shrink);
//   virtual Graphic_ptr spaces(Long count, Coord each, Font_ptr f, Color_ptr c);
  virtual Graphic_ptr align(Graphic_ptr, Alignment, Alignment);
  virtual Graphic_ptr alignAxis(Graphic_ptr, Axis, Alignment);
  virtual Graphic_ptr halign(Graphic_ptr, Alignment);
  virtual Graphic_ptr valign(Graphic_ptr, Alignment);
  virtual Graphic_ptr fixed(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr fixedAxis(Graphic_ptr, Axis, Coord);
  virtual Graphic_ptr hfixed(Graphic_ptr, Coord);
  virtual Graphic_ptr vfixed(Graphic_ptr, Coord);
  virtual Graphic_ptr flexible(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr flexibleFil(Graphic_ptr);
  virtual Graphic_ptr flexibleAxis(Graphic_ptr, Axis, Coord, Coord);
  virtual Graphic_ptr hflexible(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr vflexible(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr natural(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr naturalAxis(Graphic_ptr, Axis, Coord);
  virtual Graphic_ptr hnatural(Graphic_ptr, Coord);
  virtual Graphic_ptr vnatural(Graphic_ptr, Coord);
  virtual Graphic_ptr margin(Graphic_ptr, Coord);
  virtual Graphic_ptr marginLRBT(Graphic_ptr, Coord, Coord, Coord, Coord);
  virtual Graphic_ptr marginFlexible(Graphic_ptr, Coord, Coord, Coord);
  virtual Graphic_ptr marginLRBTFlexible(Graphic_ptr, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord);
  virtual Graphic_ptr hmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr hmarginLR(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr hmarginLRFlexible(Graphic_ptr, Coord, Coord, Coord, Coord, Coord, Coord);
  virtual Graphic_ptr vmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr vmarginBT(Graphic_ptr, Coord, Coord);
  virtual Graphic_ptr vmarginBTFlexible(Graphic_ptr, Coord, Coord, Coord, Coord, Coord, Coord);
  virtual Graphic_ptr lmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr lmarginFlexible(Graphic_ptr, Coord, Coord, Coord);
  virtual Graphic_ptr rmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr rmarginFlexible(Graphic_ptr, Coord, Coord, Coord);
  virtual Graphic_ptr bmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr bmarginFlexible(Graphic_ptr, Coord, Coord, Coord);
  virtual Graphic_ptr tmargin(Graphic_ptr, Coord);
  virtual Graphic_ptr tmarginFlexible(Graphic_ptr, Coord, Coord, Coord);
protected:
  Coord fil_;
 public:
  vector<GraphicImpl *> graphics;
};

#endif /* _LayoutKitImpl_hh */
