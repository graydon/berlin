/*$Id: LayoutKitImpl.cc,v 1.14 1999/10/21 20:23:51 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefelds <seefelds@magellan.umontreal.ca>
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

// almost all of this code is ripped straight out of fresco, though it
// is slowly mutating to suit our purposes.. We acknowledge their
// copyrights and reproduce them here for your reading enjoyment.

//
// Copyright (c) 1987-91 Stanford University
// Copyright (c) 1991-94 Silicon Graphics, Inc.
// Copyright (c) 1993-94 Fujitsu, Ltd.
//
// Permission to use, copy, modify, distribute, and sell this software and 
// its documentation for any purpose is hereby granted without fee, provided
// that (i) the above copyright notices and this permission notice appear in
// all copies of the software and related documentation, and (ii) the names
// of Stanford, Silicon Graphics, and Fujitsu may not be used in any
// advertising or publicity relating to the software without the specific,
// prior written permission of Stanford, Silicon Graphics, and Fujitsu.
// 
// THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
// EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
// WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
//
// IN NO EVENT SHALL STANFORD, SILICON GRAPHICS, OR FUJITSU BE LIABLE FOR
// ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
// OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
// WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
// LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
// OF THIS SOFTWARE.
//

#include "Layout/LayoutKitImpl.hh"
#include "Layout/Box.hh"
#include "Layout/Deck.hh"
#include "Layout/GridImpl.hh"
#include "Layout/StageImpl.hh"
#include "Layout/Glue.hh"
#include "Layout/Placement.hh"
#include "Layout/ShapeOf.hh"
#include "Layout/ViewportImpl.hh"
#include "Berlin/Plugin.hh"

template <class I, class P>
P create(I *i, LayoutKitImpl *kit)
{
  i->_obj_is_ready(kit->_boa());
  kit->graphics.push_back(i);
  return i->_this();
}

LayoutKitImpl::LayoutKitImpl() { fil_ = GraphicImpl::infinity;}
LayoutKitImpl::~LayoutKitImpl()
{
  for (vector<GraphicImpl *>::iterator i = graphics.begin(); i != graphics.end(); i++) (*i)->_dispose();
}
void LayoutKitImpl::fil(Coord c) { fil_ = c;}
Coord LayoutKitImpl::fil() { return fil_;}
Graphic_ptr LayoutKitImpl::clipper(Graphic_ptr g)
{
  return 0;
//   return create<Clipper, Graphic_ptr> (new Clipper(g));
}

// Graphic_ptr LayoutKitImpl::create_backdrop() { return new Backdrop;}
// AutoScroll_ptr LayoutKitImpl::create_auto_scroll(Adjustment_ptr x_adjustment, Adjustment_ptr y_adjustment)
// {
//   return new AutoScrollImpl(x_adjustment, y_adjustment);
// }
// FullyVisibleConstraint_ptr LayoutKitImpl::create_fully_visible_constraint(Float usable, Float align)
// {
//   return new FullyVisibleConstraintImpl(usable, align);
// }
// Scrollable* LayoutKitImpl::scroll_box(Axis a)
// {
//   ScrollBox* b = nil;
//   switch (a)
//     {
//     case X_axis:
//       // unimplemented
//       break;
//     case Y_axis:
//       b = new TBScrollBox();
//       break;
//     case Z_axis:
//       // should raise an exception
//       break;
//     }
//   Scrollable* s = new Scrollable();
//   if (is_not_nil(b))
//     {
//       s->glyph_ptr = b;
//       s->adjustment_ptr = b->scroll_adjustment(Y_axis);
//     }
//   else fresco_fail("Layout kit can't create scroll box for given axis");
//   return s;
// }

Viewport_ptr LayoutKitImpl::scrollable(Graphic_ptr g)
{
  ViewportImpl *vp = new ViewportImpl;
  vp->_obj_is_ready(_boa());
  vp->attachAdjustments();
  vp->body(g);
  return vp->_this();
}

Stage_ptr LayoutKitImpl::createStage()
{
  return create<StageImpl, Stage_ptr>(new StageImpl, this);
}

Grid_ptr LayoutKitImpl::fixedGrid(const Grid::Index &upper)
{
  return create<GridImpl, Grid_ptr>(new GridImpl(upper), this);
}

Graphic_ptr LayoutKitImpl::fixedRange(Grid_ptr g, const Grid::Range &r)
{
  return create<SubGridImpl, Graphic_ptr>(new SubGridImpl(g, r), this);
}


Graphic_ptr LayoutKitImpl::hbox()
{
  return create<HBox, Graphic_ptr>(new HBox, this);
}

Graphic_ptr LayoutKitImpl::vbox()
{
  return create<VBox, Graphic_ptr>(new VBox, this);
}

Graphic_ptr LayoutKitImpl::hboxFirstAligned()
{
  return create<HBoxFirstAligned, Graphic_ptr>(new HBoxFirstAligned, this);
}

Graphic_ptr LayoutKitImpl::vboxFirstAligned()
{
  return create<VBoxFirstAligned, Graphic_ptr>(new VBoxFirstAligned, this);
}

Graphic_ptr LayoutKitImpl::hboxAlignElements(Alignment align)
{
  return create<HBoxAlignElements, Graphic_ptr>(new HBoxAlignElements(align), this);
}

Graphic_ptr LayoutKitImpl::vboxAlignElements(Alignment align)
{
  return create<VBoxAlignElements, Graphic_ptr>(new VBoxAlignElements(align), this);
}

Graphic_ptr LayoutKitImpl::overlay()
{
  return create<Overlay, Graphic_ptr>(new Overlay, this);
}

Graphic_ptr LayoutKitImpl::deck()
{
  return create<Deck, Graphic_ptr>(new Deck, this);
}

Graphic_ptr LayoutKitImpl::back(Graphic_ptr g, Graphic_ptr under)
{
  return create<LayoutLayer, Graphic_ptr>(new LayoutLayer(g, under, 0), this);
}

Graphic_ptr LayoutKitImpl::front(Graphic_ptr g, Graphic_ptr over)
{
  return create<LayoutLayer, Graphic_ptr>(new LayoutLayer(g, 0, over), this);
}

Graphic_ptr LayoutKitImpl::between(Graphic_ptr g, Graphic_ptr under, Graphic_ptr over)
{
  return create<LayoutLayer, Graphic_ptr>(new LayoutLayer(g, under, over), this);
}

Graphic_ptr LayoutKitImpl::glue(Axis a, Coord natural, Coord stretch, Coord shrink, Alignment align)
{
  return create<Glue, Graphic_ptr>(new Glue(a, natural, stretch, shrink, align), this);
}

Graphic_ptr LayoutKitImpl::glueRequisition(const Graphic::Requisition &r)
{
  return create<Glue, Graphic_ptr>(new Glue(r), this);
}

Graphic_ptr LayoutKitImpl::hfil()
{
  return create<Glue, Graphic_ptr>(new Glue(xaxis, 0., fil_, 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::hglueFil(Coord natural)
{
  return create<Glue, Graphic_ptr>(new Glue(xaxis, natural, fil_, 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::hglue(Coord natural, Coord stretch, Coord shrink)
{
  return create<Glue, Graphic_ptr>(new Glue(xaxis, natural, stretch, shrink, 0.), this);
}

Graphic_ptr LayoutKitImpl::hglueAligned(Coord natural, Coord stretch, Coord shrink, Alignment a)
{
  return create<Glue, Graphic_ptr>(new Glue(xaxis, natural, stretch, shrink, a), this);
}

Graphic_ptr LayoutKitImpl::hspace(Coord natural)
{
  return create<Glue, Graphic_ptr>(new Glue(xaxis, natural, 0., 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::vfil()
{
  return create<Glue, Graphic_ptr>(new Glue(yaxis, 0., fil_, 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::vglueFil(Coord natural)
{
  return create<Glue, Graphic_ptr>(new Glue(yaxis, natural, fil_, 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::vglue(Coord natural, Coord stretch, Coord shrink)
{
  return create<Glue, Graphic_ptr>(new Glue(yaxis, natural, stretch, shrink, 0.), this);
}

Graphic_ptr LayoutKitImpl::vglueAligned(Coord natural, Coord stretch, Coord shrink, Alignment a)
{
  return create<Glue, Graphic_ptr>(new Glue(yaxis, natural, stretch, shrink, a), this);
}

Graphic_ptr LayoutKitImpl::vspace(Coord natural)
{
  return create<Glue, Graphic_ptr>(new Glue(yaxis, natural, 0., 0., 0.), this);
}

Graphic_ptr LayoutKitImpl::shapeOf(Graphic_ptr g)
{
  return create<ShapeOf, Graphic_ptr>(new ShapeOf(g, 0, 0), this);
}

Graphic_ptr LayoutKitImpl::shapeOfXY(Graphic_ptr gx, Graphic_ptr gy)
{
  return create<ShapeOf, Graphic_ptr>(new ShapeOf(gx, gy, 0), this);
}

Graphic_ptr LayoutKitImpl::shapeOfXYZ(Graphic_ptr gx, Graphic_ptr gy, Graphic_ptr gz)
{
  return create<ShapeOf, Graphic_ptr>(new ShapeOf(gx, gy, gz), this);
}

// Graphic_ptr LayoutKitImpl::strut(Font_ptr f, Coord natural, Coord stretch, Coord shrink)
// {
//   return new Strut(f, natural, stretch, shrink);
// }

// Graphic_ptr LayoutKitImpl::hstrut(Coord right_bearing, Coord left_bearing, Coord natural, Coord stretch, Coord shrink)
// {
//   return new HStrut(right_bearing, left_bearing, natural, stretch, shrink);
// }

// Graphic_ptr LayoutKitImpl::vstrut(Coord ascent, Coord descent, Coord natural, Coord stretch, Coord shrink)
// {
//   return new VStrut(ascent, descent, natural, stretch, shrink);
// }

// Graphic_ptr LayoutKitImpl::spaces(Long count, Coord each, Font_ptr f, Color_ptr c)
// {
//   return new Space(count, each, f, c);
// }

Graphic_ptr LayoutKitImpl::align(Graphic_ptr g, Alignment x, Alignment y)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutCenter(xaxis, x), new LayoutCenter(yaxis, y)));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::alignAxis(Graphic_ptr g, Axis a, Alignment align)
{
  Placement *placement = new Placement(new LayoutCenter(a, align));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}
 
Graphic_ptr LayoutKitImpl::halign(Graphic_ptr g, Alignment x)
{
  return alignAxis(g, xaxis, x);
}
 
Graphic_ptr LayoutKitImpl::valign(Graphic_ptr g, Alignment y)
{
  return alignAxis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::fixed(Graphic_ptr g, Coord x, Coord y)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutFixed(xaxis, x), new LayoutFixed(yaxis, y)));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::fixedAxis(Graphic_ptr g, Axis a, Coord size)
{
  Placement *placement = new Placement(new LayoutFixed(a, size));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hfixed(Graphic_ptr g, Coord x)
{
  return fixedAxis(g, xaxis, x);
}

Graphic_ptr LayoutKitImpl::vfixed(Graphic_ptr g, Coord y)
{
  return fixedAxis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::flexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutVariable(xaxis, stretch, shrink),
							   new LayoutVariable(yaxis, stretch, shrink)));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::flexibleFil(Graphic_ptr g)
{
  return flexible(g, fil_, 0.);
}

Graphic_ptr LayoutKitImpl::flexibleAxis(Graphic_ptr g, Axis a, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutVariable(a, stretch, shrink));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hflexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  return flexibleAxis(g, xaxis, stretch, shrink);
}

Graphic_ptr LayoutKitImpl::vflexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  return flexibleAxis(g, yaxis, stretch, shrink);
}

Graphic_ptr LayoutKitImpl::natural(Graphic_ptr g, Coord x, Coord y)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutNatural(xaxis, x), new LayoutNatural(yaxis, y)));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::naturalAxis(Graphic_ptr g, Axis a, Coord size)
{
  Placement *placement = new Placement(new LayoutNatural(a, size));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hnatural(Graphic_ptr g, Coord x)
{
  return naturalAxis(g, xaxis, x);
}

Graphic_ptr LayoutKitImpl::vnatural(Graphic_ptr g, Coord y)
{
  return naturalAxis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::margin(Graphic_ptr g, Coord all)
{
  Placement *placement = new Placement(new LayoutMargin(all));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::marginFlexible(Graphic_ptr g, Coord margin, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutMargin(margin, stretch, shrink, margin, stretch, shrink,
							margin, stretch, shrink, margin, stretch, shrink));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::marginLRBT(Graphic_ptr g, Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin)
{
  Placement *placement = new Placement(new LayoutMargin(lmargin, rmargin, bmargin, tmargin));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::marginLRBTFlexible(Graphic_ptr g, Coord lmargin, Coord lstretch, Coord lshrink,
					      Coord rmargin, Coord rstretch, Coord rshrink,
					      Coord bmargin, Coord bstretch, Coord bshrink,
					      Coord tmargin, Coord tstretch, Coord tshrink)
{
  Placement *placement = new Placement(new LayoutMargin(lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
							bmargin, bstretch, bshrink, tmargin, tstretch, tshrink));
  placement->_obj_is_ready(_boa());
  placement->body(g);
  graphics.push_back(placement);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hmargin(Graphic_ptr g, Coord both)
{
  return marginLRBT(g, both, both, 0., 0.);
}

Graphic_ptr LayoutKitImpl::hmarginLR(Graphic_ptr g, Coord lmargin, Coord rmargin)
{
  return marginLRBT(g, lmargin, rmargin, 0., 0.);
}

Graphic_ptr LayoutKitImpl::hmarginLRFlexible(Graphic_ptr g, Coord lmargin, Coord lstretch, Coord lshrink,
					     Coord rmargin, Coord rstretch, Coord rshrink)
{
  return marginLRBTFlexible(g, lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
			    0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::vmargin(Graphic_ptr g, Coord both) { return marginLRBT(g, 0., 0., both, both);}

Graphic_ptr LayoutKitImpl::vmarginBT(Graphic_ptr g, Coord bmargin, Coord tmargin)
{
  return marginLRBT(g, 0., 0., bmargin, tmargin);
}

Graphic_ptr LayoutKitImpl::vmarginBTFlexible(Graphic_ptr g, Coord bmargin, Coord bstretch, Coord bshrink,
					     Coord tmargin, Coord tstretch, Coord tshrink)
{
  return marginLRBTFlexible(g, 0., 0., 0., 0., 0., 0., bmargin, bstretch, bshrink, tmargin, tstretch, tshrink);
}

Graphic_ptr LayoutKitImpl::lmargin(Graphic_ptr g, Coord natural)
{
  return marginLRBT(g, natural, 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::lmarginFlexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return marginLRBTFlexible(g, natural, stretch, shrink, 0., 0., 0., 0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::rmargin(Graphic_ptr g, Coord natural)
{
  return marginLRBT(g, 0., natural, 0., 0.);
}

Graphic_ptr LayoutKitImpl::rmarginFlexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return marginLRBTFlexible(g, 0., 0., 0., natural, stretch, shrink, 0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::bmargin(Graphic_ptr g, Coord natural)
{
  return marginLRBT(g, 0., 0., natural, 0.);
}

Graphic_ptr LayoutKitImpl::bmarginFlexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return marginLRBTFlexible(g, 0., 0., 0., 0., 0., 0., natural, stretch, shrink, 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::tmargin(Graphic_ptr g, Coord natural)
{
  return marginLRBT(g, 0., 0., 0., natural);
}

Graphic_ptr LayoutKitImpl::tmarginFlexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return marginLRBTFlexible(g, 0., 0., 0., 0., 0., 0., 0., 0., 0., natural, stretch, shrink);
}

EXPORT_PLUGIN(LayoutKitImpl,interface(LayoutKit))
