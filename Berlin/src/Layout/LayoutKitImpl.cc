/*$Id: LayoutKitImpl.cc,v 1.22 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefelds <stefan@berlin-consortium.org>
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

#include <Prague/Sys/Tracer.hh>
#include <Berlin/ImplVar.hh>
#include "Layout/LayoutKitImpl.hh"
#include "Layout/Box.hh"
#include "Layout/Deck.hh"
#include "Layout/GridImpl.hh"
#include "Layout/StageImpl.hh"
#include "Layout/Glue.hh"
#include "Layout/Placement.hh"
#include "Layout/ShapeOf.hh"
#include "Layout/ViewportImpl.hh"

using namespace Prague;
using namespace Warsaw;
using namespace Layout;

LayoutKitImpl::LayoutKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p), _fill(GraphicImpl::infinity) {}
LayoutKitImpl::~LayoutKitImpl() { Trace trace("LayoutKitImpl::~LayoutKitImpl");}
void LayoutKitImpl::fill(Coord c) { _fill = c;}
Coord LayoutKitImpl::fill() { return _fill;}
Graphic_ptr LayoutKitImpl::clipper(Graphic_ptr g)
{
  return Graphic::_nil();
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
  activate(vp);
  vp->body(g);
  return vp->_this();
}

Stage_ptr LayoutKitImpl::create_stage()
{
  return create<Stage>(new StageImpl);
}

Grid_ptr LayoutKitImpl::fixed_grid(const Grid::Index &upper)
{
  return create<Grid>(new GridImpl(upper));
}

Graphic_ptr LayoutKitImpl::fixed_range(Grid_ptr g, const Grid::Range &r)
{
  return create<Graphic>(new SubGridImpl(g, r));
}


Graphic_ptr LayoutKitImpl::hbox()
{
  return create<Graphic>(new HBox);
}

Graphic_ptr LayoutKitImpl::vbox()
{
  Trace trace("LayoutKitImpl::vbox");
  return create<Graphic>(new VBox);
}

Graphic_ptr LayoutKitImpl::hbox_first_aligned()
{
  return create<Graphic>(new HBoxFirstAligned);
}

Graphic_ptr LayoutKitImpl::vbox_first_aligned()
{
  return create<Graphic>(new VBoxFirstAligned);
}

Graphic_ptr LayoutKitImpl::hbox_align_elements(Alignment align)
{
  return create<Graphic>(new HBoxAlignElements(align));
}

Graphic_ptr LayoutKitImpl::vbox_align_elements(Alignment align)
{
  return create<Graphic>(new VBoxAlignElements(align));
}

Graphic_ptr LayoutKitImpl::overlay()
{
  return create<Graphic>(new Overlay);
}

Graphic_ptr LayoutKitImpl::deck()
{
  return create<Graphic>(new Deck);
}

Graphic_ptr LayoutKitImpl::back(Graphic_ptr g, Graphic_ptr under)
{
  return create<Graphic>(new LayoutLayer(g, under, 0));
}

Graphic_ptr LayoutKitImpl::front(Graphic_ptr g, Graphic_ptr over)
{
  return create<Graphic>(new LayoutLayer(g, 0, over));
}

Graphic_ptr LayoutKitImpl::between(Graphic_ptr g, Graphic_ptr under, Graphic_ptr over)
{
  return create<Graphic>(new LayoutLayer(g, under, over));
}

Graphic_ptr LayoutKitImpl::glue(Axis a, Coord natural, Coord stretch, Coord shrink, Alignment align)
{
  return create<Graphic>(new Glue(a, natural, stretch, shrink, align));
}

Graphic_ptr LayoutKitImpl::glue_requisition(const Graphic::Requisition &r)
{
  return create<Graphic>(new Glue(r));
}

Graphic_ptr LayoutKitImpl::hfill()
{
  return create<Graphic>(new Glue(xaxis, 0., _fill, 0., 0.));
}

Graphic_ptr LayoutKitImpl::hglue_fill(Coord natural)
{
  return create<Graphic>(new Glue(xaxis, natural, _fill, 0., 0.));
}

Graphic_ptr LayoutKitImpl::hglue(Coord natural, Coord stretch, Coord shrink)
{
  return create<Graphic>(new Glue(xaxis, natural, stretch, shrink, 0.));
}

Graphic_ptr LayoutKitImpl::hglue_aligned(Coord natural, Coord stretch, Coord shrink, Alignment a)
{
  return create<Graphic>(new Glue(xaxis, natural, stretch, shrink, a));
}

Graphic_ptr LayoutKitImpl::hspace(Coord natural)
{
  return create<Graphic>(new Glue(xaxis, natural, 0., 0., 0.));
}

Graphic_ptr LayoutKitImpl::vfill()
{
  return create<Graphic>(new Glue(yaxis, 0., _fill, 0., 0.));
}

Graphic_ptr LayoutKitImpl::vglue_fill(Coord natural)
{
  return create<Graphic>(new Glue(yaxis, natural, _fill, 0., 0.));
}

Graphic_ptr LayoutKitImpl::vglue(Coord natural, Coord stretch, Coord shrink)
{
  return create<Graphic>(new Glue(yaxis, natural, stretch, shrink, 0.));
}

Graphic_ptr LayoutKitImpl::vglue_aligned(Coord natural, Coord stretch, Coord shrink, Alignment a)
{
  return create<Graphic>(new Glue(yaxis, natural, stretch, shrink, a));
}

Graphic_ptr LayoutKitImpl::vspace(Coord natural)
{
  return create<Graphic>(new Glue(yaxis, natural, 0., 0., 0.));
}

Graphic_ptr LayoutKitImpl::shape_of(Graphic_ptr g)
{
  return create<Graphic>(new ShapeOf(g, 0, 0));
}

Graphic_ptr LayoutKitImpl::shape_of_xy(Graphic_ptr gx, Graphic_ptr gy)
{
  return create<Graphic>(new ShapeOf(gx, gy, 0));
}

Graphic_ptr LayoutKitImpl::shape_of_xyz(Graphic_ptr gx, Graphic_ptr gy, Graphic_ptr gz)
{
  return create<Graphic>(new ShapeOf(gx, gy, gz));
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
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::align_axis(Graphic_ptr g, Axis a, Alignment align)
{
  Placement *placement = new Placement(new LayoutCenter(a, align));
  activate(placement);
  placement->body(g);
  return placement->_this();
}
 
Graphic_ptr LayoutKitImpl::halign(Graphic_ptr g, Alignment x)
{
  return align_axis(g, xaxis, x);
}
 
Graphic_ptr LayoutKitImpl::valign(Graphic_ptr g, Alignment y)
{
  return align_axis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::fixed_size(Graphic_ptr g, Coord x, Coord y)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutFixed(xaxis, x), new LayoutFixed(yaxis, y)));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::fixed_axis(Graphic_ptr g, Axis a, Coord size)
{
  Placement *placement = new Placement(new LayoutFixed(a, size));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hfixed(Graphic_ptr g, Coord x)
{
  return fixed_axis(g, xaxis, x);
}

Graphic_ptr LayoutKitImpl::vfixed(Graphic_ptr g, Coord y)
{
  return fixed_axis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::flexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutVariable(xaxis, stretch, shrink),
							   new LayoutVariable(yaxis, stretch, shrink)));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::flexible_fill(Graphic_ptr g)
{
  return flexible(g, _fill, 0.);
}

Graphic_ptr LayoutKitImpl::flexible_axis(Graphic_ptr g, Axis a, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutVariable(a, stretch, shrink));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hflexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  return flexible_axis(g, xaxis, stretch, shrink);
}

Graphic_ptr LayoutKitImpl::vflexible(Graphic_ptr g, Coord stretch, Coord shrink)
{
  return flexible_axis(g, yaxis, stretch, shrink);
}

Graphic_ptr LayoutKitImpl::natural(Graphic_ptr g, Coord x, Coord y)
{
  Placement *placement = new Placement(new LayoutSuperpose(new LayoutNatural(xaxis, x), new LayoutNatural(yaxis, y)));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::natural_axis(Graphic_ptr g, Axis a, Coord size)
{
  Placement *placement = new Placement(new LayoutNatural(a, size));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hnatural(Graphic_ptr g, Coord x)
{
  return natural_axis(g, xaxis, x);
}

Graphic_ptr LayoutKitImpl::vnatural(Graphic_ptr g, Coord y)
{
  return natural_axis(g, yaxis, y);
}

Graphic_ptr LayoutKitImpl::margin(Graphic_ptr g, Coord all)
{
  Placement *placement = new Placement(new LayoutMargin(all));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::margin_flexible(Graphic_ptr g, Coord margin, Coord stretch, Coord shrink)
{
  Placement *placement = new Placement(new LayoutMargin(margin, stretch, shrink, margin, stretch, shrink,
							margin, stretch, shrink, margin, stretch, shrink));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::margin_lrbt(Graphic_ptr g, Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin)
{
  Placement *placement = new Placement(new LayoutMargin(lmargin, rmargin, bmargin, tmargin));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::margin_lrbt_flexible(Graphic_ptr g, Coord lmargin, Coord lstretch, Coord lshrink,
						Coord rmargin, Coord rstretch, Coord rshrink,
						Coord tmargin, Coord tstretch, Coord tshrink,
						Coord bmargin, Coord bstretch, Coord bshrink)
{
  Placement *placement = new Placement(new LayoutMargin(lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
							tmargin, tstretch, tshrink, bmargin, bstretch, bshrink));
  activate(placement);
  placement->body(g);
  return placement->_this();
}

Graphic_ptr LayoutKitImpl::hmargin(Graphic_ptr g, Coord both)
{
  return margin_lrbt(g, both, both, 0., 0.);
}

Graphic_ptr LayoutKitImpl::hmargin_lr(Graphic_ptr g, Coord lmargin, Coord rmargin)
{
  return margin_lrbt(g, lmargin, rmargin, 0., 0.);
}

Graphic_ptr LayoutKitImpl::hmargin_lr_flexible(Graphic_ptr g, Coord lmargin, Coord lstretch, Coord lshrink,
					       Coord rmargin, Coord rstretch, Coord rshrink)
{
  return margin_lrbt_flexible(g, lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
			      0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::vmargin(Graphic_ptr g, Coord both) { return margin_lrbt(g, 0., 0., both, both);}

Graphic_ptr LayoutKitImpl::vmargin_bt(Graphic_ptr g, Coord tmargin, Coord bmargin)
{
  return margin_lrbt(g, 0., 0., tmargin, bmargin);
}

Graphic_ptr LayoutKitImpl::vmargin_bt_flexible(Graphic_ptr g, Coord tmargin, Coord tstretch, Coord tshrink,
					       Coord bmargin, Coord bstretch, Coord bshrink)
{
  return margin_lrbt_flexible(g, 0., 0., 0., 0., 0., 0., tmargin, tstretch, tshrink, bmargin, bstretch, bshrink);
}

Graphic_ptr LayoutKitImpl::lmargin(Graphic_ptr g, Coord natural)
{
  return margin_lrbt(g, natural, 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::lmargin_flexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return margin_lrbt_flexible(g, natural, stretch, shrink, 0., 0., 0., 0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::rmargin(Graphic_ptr g, Coord natural)
{
  return margin_lrbt(g, 0., natural, 0., 0.);
}

Graphic_ptr LayoutKitImpl::rmargin_flexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return margin_lrbt_flexible(g, 0., 0., 0., natural, stretch, shrink, 0., 0., 0., 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::bmargin(Graphic_ptr g, Coord natural)
{
  return margin_lrbt(g, 0., 0., 0., natural);
}

Graphic_ptr LayoutKitImpl::bmargin_flexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return margin_lrbt_flexible(g, 0., 0., 0., 0., 0., 0., natural, stretch, shrink, 0., 0., 0.);
}

Graphic_ptr LayoutKitImpl::tmargin(Graphic_ptr g, Coord natural)
{
  return margin_lrbt(g, 0., 0., natural, 0.);
}

Graphic_ptr LayoutKitImpl::tmargin_flexible(Graphic_ptr g, Coord natural, Coord stretch, Coord shrink)
{
  return margin_lrbt_flexible(g, 0., 0., 0., 0., 0., 0., natural, stretch, shrink, 0., 0., 0.);
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "LayoutKitImpl"};
  return new KitFactoryImpl<LayoutKitImpl> ("IDL:Warsaw/LayoutKit:1.0", properties, 1);
}
