/*$Id: LayoutKitImpl.hh,v 1.17 2000/09/19 21:11:04 stefan Exp $
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
#ifndef _LayoutKitImpl_hh
#define _LayoutKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/LayoutKit.hh>
#include <Berlin/KitImpl.hh>
#include <vector>

class GraphicImpl;

class LayoutKitImpl : public virtual POA_Warsaw::LayoutKit,
		      public KitImpl
{
public:
  LayoutKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  ~LayoutKitImpl();
  virtual Warsaw::Coord fill();
  virtual void fill(Warsaw::Coord);
  virtual Warsaw::Graphic_ptr clipper(Warsaw::Graphic_ptr);
//   virtual Graphic_ptr create_backdrop();
//   virtual AutoScroll_ptr create_auto_scroll(Adjustment_ptr x_adjustment, Adjustment_ptr y_adjustment);
//   virtual FullyVisibleConstraint_ptr create_fully_visible_constraint(Float usable, Float align);
//   virtual Scrollable* scroll_box(Axis a);
  virtual Layout::Viewport_ptr scrollable(Warsaw::Graphic_ptr);
  virtual Layout::Stage_ptr create_stage();
  virtual Layout::Grid_ptr fixed_grid(const Layout::Grid::Index &);
  virtual Warsaw::Graphic_ptr fixed_range(Layout::Grid_ptr g, const Layout::Grid::Range &);
  virtual Warsaw::Graphic_ptr hbox();
  virtual Warsaw::Graphic_ptr vbox();
  virtual Warsaw::Graphic_ptr hbox_first_aligned();
  virtual Warsaw::Graphic_ptr vbox_first_aligned();
  virtual Warsaw::Graphic_ptr hbox_align_elements(Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr vbox_align_elements(Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr overlay();
  virtual Warsaw::Graphic_ptr deck();
  virtual Warsaw::Graphic_ptr back(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr front(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr between(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr glue(Warsaw::Axis, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr glue_requisition(const Warsaw::Graphic::Requisition &);
  virtual Warsaw::Graphic_ptr hfill();
  virtual Warsaw::Graphic_ptr hglue_fill(Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hglue(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hglue_aligned(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr hspace(Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vfill();
  virtual Warsaw::Graphic_ptr vglue_fill(Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vglue(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vglue_aligned(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr vspace(Warsaw::Coord);
  virtual Warsaw::Graphic_ptr shape_of(Warsaw::Graphic_ptr g);
  virtual Warsaw::Graphic_ptr shape_of_xy(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr shape_of_xyz(Warsaw::Graphic_ptr, Warsaw::Graphic_ptr, Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr align(Warsaw::Graphic_ptr, Warsaw::Alignment, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr align_axis(Warsaw::Graphic_ptr, Warsaw::Axis, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr halign(Warsaw::Graphic_ptr, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr valign(Warsaw::Graphic_ptr, Warsaw::Alignment);
  virtual Warsaw::Graphic_ptr fixed_size(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr fixed_axis(Warsaw::Graphic_ptr, Warsaw::Axis, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hfixed(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vfixed(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr flexible_fill(Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr flexible_axis(Warsaw::Graphic_ptr, Warsaw::Axis, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hflexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vflexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr natural(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr natural_axis(Warsaw::Graphic_ptr, Warsaw::Axis, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hnatural(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vnatural(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr margin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr margin_lrbt(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr margin_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr margin_lrbt_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
						   Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
						   Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
						   Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hmargin_lr(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr hmargin_lr_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
						  Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vmargin_bt(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr vmargin_bt_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord,
						  Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr lmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr lmargin_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr rmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr rmargin_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr bmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr bmargin_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr tmargin(Warsaw::Graphic_ptr, Warsaw::Coord);
  virtual Warsaw::Graphic_ptr tmargin_flexible(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
private:
  template <typename I, typename Im>
  typename I::_ptr_type create(Im *impl)
  {
    activate(impl);
    return impl->_this();
  }

  Warsaw::Coord _fill;
};

#endif
