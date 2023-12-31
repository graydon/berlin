/*$Id: Bevel.hh,v 1.8 1999/09/30 17:23:33 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _Bevel_hh
#define _Bevel_hh

#include "Warsaw/config.hh"
#include "Berlin/MonoGraphic.hh"
#include "Warsaw/DrawingKit.hh"
#include "Berlin/RegionImpl.hh"

class Bevel : public MonoGraphic
{
public:
  Bevel(Coord, Alignment = 0.0, Alignment = 0.0, bool = true, bool = true);
  ~Bevel();

  virtual void request(Requisition &);
  virtual void traverse(Traversal_ptr);
  virtual void extension(const Allocation::Info &a, Region_ptr);
  virtual void allocate(Tag, const Allocation::Info &);

  static void rect(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void leftArrow(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void rightArrow(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void upArrow(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void downArrow(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void diamond(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);
  static void circle(DrawTraversal_ptr, Coord, const Color &, const Color &, const Color&, Coord, Coord, Coord, Coord, bool);

protected:
  void allocateSpan(const Requirement &, Region::Allotment &, Coord, Alignment);
  Coord thickness;
  Alignment xalign;
  Alignment yalign;
  bool hmargin : 1;
  bool vmargin : 1;
  RegionImpl *allocation;
};

#endif /* _Bevel_hh */
