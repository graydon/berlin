/*$Id: Frame.hh,v 1.11 1999/09/30 17:23:33 gray Exp $
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
#ifndef _Frame_hh
#define _Frame_hh

#include "Warsaw/config.hh"
#include "Widget/Bevel.hh"
#include "Warsaw/View.hh"
#include "Warsaw/Telltale.hh"
#include "Berlin/SubjectImpl.hh"

declare_corba_ptr_type(Subject)

class Frame : public Bevel
{
public:
  enum type { concav, convex, flat, black};
  Frame(Coord, const Color &, type, bool);
  virtual ~Frame();
  virtual void draw(DrawTraversal_ptr);
protected:
  Color color;
  type mode;
  bool fill;
};

class DynamicFrame : implements(View), public Frame
{
 public:
  DynamicFrame(Coord, const Color &, type, type, Telltale::Flag, bool);
  virtual ~DynamicFrame();
  void attach(Telltale_ptr);
  virtual void update(Subject_ptr, const CORBA::Any &);
 protected:
  Telltale_var telltale;
  type type1;
  type type2;
  bool on;
  Telltale::Flag mask;
};

#endif /* _Frame_hh */
