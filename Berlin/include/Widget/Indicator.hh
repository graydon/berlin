/*$Id: Indicator.hh,v 1.1 1999/08/26 14:06:40 gray Exp $
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
#ifndef _Indicator_hh
#define _Indicator_hh

#include "Warsaw/config.hh"
#include "Widget/Bevel.hh"
#include "Warsaw/View.hh"
#include "Warsaw/Telltale.hh"
#include "Berlin/SubjectImpl.hh"

declare_corba_ptr_type(Subject)

class Indicator : implements(View), public MonoGraphic
{
 public:
  Indicator(const Color &);
  virtual ~Indicator();
  void attach(Telltale_ptr);
  virtual void update(Subject_ptr, const CORBA::Any &);
  virtual void traverse(Traversal_ptr);
  virtual void draw(DrawTraversal_ptr);
 protected:
  Color color;
  Telltale_var telltale;
};

#endif /* _Indicator_hh */
