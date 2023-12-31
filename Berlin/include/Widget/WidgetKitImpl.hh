/*$Id: WidgetKitImpl.hh,v 1.18 1999/10/21 20:23:51 gray Exp $
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
#ifndef _WidgetKitImpl_hh
#define _WidgetKitImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/WidgetKit.hh"
#include "Berlin/CloneableImpl.hh"
#include <vector>

class GraphicImpl;
class SubjectImpl;

class WidgetKitImpl : lcimplements(WidgetKit), virtual public CloneableImpl
{
 public:
  WidgetKitImpl();
  virtual ~WidgetKitImpl();
  virtual void bind(ServerContext_ptr sc);
  TelltaleConstraint_ptr exclusive();
  TelltaleConstraint_ptr selectionRequired();
  Telltale_ptr     constrainedTelltale(TelltaleConstraint_ptr);
  Telltale_ptr     normalTelltale();
  BoundedValue_ptr bvalue(Coord, Coord, Coord, Coord, Coord);
  BoundedRange_ptr brange(Coord, Coord, Coord, Coord, Coord, Coord);
  TextBuffer_ptr   text();
  StreamBuffer_ptr stream();

  Graphic_ptr      debugger(Graphic_ptr  g, const char*  s);
  Graphic_ptr      inset(Graphic_ptr, const Color &, CORBA::Boolean);
  Graphic_ptr      outset(Graphic_ptr, const Color &, CORBA::Boolean);
  Graphic_ptr      filler(Graphic_ptr, const Color &);
  Graphic_ptr      indicator(Graphic_ptr, const Color &, Telltale_ptr);
  View_ptr         pushButtonFrame(Graphic_ptr, const Color &, Telltale_ptr);
  Controller_ptr   dragger(Graphic_ptr, Command_ptr);
  Controller_ptr   stepper(Graphic_ptr, Command_ptr);
  
  Button_ptr       pushButton(Graphic_ptr, const Color &, Command_ptr);
  Controller_ptr   toggle(Graphic_ptr, const Color &);
  Graphic_ptr      gauge(const Color &, BoundedValue_ptr);
  Controller_ptr   slider(const Color &, BoundedValue_ptr);
 private:
  LayoutKit_var lk;
  vector<GraphicImpl *> graphics;
  vector<SubjectImpl *> subjects;
};

#endif /* _WidgetKitImpl_hh */
