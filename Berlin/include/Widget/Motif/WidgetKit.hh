/*$Id: WidgetKit.hh,v 1.11 2000/12/21 21:05:43 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Motif_WidgetKitImpl_hh
#define _Motif_WidgetKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/TextKit.hh>
#include <Berlin/KitImpl.hh>
#include <Berlin/RefCountBaseImpl.hh>
#include <Berlin/RefCountVar.hh>
#include <vector>

class GraphicImpl;

namespace Motif
{

class WidgetKit : public virtual POA_Warsaw::WidgetKit,
		  public KitImpl
{
 public:
  WidgetKit(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~WidgetKit();
  virtual void bind(Warsaw::ServerContext_ptr);
  
  Warsaw::Trigger_ptr      button(Warsaw::Graphic_ptr, Warsaw::Command_ptr);
  Warsaw::Controller_ptr   toggle(Warsaw::Graphic_ptr);
  Warsaw::Graphic_ptr      gauge(Warsaw::BoundedValue_ptr);
  Warsaw::Controller_ptr   slider(Warsaw::BoundedValue_ptr, Warsaw::Axis);
  Warsaw::Controller_ptr   panner(Warsaw::BoundedRange_ptr, Warsaw::BoundedRange_ptr);
  Warsaw::Controller_ptr   scrollbar(Warsaw::BoundedRange_ptr, Warsaw::Axis);
  Warsaw::Choice_ptr       toggle_choice();
  Warsaw::Choice_ptr       checkbox_choice();
  Warsaw::Choice_ptr       toolbar();
  Warsaw::Controller_ptr   terminal();

  Warsaw::Controller_ptr   scrollable(Warsaw::Graphic_ptr);
 private:
  RefCount_var<Warsaw::LayoutKit>  layout;
  RefCount_var<Warsaw::CommandKit> command;
  RefCount_var<Warsaw::ToolKit>    tool;
  RefCount_var<Warsaw::TextKit>    text;
};

};

#endif
