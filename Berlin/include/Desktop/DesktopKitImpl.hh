/*$Id: DesktopKitImpl.hh,v 1.12 2000/11/10 20:55:09 stefan Exp $
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
#ifndef _DesktopKitImpl_hh
#define _DesktopKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/DesktopKit.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/Desktop.hh>
#include <Berlin/KitImpl.hh>
#include <Berlin/RefCountVar.hh>

class WindowImpl;
class DesktopImpl;

class DesktopKitImpl : public virtual POA_Warsaw::DesktopKit,
		       public KitImpl
{
 public:
  DesktopKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~DesktopKitImpl();
  virtual void bind(Warsaw::ServerContext_ptr);
  virtual Warsaw::Desktop_ptr desk();
  virtual Warsaw::Window_ptr shell(Warsaw::Controller_ptr);
  virtual Warsaw::Window_ptr transient(Warsaw::Controller_ptr);
  virtual Warsaw::Window_ptr pulldown(Warsaw::Controller_ptr);

  virtual Warsaw::Command_ptr move(Warsaw::Window_ptr);
  virtual Warsaw::Command_ptr resize(Warsaw::Window_ptr);
  virtual Warsaw::Command_ptr move_resize(Warsaw::Window_ptr, Warsaw::Alignment, Warsaw::Alignment, CORBA::Short);
  virtual Warsaw::Command_ptr relayer(Warsaw::Window_ptr);
  virtual Warsaw::Command_ptr map(Warsaw::Window_ptr, CORBA::Boolean);
 private:
  RefCount_var<Warsaw::Desktop>   _desktop;
  RefCount_var<Warsaw::LayoutKit> _layout;
  RefCount_var<Warsaw::ToolKit>   _tool;
  RefCount_var<Warsaw::WidgetKit> _widget;
};

#endif
