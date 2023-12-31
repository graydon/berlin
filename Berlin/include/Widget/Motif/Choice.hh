/*$Id: Choice.hh,v 1.7 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _Motif_Choice_hh
#define _Motif_Choice_hh

#include <Warsaw/config.hh>
#include <Warsaw/Choice.hh>
#include <Warsaw/Selection.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/ControllerImpl.hh>
#include <vector>

namespace Motif
{

class Choice : public virtual POA_Warsaw::Choice,
	       public ControllerImpl
{
 public:
  Choice(Warsaw::Selection_ptr, Warsaw::LayoutKit_ptr, Warsaw::ToolKit_ptr, Warsaw::WidgetKit_ptr);
  virtual ~Choice();
  virtual Warsaw::Selection_ptr state();
  virtual Warsaw::Tag append_item(Warsaw::Graphic_ptr) = 0;
  virtual Warsaw::Tag prepend_item(Warsaw::Graphic_ptr) = 0;
  void remove_item(Warsaw::Tag) = 0;
 protected:
  RefCount_var<Warsaw::Selection> selection;
  RefCount_var<Warsaw::LayoutKit> layout;
  RefCount_var<Warsaw::ToolKit>   tools;
  RefCount_var<Warsaw::WidgetKit> widgets;
};

class ToggleChoice : public Choice
{
 public:
  ToggleChoice(Warsaw::Selection_ptr, Warsaw::LayoutKit_ptr, Warsaw::ToolKit_ptr, Warsaw::WidgetKit_ptr);
  virtual Warsaw::Tag append_item(Warsaw::Graphic_ptr);
  virtual Warsaw::Tag prepend_item(Warsaw::Graphic_ptr);
  void remove_item(Warsaw::Tag);
};

class CheckboxChoice : public Choice
{
 public:
  CheckboxChoice(Warsaw::Selection_ptr, Warsaw::LayoutKit_ptr, Warsaw::ToolKit_ptr, Warsaw::WidgetKit_ptr);
  virtual Warsaw::Tag append_item(Warsaw::Graphic_ptr);
  virtual Warsaw::Tag prepend_item(Warsaw::Graphic_ptr);
  void remove_item(Warsaw::Tag);
};

class ToolChoice : public Choice
{
 public:
  ToolChoice(Warsaw::Selection_ptr, Warsaw::LayoutKit_ptr, Warsaw::ToolKit_ptr, Warsaw::WidgetKit_ptr);
  virtual Warsaw::Tag append_item(Warsaw::Graphic_ptr);
  virtual Warsaw::Tag prepend_item(Warsaw::Graphic_ptr);
  void remove_item(Warsaw::Tag);
};

};
#endif
