/*$Id: Application.hh,v 1.15 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _Application_hh
#define _Application_hh

#include <Warsaw/config.hh>
#include <Warsaw/TextKit.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/FigureKit.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/DesktopKit.hh>
#include <Warsaw/ImageKit.hh>
#include <Warsaw/GadgetKit.hh>
#include <Warsaw/Trigger.hh>
#include <Warsaw/BoundedValue.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/ClientContextImpl.hh>
#include <Prague/Sys/Signal.hh>
#include <Warsaw/Unicode.hh>
#include <Warsaw/Selection.hh>
#include <Warsaw/Choice.hh>
#include <unistd.h>
#include <iostream>
#include <vector>

class Application
{
  struct Item
  {
    Warsaw::Tag id;
    Warsaw::Command_var mapper;
    Warsaw::BoundedValue_var alpha;
    Warsaw::BoundedValue_var red;
    Warsaw::BoundedValue_var blue;
    Warsaw::BoundedValue_var green;
    Warsaw::BoundedValue_var zrotation;
    Warsaw::BoundedValue_var yrotation;
    Warsaw::BoundedValue_var zoom;
    Warsaw::Command_var settings;
  };
  typedef std::vector<Item> list_t;
  class Mapper;
  friend class Mapper;
public:
  class CommandImpl : public virtual POA_Warsaw::Command,
                      public virtual PortableServer::RefCountServantBase
  {
  public:
    virtual void execute(const CORBA::Any &) = 0;
    virtual void destroy();
  };
  Application(Warsaw::ServerContext_ptr);
  Warsaw::TextKit_ptr text() { return Warsaw::TextKit::_duplicate(tk);}
  Warsaw::DesktopKit_ptr desktop() { return Warsaw::DesktopKit::_duplicate(dk);}
  Warsaw::LayoutKit_ptr layout() { return Warsaw::LayoutKit::_duplicate(lk);}
  Warsaw::ToolKit_ptr tool() { return Warsaw::ToolKit::_duplicate(ttk);}
  Warsaw::WidgetKit_ptr widget() { return Warsaw::WidgetKit::_duplicate(wk);}
  Warsaw::FigureKit_ptr figure() { return Warsaw::FigureKit::_duplicate(fk);}
  Warsaw::CommandKit_ptr command() { return Warsaw::CommandKit::_duplicate(ck);}
  Warsaw::ImageKit_ptr image() { return Warsaw::ImageKit::_duplicate(ik);}
  Warsaw::GadgetKit_ptr gadget() { return Warsaw::GadgetKit::_duplicate(gk);}
  void append(Warsaw::Controller_ptr, const Babylon::String &);
  void run();
protected:
  Item make_item(const Babylon::String &);
private:
  Warsaw::ServerContext_var server;
  Warsaw::TextKit_var tk;
  Warsaw::DesktopKit_var dk;
  Warsaw::LayoutKit_var lk;
  Warsaw::ToolKit_var ttk;
  Warsaw::WidgetKit_var wk;
  Warsaw::FigureKit_var fk;
  Warsaw::CommandKit_var ck;
  Warsaw::ImageKit_var ik;
  Warsaw::GadgetKit_var gk;
  Warsaw::Graphic_var vbox;
  Warsaw::Choice_var  choice;
  list_t demos;
  Mapper *mapper;
  Warsaw::Color background;
  Warsaw::Graphic_var done;
  Warsaw::Graphic_var settings;
};

#endif
