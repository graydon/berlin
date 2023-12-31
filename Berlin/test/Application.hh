/*$Id: Application.hh,v 1.2 1999/11/30 20:03:51 tobias Exp $
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
#ifndef _Application_hh
#define _Application_hh

#include "Warsaw/config.hh"
#include "Warsaw/TextKit.hh"
#include "Warsaw/LayoutKit.hh"
#include "Warsaw/WidgetKit.hh"
#include "Warsaw/FigureKit.hh"
#include "Warsaw/CommandKit.hh"
#include "Warsaw/DesktopKit.hh"
#include "Warsaw/ImageKit.hh"
#include "Warsaw/Button.hh"
#include "Warsaw/ServerContext.hh"
#include "Berlin/ClientContextImpl.hh"
#include "Berlin/NameUtil.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"
#include <Prague/Sys/Signal.hh>
#include <Warsaw/Unicode.hh>
#include <unistd.h>
#include <iostream>
#include <vector>

class Application
{
  struct item
  {
    item(Controller_ptr c, Command_ptr m)
      : toggle(Controller::_duplicate(c)), map(Command::_duplicate(m)) {}
    Controller_var toggle;
    Command_var    map;
  };
  typedef vector<item> list_t;
  class Mapper : implements(Command)
  {
  public:
    Mapper(Application::list_t &e) : examples(e) {}
    virtual void execute(const CORBA::Any &);
  private:
    Application::list_t &examples;
  };
  friend class Mapper;
public:
  Application(ServerContextManager_ptr);
  TextKit_ptr text() { return TextKit::_duplicate(tk);}
  DesktopKit_ptr desktop() { return DesktopKit::_duplicate(dk);}
  LayoutKit_ptr layout() { return LayoutKit::_duplicate(lk);}
  WidgetKit_ptr widget() { return WidgetKit::_duplicate(wk);}
  FigureKit_ptr figure() { return FigureKit::_duplicate(fk);}
  CommandKit_ptr command() { return CommandKit::_duplicate(ck);}
  ImageKit_ptr image() { return ImageKit::_duplicate(ik);}
  Text::Font_ptr font() { return Text::Font::_duplicate(f);}
  void append(Graphic_ptr, const Unicode::String &);
  void run();
protected:
private:
  Impl_var<ClientContextImpl> client;
  ServerContext_var server;
  TextKit_var tk;
  DesktopKit_var dk;
  LayoutKit_var lk;
  WidgetKit_var wk;
  FigureKit_var fk;
  CommandKit_var ck;
  ImageKit_var ik;
  Text::Font_var f;
  Graphic_var vbox;
  TelltaleConstraint_var exclusive;
  list_t examples;
  Impl_var<Mapper> mapper;
  Color background;
  Color foreground;
  Graphic_var done;
};

#endif /* _Application_hh */
