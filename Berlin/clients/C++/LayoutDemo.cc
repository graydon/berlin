/*$Id: LayoutDemo.cc,v 1.9 2001/01/09 21:35:08 tobias Exp $
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

#include "LayoutDemo.hh"

using namespace Warsaw;

LayoutDemo::LayoutDemo(Application *a)
  : Demo(a)
{
  TextKit_var text = application->text();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();
  Graphic_var label1 = tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("first button")))), 0.0,0.0,0.0);
  Graphic_var label2 = tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("second button")))), 0.0,0.0,0.0);
  Graphic_var label3 = tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("third button")))), 0.0,0.0,0.0);
  
  Graphic_var hbox = layout->hbox();
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  Controller_var button1 = widget->button(label1, command1);
  hbox->append_graphic(Graphic_var(tool->rgb(button1, 1.0, 0.5, 0.5)));
  hbox->append_graphic(Graphic_var(layout->hglue(100., 500., 0.)));
  Controller_var button2 = widget->button(label2, command2);
  hbox->append_graphic(Graphic_var(tool->rgb(button2, 0.5, 1., 0.5)));
  hbox->append_graphic(Graphic_var(layout->hglue(100., 500., 0.)));
  Controller_var button3 = widget->button(label3, command3);
  hbox->append_graphic(Graphic_var(tool->rgb(button3, 0.5, 0.5, 1.)));
  Graphic_var margin = layout->margin_flexible(hbox, 100., 500., 100.);
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);
  Graphic_var demo = tool->frame(margin, 10., spec, true);
  Controller_var group = tool->group(demo);
  group->append_controller(button1);
  group->append_controller(button2);
  group->append_controller(button3);
  application->append(group, Babylon::String("layout demo"));
};
