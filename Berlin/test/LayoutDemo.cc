/*$Id: LayoutDemo.cc,v 1.2 1999/11/30 20:03:51 tobias Exp $
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

#include "LayoutDemo.hh"

LayoutDemo::LayoutDemo(Application *a)
  : Demo(a)
{
  TextKit_var text = application->text();
  Text::Font_var font = application->font();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  WidgetKit_var widget = application->widget();
  Graphic_var label1 =
    text->chunk(Unicode::toCORBA(Unicode::String("first button")), font);
  Graphic_var label2 =
    text->chunk(Unicode::toCORBA(Unicode::String("second button")), font);
  Graphic_var label3 =
    text->chunk(Unicode::toCORBA(Unicode::String("third button")), font);
  
  Color red = {1.0, 0.5, 0.5, 1.0};
  Color green = {0.5, 1.0, 0.5, 1.0};
  Color blue = {0.5, 0.5, 1.0, 1.0};
  Color gray = {0.5, 0.5, 0.5, 1.0};
  
  Graphic_var hbox = layout->hbox();
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  Graphic_var button = widget->pushButton(label1, red, command1);
  hbox->append(button);
  hbox->append(Graphic_var(layout->hglue(10., 50., 0.)));
  button = widget->pushButton(label2, green, command2);
  hbox->append(button);
  hbox->append(Graphic_var(layout->hglue(10., 50., 0.)));
  button = widget->pushButton(label3, blue, command3);
  hbox->append(button);
  Graphic_var demo =
    widget->outset(Graphic_var(layout->marginFlexible(hbox, 10., 50., 10.)),
		   gray,
		   true);
  application->append(demo, Unicode::String("layout demo"));
};
