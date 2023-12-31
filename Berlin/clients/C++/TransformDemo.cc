/*$Id: TransformDemo.cc,v 1.7 2001/01/09 21:35:08 tobias Exp $
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

#include "TransformDemo.hh"
#include <Warsaw/Transform.hh>
#include <Warsaw/Image.hh>

using namespace Warsaw;

TransformDemo::TransformDemo(Application *a)
  : Demo(a)
{
  ImageKit_var image = application->image();
  CommandKit_var command = application->command();
  LayoutKit_var layout = application->layout();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();
  FigureKit_var figure = application->figure();
  
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  
  Raster_var raster = image->create("../etc/PNG/png.png");
  Image_var  im = figure->pixmap(raster);
  
  Graphic_var hbox = layout->hbox();
  hbox->append_graphic(Graphic_var(widget->button(im, command1)));
  hbox->append_graphic(Graphic_var(widget->button(im, command2)));
  hbox->append_graphic(Graphic_var(widget->button(im, command3)));
  Graphic_var transformer = figure->transformer(hbox);
  Transform_var(transformer->transformation())->rotate(45., zaxis);
  Graphic_var root = layout->halign(transformer, 0.);
  Controller_var group = tool->group(root);
  application->append(group, Babylon::String("transformation demo"));
};
