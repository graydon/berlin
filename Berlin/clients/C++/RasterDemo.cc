/*$Id: RasterDemo.cc,v 1.6 2001/01/09 21:35:08 tobias Exp $
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

#include "RasterDemo.hh"
#include <Warsaw/Image.hh>

using namespace Warsaw;

RasterDemo::RasterDemo(Application *a)
  : Demo(a)
{
  ImageKit_var image = application->image();
  FigureKit_var figure = application->figure();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();
  
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  
  Raster_var raster = image->create("png.png");
  Image_var  im = figure->pixmap(raster);
  
  Graphic_var hbox = layout->hbox();
  hbox->append_graphic(Graphic_var(widget->button(im, command1)));
  hbox->append_graphic(Graphic_var(widget->button(im, command2)));
  hbox->append_graphic(Graphic_var(widget->button(im, command3)));
  Controller_var group = tool->group(hbox);

  application->append(group, Babylon::String("raster demo"));
};
