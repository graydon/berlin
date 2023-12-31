/*$Id: RasterDemo.cc,v 1.2 1999/11/30 20:03:51 tobias Exp $
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

#include "RasterDemo.hh"
#include "Warsaw/Image.hh"

RasterDemo::RasterDemo(Application *a)
  : Demo(a)
{
  ImageKit_var image = application->image();
  FigureKit_var figure = application->figure();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  WidgetKit_var widget = application->widget();
  Color red = {1.0, 0.5, 0.5, 0.5};
  Color green = {0.5, 1.0, 0.5, 0.5};
  Color blue = {0.5, 0.5, 1.0, 0.5};
  
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  
  Raster_var raster = image->create("raster.png");
  Image_var  im = figure->pixmap(raster);
  
  Graphic_var hbox = layout->hbox();
  hbox->append(Graphic_var(widget->pushButton(im, red, command1)));
  hbox->append(Graphic_var(widget->pushButton(im, green, command2)));
  hbox->append(Graphic_var(widget->pushButton(im, blue, command3)));

  application->append(hbox, Unicode::String("raster demo"));
};
