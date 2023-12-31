/*$Id: TransformDemo.cc,v 1.2 1999/11/30 20:03:51 tobias Exp $
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

#include "TransformDemo.hh"
#include <Warsaw/Transform.hh>
#include <Warsaw/Image.hh>
#include <Warsaw/Transformator.hh>

TransformDemo::TransformDemo(Application *a)
  : Demo(a)
{
  ImageKit_var image = application->image();
  CommandKit_var command = application->command();
  LayoutKit_var layout = application->layout();
  WidgetKit_var widget = application->widget();
  FigureKit_var figure = application->figure();
  Color red = {1.0, 0.5, 0.5, 1.0};
  Color green = {0.5, 1.0, 0.5, 1.0};
  Color blue = {0.5, 0.5, 1.0, 1.0};
  
  Command_var command1 = command->log("hello World 1");
  Command_var command2 = command->log("hello World 2");
  Command_var command3 = command->log("hello World 3");
  
  Graphic::Requisition req;
  req.x.defined = true;
  req.x.minimum = req.x.natural = req.x.maximum = 100.;
  req.y.align = 0.;
  req.y.defined = true;
  req.y.minimum = req.y.natural = req.y.maximum = 20.;
  req.y.align = 0.;
  
  Raster_var raster = image->create("raster.png");
  Image_var  im = figure->pixmap(raster);
  
  Graphic_var hbox = layout->hbox();
  hbox->append(Graphic_var(widget->pushButton(im, red, command1)));
  hbox->append(Graphic_var(widget->pushButton(im, green, command2)));
  hbox->append(Graphic_var(widget->pushButton(im, blue, command3)));
  Transformator_var transformator = figure->projection(hbox);
  transformator->transformation()->rotate(45., zaxis);

  application->append(Graphic_var(layout->halign(transformator, 0.)),
		      Unicode::String("transformation demo"));
};
