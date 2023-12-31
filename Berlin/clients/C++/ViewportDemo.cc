/*$Id: ViewportDemo.cc,v 1.8 2001/01/09 21:35:08 tobias Exp $
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

#include "ViewportDemo.hh"
#include "Warsaw/Image.hh"
#include "Warsaw/Viewport.hh"

using namespace Warsaw;

ViewportDemo::ViewportDemo(Application *a)
  : Demo(a)
{
  LayoutKit_var layout = application->layout();
  ToolKit_var   tool = application->tool();
  WidgetKit_var widget = application->widget();
  ImageKit_var image = application->image();
  FigureKit_var figure = application->figure();

  Raster_var raster = image->create("landscape.png");
  Image_var pixmap = figure->pixmap(raster);
  Layout::Viewport_var viewport = layout->scrollable(pixmap);
  Controller_var panner = widget->panner(BoundedRange_var(viewport->adjustment(xaxis)), BoundedRange_var(viewport->adjustment(yaxis)));
  Controller_var xscroller = widget->scrollbar(BoundedRange_var(viewport->adjustment(xaxis)), xaxis);
  Controller_var yscroller = widget->scrollbar(BoundedRange_var(viewport->adjustment(yaxis)), yaxis);
  Graphic_var hbox1 = layout->hbox();
  hbox1->append_graphic(viewport);
  hbox1->append_graphic(yscroller);
  Graphic_var hbox2 = layout->hbox();
  hbox2->append_graphic(xscroller);
  hbox2->append_graphic(Graphic_var(layout->fixed_size(Graphic_var(Graphic::_nil()), 200., 200.)));
  Graphic_var vbox1 = layout->vbox();
  vbox1->append_graphic(hbox1);
  vbox1->append_graphic(hbox2);
  Graphic_var margin = layout->margin_flexible(panner, 500., 2000., 500.);
  Graphic_var hbox = layout->hbox();
  hbox->append_graphic(margin);
  hbox->append_graphic(vbox1);
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);
  Graphic_var background = tool->frame(hbox, 10., spec, true);
  Controller_var group  = tool->group(background);
  group->append_controller(panner);
  group->append_controller(xscroller);
  group->append_controller(yscroller);

  application->append(group, Babylon::String("viewport demo"));
}
