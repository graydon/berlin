/*$Id: ColorDemo.cc,v 1.7 2001/01/09 21:35:08 tobias Exp $
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

#include "ColorDemo.hh"
#include <Berlin/Color.hh>

using namespace Warsaw;

ColorDemo::ColorDemo(Application *a)
  : Demo(a)
{
  LayoutKit_var layout = application->layout();
  ToolKit_var   tool = application->tool();
  WidgetKit_var widget = application->widget();
  TextKit_var text = application->text();
  CommandKit_var command = application->command();
  GadgetKit_var gadget = application->gadget();
  
  for (size_t i = 0; i != 6; ++i)
    adapter[i] = new Adapter(this, i);
  
  red = command->bvalue(0., 1., 0., .1, .5);
  green = command->bvalue(0., 1., 0., .1, .5);
  blue = command->bvalue(0., 1., 0., .1, .5);

  red->attach(Observer_var(adapter[0]->_this()));
  green->attach(Observer_var(adapter[1]->_this()));
  blue->attach(Observer_var(adapter[2]->_this()));

  hue = command->bvalue(0., 360., 0., 10., 50.);
  saturation = command->bvalue(0., 1., 0., 0.1, 0.1);
  value = command->bvalue(0., 1., 0., 0.1, 0.1);
  
  hue->attach(Observer_var(adapter[3]->_this()));
  saturation->attach(Observer_var(adapter[4]->_this()));
  value->attach(Observer_var(adapter[5]->_this()));

  Graphic_var hbox = layout->hbox();
  Graphic_var panel = layout->fixed_size(Graphic_var(Graphic::_nil()), 1000., 1000.);
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::inset);
  hbox->append_graphic(Graphic_var(layout->margin(Graphic_var(gadget->rgb(Graphic_var(tool->frame(panel, 20., spec, true)), red, green, blue)), 500.)));

  Graphic_var vbox = layout->vbox();

  Graphic_var hbox21 = layout->hbox();
  hbox21->append_graphic(Graphic_var(layout->hfill()));
  hbox21->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("RGB Color Model")))), 0., 0., 0.)));
  hbox21->append_graphic(Graphic_var(layout->hfill()));
  vbox->append_graphic(hbox21);

  Graphic_var hbox22 = layout->hbox();
  hbox22->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("R  ")))), 0., 0., 0.)));
  hbox22->append_graphic(Graphic_var(widget->slider(red, xaxis)));
  vbox->append_graphic(hbox22);
 
  Graphic_var hbox23 = layout->hbox();
  hbox23->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("G  ")))), 0., 0., 0.)));
  hbox23->append_graphic(Graphic_var(widget->slider(green, xaxis)));
  vbox->append_graphic(hbox23);

  Graphic_var hbox24 = layout->hbox();
  hbox24->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("B  ")))), 0., 0., 0.)));
  hbox24->append_graphic(Graphic_var(widget->slider(blue, xaxis)));
  vbox->append_graphic(hbox24);

  Graphic_var hbox25 = layout->hbox();
  hbox25->append_graphic(Graphic_var(layout->hfill()));
  hbox25->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("HSV Color Model")))), 0., 0., 0.)));
  hbox25->append_graphic(Graphic_var(layout->hfill()));
  vbox->append_graphic(hbox25);

  Graphic_var hbox26 = layout->hbox();
  hbox26->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("H  ")))), 0., 0., 0.)));
  hbox26->append_graphic(Graphic_var(widget->slider(hue, xaxis)));
  vbox->append_graphic(hbox26);
 
  Graphic_var hbox27 = layout->hbox();
  hbox27->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("S  ")))), 0., 0., 0.)));
  hbox27->append_graphic(Graphic_var(widget->slider(saturation, xaxis)));
  vbox->append_graphic(hbox27);

  Graphic_var hbox28 = layout->hbox();
  hbox28->append_graphic(Graphic_var(tool->rgb(Graphic_var(text->chunk(Unicode::to_CORBA(Babylon::String("V  ")))), 0., 0., 0.)));
  hbox28->append_graphic(Graphic_var(widget->slider(value, xaxis)));
  vbox->append_graphic(hbox28);
  hbox->append_graphic(vbox);
  Controller_var root = tool->group(Graphic_var(layout->margin(hbox, 100.)));
  application->append(root, Babylon::String("Color demo"));
}

void ColorDemo::adjust(Tag tag)
  //. this is a hack of a constraint solver.
  //. the reason to do this serialization is
  //. that we update the three values one at
  //. a time, though only after all three
  //. have been set the color is in a coherent
  //. state...
  //.  -stefan
{
  static bool processing = false;
  if (processing) return;
  processing = true;
  if (tag < 3) // set hsv sliders
    {
      Color color;
      color.red = red->value();
      color.green = green->value();
      color.blue = blue->value();
      Coord h, s, v;
      RGBtoHSV(color, h, s, v);
      //. the following three calls need to be atomic
      hue->value(h);
      saturation->value(s);
      value->value(v);
    }
  else // set rgb sliders
    {
      Color color;
      Coord h = hue->value();
      Coord s = saturation->value();
      Coord v = value->value();
      HSVtoRGB(h, s, v, color);
      // the following three calls need to be atomic
      red->value(color.red);
      green->value(color.green);
      blue->value(color.blue);
    }
  processing = false;
}
