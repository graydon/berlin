/*$Id: FocusDemo.cc,v 1.12 2001/01/09 21:35:08 tobias Exp $
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

#include <Warsaw/config.hh>
#include <Warsaw/Subject.hh>
#include <Warsaw/Choice.hh>
#include <Berlin/ObserverImpl.hh>
#include "FocusDemo.hh"

using namespace Warsaw;

class FocusDemo::Observer : public ObserverImpl
{
 public:
  virtual void update(const CORBA::Any &any)
    {
      Warsaw::Selection::Item *item;
      if (any >>= item)
	cout << "new selection" << endl;
    }
};

FocusDemo::FocusDemo(Application *a)
  : Demo(a), observer(new Observer)
{
  TextKit_var text = application->text();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ImageKit_var image = application->image();
  FigureKit_var figure = application->figure();
  ToolKit_var   tool = application->tool();
  WidgetKit_var widget = application->widget();
  Graphic_var      vbox = layout->vbox();
  Graphic_var     hbox1 = layout->hbox();
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::concav);
  /*
   * first group
   */
  Choice_var c1 = widget->toggle_choice();
  for (size_t i = 0; i != 5; i++)
    c1->append_item(Graphic_var(Graphic::_nil()));
  c1->attach(Observer_var(observer->_this()));
  /*
   * second group
   */
  Choice_var c2 = widget->checkbox_choice();
  for (size_t i = 0; i != 5; i++)
    c2->append_item(Graphic_var(Graphic::_nil()));
  c2->attach(Observer_var(observer->_this()));
  hbox1->append_graphic(Graphic_var(layout->margin(Graphic_var(tool->frame(Graphic_var(layout->margin(c1, 100.)),
									   20., spec, true)), 100.)));
  hbox1->append_graphic(Graphic_var(layout->margin(Graphic_var(tool->frame(Graphic_var(layout->margin(c2, 100.)),
									   20., spec, true)), 100.)));
  Graphic_var     hbox2 = layout->hbox();
  /*
   * third group
   */
  Choice_var c3 = widget->toggle_choice();
  for (size_t i = 0; i != 5; i++)
    c3->append_item(Graphic_var(Graphic::_nil()));
  c3->attach(Observer_var(observer->_this()));
  /*
   * fourth group
   */
  Choice_var c4 = widget->checkbox_choice();
  for (size_t i = 0; i != 5; i++)
    c4->append_item(Graphic_var(Graphic::_nil()));
  c4->attach(Observer_var(observer->_this()));
  hbox2->append_graphic(Graphic_var(layout->margin(Graphic_var(tool->frame(Graphic_var(layout->margin(c3, 100.)),
									   20., spec, true)), 100.)));
  hbox2->append_graphic(Graphic_var(layout->margin(Graphic_var(tool->frame(Graphic_var(layout->margin(c4, 100.)),
									   20., spec, true)), 100.)));
  vbox->append_graphic(hbox1);
  vbox->append_graphic(hbox2);
  Raster_var raster = image->create("marble.png");
  Graphic_var texture = figure->texture(vbox, raster);
  Controller_var gr = tool->group(texture);
  gr->append_controller(c1);
  gr->append_controller(c2);
  gr->append_controller(c3);
  gr->append_controller(c4);
  application->append(gr, Babylon::String("focus demo"));
};
