/*$Id: TermDemo.cc,v 1.4 2001/01/09 21:35:08 tobias Exp $
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

#include "TermDemo.hh"
#include <fstream>

using namespace Prague;
using namespace Warsaw;

TermDemo::TermDemo(Application *a)
  : Demo(a)
{
  TextKit_var text = application->text();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();

  Controller_var terminal = widget->terminal();
  Controller_var scrollable = widget->scrollable(Graphic_var(tool->rgb(terminal, 0., 0., 0.)));
  scrollable->append_controller(terminal);
  Controller_var group = tool->group(Graphic_var(layout->fixed_size(scrollable, 4000., 3000.)));
  group->append_controller(scrollable);
  application->append(group, Babylon::String("terminal demo"));
};
