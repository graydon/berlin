/*$Id: DocDemo.cc,v 1.5 2001/04/18 06:07:25 stefan Exp $
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

#include "DocDemo.hh"
#include <fstream>

using namespace Warsaw;

DocDemo::DocDemo(Application *a)
  : Demo(a)
{
  TextKit_var text = application->text();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();

  Graphic_var column = layout->vbox();
  std::string file = "/etc/passwd";
  std::ifstream ifs(file.c_str());
  while (ifs)
    {
      std::string buf;
      getline(ifs, buf);
      Graphic_var line;
      if (buf.length())	line = text->chunk(Unicode::to_CORBA(Babylon::String(buf)));
      else line = text->strut();
      column->append_graphic(line);
    }
  Controller_var page = widget->scrollable(Graphic_var(tool->rgb(column, 0., 0., .0)));
  Controller_var root = tool->group(Graphic_var(layout->fixed_size(page, 3000., 2000.)));
  root->append_controller(page);
  application->append(root, Babylon::String("multiline text demo"));
};
