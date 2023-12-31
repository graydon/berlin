/*$Id: TextDemo.cc,v 1.5 2001/01/09 21:35:08 tobias Exp $
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

#include "TextDemo.hh"

using namespace Warsaw;

TextDemo::TextDemo(Application *a)
  : Demo(a)
{
  TextKit_var text = application->text();
  LayoutKit_var layout = application->layout();
  CommandKit_var command = application->command();
  ToolKit_var tool = application->tool();
  WidgetKit_var widget = application->widget();
  Babylon::Char chars[] = {
    0x004d, 0x0061, 0x0067, 0x0079, 0x0061, 0x0072, 0x0020, 0x0420,
    0x0443, 0x0441, 0x0441, 0x043a, 0x0438, 0x0439, 0x0020, 0x0395,
    0x039b, 0x039b, 0x0397, 0x039d, 0x0399, 0x039a, 0x0391, 0x0020,
    0x65e5, 0x672c, 0x8a9e, 0x0020, 0x4e2d, 0x6587, 0x0020, 0xd55c,
    0xad6d, 0xc5b4
  };

  Babylon::String str(34, chars);
  Graphic_var txt = text->chunk(Unicode::to_CORBA(str));
  Controller_var group = tool->group(Graphic_var(tool->rgb(txt, 0.7, 0.8, 1.0)));
  application->append(group, Babylon::String("text demo"));
};
