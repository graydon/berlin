/*$Id: Console.cc,v 1.4 2001/04/23 21:13:39 tobias Exp $
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

#include "Berlin/Console.hh"

#if defined(CONSOLE_GGI)
#  include "GGI.cc"

GGIConsole *Console::t = 0;

#elif defined(CONSOLE_SDL)
#  include "SDL.cc"

SDLConsole *Console::t = 0;

#elif defined(CONSOLE_CAVELIB)
#  include "CAVE.cc"

CAVEConsole *Console::t = 0;

#elif defined(CONSOLE_GLUT)
#  include "GLUT.cc"

GLUTConsole *Console::t = 0;

#elif defined(CONSOLE_DIRECTFB)
#  include "DirectFB.cc"

DirectFBConsole *Console::t = 0;

#else
#  warning "no console type defined"
#endif

Console::Reaper Console::reaper;
