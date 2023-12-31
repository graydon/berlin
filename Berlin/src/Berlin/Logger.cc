//
// $Id: Logger.cc,v 1.4 1999/09/23 20:02:10 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// Copyright (C) 1998 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//

#include "Berlin/Logger.hh"

using namespace Prague;

bool         Logger::active[numGroups] = {false, false, false, false, false, false, false, false, false, false,
					  false, false, false, false, false, false, false, false};
const char * Logger::groupname[numGroups] = {"corba", "loader", "traversal", "thread", "main",
					     "agent", "message", "command", "subject", 
					     "observer", "text", "widget", "image", "figure",
					     "layout", "drawing", "desktop", "picking", "focus", "geometry"};
logbuf       Logger::buf(1024 * 64);
logstream    Logger::los(&buf);
EventLogger  Logger::events(256);
Time         Logger::start = Time::currentTime();
Mutex        Logger::mutex;

void Logger::dump(ostream &os)
{
  os << "Something went wrong. Here's a debugging log. \n"
     << "Please mail this output to bugs@berlin-consortium.org :\n\n";
  los.dump(os);
  os << "\n\nDetailed Trace:\n";
  events.dump(os);
}
