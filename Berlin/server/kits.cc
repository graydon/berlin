/*$Id: kits.cc,v 1.7 2001/04/18 06:07:26 stefan Exp $
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

#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Signal.hh>
#include <Prague/Sys/Profiler.hh>
#include <Prague/Sys/Timer.hh>
#include <Prague/Sys/GetOpt.hh>
#include <Prague/Sys/Path.hh>
#include <Prague/Sys/User.hh>
#include <Warsaw/config.hh>
#include <Warsaw/resolve.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/DrawingKit.hh>
#include <Berlin/ScreenImpl.hh>
#include <Berlin/ScreenManager.hh>
#include <Berlin/ServerImpl.hh>
#include <Berlin/Logger.hh>
#include <Berlin/DesktopImpl.hh>
#include <Berlin/RCManager.hh>
#include <fstream>

#ifdef RC_PREFIX
const std::string prefix = RC_PREFIX;
#else
const std::string prefix = "";
#endif

#ifdef VERSION
const std::string version = VERSION;
#else
const std::string version = "unknown";
#endif

using namespace Prague;

int main(int argc, char **argv)
{
  GetOpt getopt(argv[0], "a module inspection tool");
  getopt.add('h', "help", GetOpt::novalue, "help message");
  getopt.add('v', "version", GetOpt::novalue, "version number");
  getopt.add('r', "resource", GetOpt::mandatory, "the resource file to load");
  size_t argo = getopt.parse(argc, argv);
  argc -= argo;
  argv += argo;
  std::string value;
  getopt.get("version", &value);
  if (value == "true") { std::cout << "version is " << version << std::endl; exit(0);}
  value = "";
  getopt.get("help", &value);
  if (value == "true") { getopt.usage(); exit(0);}
  value = "";
  getopt.get("resource", &value);
  if (!value.empty()) RCManager::read(Prague::Path::expand_user(value));

  ServerImpl *server = ServerImpl::instance();

  Prague::Path path = RCManager::get_path("modulepath");
  for (Prague::Path::iterator i = path.begin(); i != path.end(); ++i)
    server->scan(*i);

  ServerImpl::FactoryList listing = server->list();
  for (ServerImpl::FactoryList::iterator i = listing.begin(); i != listing.end(); ++i)
    {
      std::cout << (*i).first << " supports :\n";
      Warsaw::Kit::Property *begin = (*i).second->get_buffer();
      Warsaw::Kit::Property *end = (*i).second->get_buffer() + (*i).second->length();
      for (Warsaw::Kit::Property *p = begin; p != end; ++p)
	std::cout << (*p).name << " : " << (*p).value << '\n';
    }
}
