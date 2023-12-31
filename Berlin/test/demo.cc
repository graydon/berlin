 /*$Id: demo.cc,v 1.1 1999/11/07 22:02:05 stefan Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/ServerContext.hh"
#include "Application.hh"
#include "LayoutDemo.hh"
#include "TextDemo.hh"
#include "RasterDemo.hh"
#include "TransformDemo.hh"
#include "LogoDemo.hh"

using namespace Prague;

bool running;

struct Dump : Signal::Notifier 
{
  void notify(int signal)
    {
      if (signal != Signal::interrupt)
	{
	  Logger::dump(cerr);
	  exit(-1);
	}
      else running = false;
    }
};

static ServerContextManager_ptr getManager(CORBA::ORB_ptr orb)
{
  CORBA::Object_ptr obj = lookup(orb, ServerContextManager_IntfRepoID);
  ServerContextManager_ptr manager = ServerContextManager::_narrow(obj);
  return manager;
};

int main(int argc, char **argv)
{
  Dump *dump = new Dump;
  Signal::set(Signal::abort, dump);
  Signal::set(Signal::segv, dump);
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");
  boa->impl_is_ready(0,1);

  ServerContextManager_ptr manager = getManager(orb);

  Application *application = new Application(manager);
  LayoutDemo *layout = new LayoutDemo(application);
  TextDemo *text = new TextDemo(application);
  RasterDemo *raster = new RasterDemo(application);
  TransformDemo *transform = new TransformDemo(application);
  LogoDemo *logo = new LogoDemo(application);
  application->run();
  delete logo;
  delete transform;
  delete raster;
  delete text;
  delete layout;
  delete application;
}
