/*$Id: terminal.cc,v 1.6 1999/09/10 20:57:38 gray Exp $
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
#include "Warsaw/WidgetKit.hh"
#include "Warsaw/ServerContext.hh"
#include "Berlin/ClientContextImpl.hh"
#include "Berlin/NameUtil.hh"
#include "Warsaw/StreamBuffer.hh"
#include "Berlin/Logger.hh"
#include <Prague/Sys/Signal.hh>
#include <unistd.h>
#include <iostream>
#include <fstream>

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

class Terminal : implements(Observer)
{
 public:
  Terminal(const char *file) : ofs(file) {}
  ~Terminal() {}
  virtual void update(Subject *subject, const CORBA::Any &)
    {
      StreamBuffer_var stream = StreamBuffer::_narrow(subject);
      StreamBuffer::Data_var data = stream->read();
      unsigned long length = data->length();
      for (unsigned long i = 0; i != length; i++)
	ofs.put(data[i]);
      ofs.flush();
    }
 private:
  ofstream ofs;
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
//   Logger::setall();
  Logger::set(Logger::subject);
  Logger::set(Logger::picking);
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");
  boa->impl_is_ready(0,1);

  ServerContextManager_ptr manager = getManager(orb);

  cerr << "[1/5] build client context" << endl;
  ClientContextImpl *ccontext = new ClientContextImpl;
  ccontext->_obj_is_ready(boa);

  cerr << "[2/5] retrieve server context" << endl;
  ServerContext_ptr sc = manager->newServerContext(ccontext->_this());

  WidgetKit_var wk = obtain(sc,WidgetKit);
  
  cout << "[3/5] got kits" << endl;

  StreamBuffer_var stream = wk->stream();
  Terminal *terminal = new Terminal("term.dat");
  terminal->_obj_is_ready(CORBA::BOA::getBOA());
  stream->attach(Observer_var(terminal->_this()));

  long count = 0;
  while (true)
    {
      sleep(1);
      count++;
      StreamBuffer::Data data;
      data.length(4);
      data[0] = 'l';
      data[1] = 'o';
      data[2] = 'g';
      data[3] = '\n';
      stream->write(data);
    }
}
