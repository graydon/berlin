/*$Id: Dispatcher.cc,v 1.5 1999/11/17 18:15:52 stefan Exp $
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

#include "Prague/Sys/Signal.hh"
#include "Prague/IPC/Dispatcher.hh"
#include <queue>
#include <algorithm>
#include <ctime>
#include <cerrno>
#include <unistd.h>
#include <sys/types.h>

using namespace Prague;

Dispatcher *Dispatcher::dispatcher = 0;
Mutex Dispatcher::singletonMutex;
Dispatcher::Cleaner Dispatcher::cleaner;

struct SignalNotifier : Signal::Notifier
{
  virtual void notify(int signum)
  {
    cerr << Signal::name(signum) << endl;
    exit(1);
  }
};

Dispatcher::Cleaner::~Cleaner()
{
//   MutexGuard guard(singletonMutex);
  delete dispatcher;
}

Dispatcher *Dispatcher::instance()
{
  MutexGuard guard(singletonMutex);
  if (!dispatcher) dispatcher = new Dispatcher;
  return dispatcher;
}

Dispatcher::Dispatcher()
  //. create a queue of up to 64 tasks 
  //. and a thread pool with 16 threads
  : notifier(new SignalNotifier),
    tasks(64),
    workers(tasks, acceptor, 4),
    server(&Dispatcher::dispatch, this)
{
  Signal::mask(Signal::pipe);
  Signal::set(Signal::hangup, notifier);
  Signal::set(Signal::interrupt, notifier);
  Signal::set(Signal::quit, notifier);
  Signal::set(Signal::illegal, notifier);
  Signal::set(Signal::abort, notifier);
  Signal::set(Signal::fpe, notifier);
  Signal::set(Signal::bus, notifier);
  //  Signal::set(Signal::segv, notifier);
  Signal::set(Signal::iotrap, notifier);
  Signal::set(Signal::terminate, notifier);
}

Dispatcher::~Dispatcher()
{
}

void Dispatcher::bind(int fd, Agent *agent, Agent::iomask_t mask)
{
  if (server.state() != Thread::running)
    {
      pipe(wakeup);
      rfds.set(wakeup[0]);
      server.start();
    }
//   Signal::Guard sguard(Signal::child);
  MutexGuard guard(mutex);
  if (find(agents.begin(), agents.end(), agent) == agents.end())
    agents.push_back(agent);
  if (mask & Agent::in)
    {
      if (mask & Agent::inready)
	{
	  wfds.set(fd);
	  if (wchannel.find(fd) == wchannel.end()) wchannel[fd] = task(fd, agent, Agent::inready);
	  else cerr << "Dispatcher::bind() : Error : file descriptor already in use" << endl;
	}
      if (mask & Agent::inexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = task(fd, agent, Agent::inexc);
	}
    }
  if (mask & Agent::out)
    {
      if (mask & Agent::outready)
	{
	  rfds.set(fd);
	  if (rchannel.find(fd) == rchannel.end()) rchannel[fd] = task(fd, agent, Agent::outready);
	  else cerr << "Dispatcher::bind() : Error : file descriptor already in use" << endl;
	}
      if (mask & Agent::outexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = task(fd, agent, Agent::outexc);
	}
    }
  if (mask & Agent::err)
    {
      if (mask & Agent::errready)
	{
	  rfds.set(fd);
	  if (rchannel.find(fd) == rchannel.end()) rchannel[fd] = task(fd, agent, Agent::errready);
	  else cerr << "Dispatcher::bind() : Error : file descriptor already in use" << endl;
	}
      if (mask & Agent::errexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = task(fd, agent, Agent::errexc);
	}
    }
}

void Dispatcher::release(int fd)
{
  MutexGuard guard(mutex);
  dictionary_t::iterator c;
  if ((c = rchannel.find(fd)) != rchannel.end())
    {
      rchannel.erase(c);
      rfds.clear(fd);
    }
  if ((c = wchannel.find(fd)) != wchannel.end())
    {
      wchannel.erase(c);
      wfds.clear(fd);
    }
  if ((c = xchannel.find(fd)) != xchannel.end())
    {
      xchannel.erase(c);
      xfds.clear(fd);
    }
}

void Dispatcher::release(Agent *agent)
{
//   Signal::Guard guard(Signal::child);
  MutexGuard guard(mutex);
  for (dictionary_t::iterator i = rchannel.begin(); i != rchannel.end(); i++)
    if ((*i).second.agent == agent) rchannel.erase(i);
  for (dictionary_t::iterator i = wchannel.begin(); i != wchannel.end(); i++)
    if ((*i).second.agent == agent) wchannel.erase(i);
  for (dictionary_t::iterator i = xchannel.begin(); i != xchannel.end(); i++)
    if ((*i).second.agent == agent) xchannel.erase(i);
  alist_t::iterator i = find(agents.begin(), agents.end(), agent);
  if (i != agents.end()) agents.erase(i);
}

void *Dispatcher::dispatch(void *X)
{
  Dispatcher *dispatcher = reinterpret_cast<Dispatcher *>(X);
  dispatcher->workers.start();
  do dispatcher->wait();
  while (true);
  return 0;
};

void Dispatcher::process(const task &t)
{
  if (t.agent->processIO(t.fd, t.mask)) activate(t);
}

void Dispatcher::deactivate(const task &t)
{
  switch (t.mask)
    {
    case Agent::inready: wfds.clear(t.fd); break;
    case Agent::outready:
    case Agent::errready: rfds.clear(t.fd); break;
    case Agent::inexc:
    case Agent::outexc:
    case Agent::errexc: xfds.clear(t.fd); break;
    default: break;
    }
}

void Dispatcher::activate(const task &t)
{
  switch (t.mask)
    {
    case Agent::inready: wfds.set(t.fd); break;
    case Agent::outready:
    case Agent::errready: rfds.set(t.fd); break;
    case Agent::inexc:
    case Agent::outexc:
    case Agent::errexc: xfds.set(t.fd); break;
    default: break;
    }
  char *c = "c";
  write(wakeup[1], c, 1);
}

void Dispatcher::wait()
{
  FdSet tmprfds = rfds;
  FdSet tmpwfds = wfds;
  FdSet tmpxfds = xfds;
  unsigned int fdsize = max(max(tmprfds.max(), tmpwfds.max()), tmpxfds.max()) + 1;
  int nsel = select(fdsize, tmprfds, tmpwfds, tmpxfds, 0);
  pthread_testcancel();
  if (nsel == -1)
    {
      if (errno == EINTR || errno == EAGAIN) errno = 0;
    }
  else if (nsel > 0 && fdsize)
    {
      tlist_t t;
      for (dictionary_t::iterator i = rchannel.begin(); i != rchannel.end(); i++)
	if (tmprfds.isset((*i).first))
	  {
	    t.push_back((*i).second);
	    deactivate((*i).second);
	  }
      for (dictionary_t::iterator i = wchannel.begin(); i != wchannel.end(); i++)
	if (tmpwfds.isset((*i).first))
	  {
	    t.push_back((*i).second);
	    deactivate((*i).second);
	  }
      for (dictionary_t::iterator i = xchannel.begin(); i != xchannel.end(); i++)
	if (tmpxfds.isset((*i).first))
	  {
	    t.push_back((*i).second);
	    deactivate((*i).second);
	  }
      if (tmprfds.isset(wakeup[0]))
	{
	  char c[1];
	  read(wakeup[0],c,1);
	}
      for (tlist_t::const_iterator i = t.begin(); i != t.end(); i++)
	tasks.push(*i);
    }
};
