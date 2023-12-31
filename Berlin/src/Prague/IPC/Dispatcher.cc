/*$Id: Dispatcher.cc,v 1.15 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Prague/Sys/Tracer.hh"
#include "Prague/IPC/Dispatcher.hh"
#include <queue>
#include <algorithm>
#include <ctime>
#include <cerrno>
#include <sys/types.h>

using namespace Prague;

Dispatcher *Dispatcher::dispatcher = 0;
Mutex Dispatcher::singletonMutex;
Dispatcher::Cleaner Dispatcher::cleaner;

struct SignalNotifier : Signal::Notifier
{
  virtual void notify(int signum)
  {
    std::cerr << Signal::name(signum) << std::endl;
    exit(1);
  }
};

Dispatcher::Cleaner::~Cleaner()
{
//   Prague::Guard<Mutex> guard(singletonMutex);
  delete dispatcher;
}

Dispatcher *Dispatcher::instance()
{
  Prague::Guard<Mutex> guard(singletonMutex);
  if (!dispatcher) dispatcher = new Dispatcher;
  return dispatcher;
}

Dispatcher::Dispatcher()
  //. create a queue of up to 64 tasks 
  //. and a thread pool with 16 threads
  : notifier(new SignalNotifier),
    tasks(64),
    workers(tasks, acceptor, 4),
    server(&Dispatcher::run, this)
{
  Signal::mask(Signal::pipe);
//   Signal::set(Signal::hangup, notifier);
//   Signal::set(Signal::interrupt, notifier);
//   Signal::set(Signal::quit, notifier);
//   Signal::set(Signal::illegal, notifier);
//   Signal::set(Signal::abort, notifier);
//   Signal::set(Signal::fpe, notifier);
//   Signal::set(Signal::bus, notifier);
//   //  Signal::set(Signal::segv, notifier);
//   Signal::set(Signal::iotrap, notifier);
//   Signal::set(Signal::terminate, notifier);
}

Dispatcher::~Dispatcher()
{
}

void Dispatcher::bind(Agent *agent, int fd, Agent::iomask mask)
{
  Trace trace("Dispatcher::bind");
  if (server.state() != Thread::running)
    {
      pipe(wakeup);
      rfds.set(wakeup[0]);
      server.start();
    }
  Prague::Guard<Mutex> guard(mutex);
  if (find(agents.begin(), agents.end(), agent) == agents.end())
    {
      agents.push_back(agent);
      agent->add_ref();
    }
  if (mask & Agent::in)
    {
      if (mask & Agent::inready)
	{
	  wfds.set(fd);
	  if (wchannel.find(fd) == wchannel.end()) wchannel[fd] = new task(fd, agent, Agent::inready);
	  else std::cerr << "Dispatcher::bind() : Error : file descriptor already in use" << std::endl;
	}
      if (mask & Agent::inexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = new task(fd, agent, Agent::inexc);
	}
    }
  if (mask & Agent::out)
    {
      if (mask & Agent::outready)
	{
	  rfds.set(fd);
	  if (rchannel.find(fd) == rchannel.end()) rchannel[fd] = new task(fd, agent, Agent::outready);
	  else std::cerr << "Dispatcher::bind() : Error : file descriptor already in use" << std::endl;
	}
      if (mask & Agent::outexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = new task(fd, agent, Agent::outexc);
	}
    }
  if (mask & Agent::err)
    {
      if (mask & Agent::errready)
	{
	  rfds.set(fd);
	  if (rchannel.find(fd) == rchannel.end()) rchannel[fd] = new task(fd, agent, Agent::errready);
	  else std::cerr << "Dispatcher::bind() : Error : file descriptor already in use" << std::endl;
	}
      if (mask & Agent::errexc)
	{
	  xfds.set(fd);
	  if (xchannel.find(fd) == xchannel.end()) xchannel[fd] = new task(fd, agent, Agent::errexc);
	}
    }
  notify();
}

void Dispatcher::release(Agent *agent, int fd)
{
  Trace trace("Dispatcher::release");
  /*
   * release file descriptors
   */
  Prague::Guard<Mutex> guard(mutex);
  for (repository_t::iterator i = rchannel.begin(); i != rchannel.end(); i++)
    if ((*i).second->agent == agent && (fd == -1 || fd == (*i).second->fd))
      {
	deactivate((*i).second);
	(*i).second->released = true;
	rchannel.erase(i);
      }
  for (repository_t::iterator i = wchannel.begin(); i != wchannel.end(); i++)
    if ((*i).second->agent == agent && (fd == -1 || fd == (*i).second->fd))
      {
	deactivate((*i).second);
	(*i).second->released = true;
	wchannel.erase(i);
      }
  for (repository_t::iterator i = xchannel.begin(); i != xchannel.end(); i++)
    if ((*i).second->agent == agent && (fd == -1 || fd == (*i).second->fd))
      {
	deactivate((*i).second);
	(*i).second->released = true;
	xchannel.erase(i);
      }
  /*
   * release Agent if no more file descriptors left
   */
  for (repository_t::iterator i = rchannel.begin(); i != rchannel.end(); i++)
    if ((*i).second->agent == agent) return;
  for (repository_t::iterator i = wchannel.begin(); i != wchannel.end(); i++)
    if ((*i).second->agent == agent) return;
  for (repository_t::iterator i = xchannel.begin(); i != xchannel.end(); i++)
    if ((*i).second->agent == agent) return;

  alist_t::iterator i = find(agents.begin(), agents.end(), agent);
  if (i != agents.end())
    {
      agents.erase(i);
      agent->remove_ref();
    }
}

void *Dispatcher::run(void *X)
{
  Dispatcher *dispatcher = reinterpret_cast<Dispatcher *>(X);
  dispatcher->workers.start();
  do dispatcher->wait();
  while (true);
  return 0;
};

void Dispatcher::dispatch(task *t)
{
  deactivate(t);
  tasks.push(t);
}

void Dispatcher::process(task *t)
{
  Trace trace("Dispatcher::process");
  Agent *agent = t->agent;
  // save the agent from being deleted while it processes i/o
  agent->add_ref();
  bool done = !agent->process(t->fd, t->mask);
  agent->remove_ref();
  // now look whether the agent is released and the task should be deleted
  Prague::Guard<Mutex> guard(mutex);
  if (!done)
    {
      if (t->released) delete t;
      else activate(t);
    }
}

void Dispatcher::deactivate(task *t)
{
  switch (t->mask)
    {
    case Agent::inready: wfds.clear(t->fd); break;
    case Agent::outready:
    case Agent::errready: rfds.clear(t->fd); break;
    case Agent::inexc:
    case Agent::outexc:
    case Agent::errexc: xfds.clear(t->fd); break;
    default: break;
    }
}

void Dispatcher::activate(task *t)
{
  switch (t->mask)
    {
    case Agent::inready: wfds.set(t->fd); break;
    case Agent::outready:
    case Agent::errready: rfds.set(t->fd); break;
    case Agent::inexc:
    case Agent::outexc:
    case Agent::errexc: xfds.set(t->fd); break;
    default: break;
    }
  notify();
}

void Dispatcher::wait()
{
  Trace trace("Dispatcher::wait");
  FdSet tmprfds = rfds;
  FdSet tmpwfds = wfds;
  FdSet tmpxfds = xfds;
  unsigned int fdsize = std::max(std::max(tmprfds.max(), tmpwfds.max()), tmpxfds.max()) + 1;
  int nsel = select(fdsize, tmprfds, tmpwfds, tmpxfds, 0);
  pthread_testcancel();
  if (nsel == -1)
    {
      if (errno == EINTR || errno == EAGAIN) errno = 0;
    }
  else if (nsel > 0 && fdsize)
    {
      Prague::Guard<Mutex> guard(mutex);
      for (repository_t::iterator i = rchannel.begin(); i != rchannel.end(); i++)
	if (tmprfds.isset((*i).first))
	  dispatch((*i).second);
      for (repository_t::iterator i = wchannel.begin(); i != wchannel.end(); i++)
	if (tmpwfds.isset((*i).first))
	  dispatch((*i).second);
      for (repository_t::iterator i = xchannel.begin(); i != xchannel.end(); i++)
	if (tmpxfds.isset((*i).first))
	  dispatch((*i).second);
      if (tmprfds.isset(wakeup[0]))
	{
	  char c[1];
	  read(wakeup[0], c, 1);
	}
    }
}
