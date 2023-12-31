/*$Id: ThreadPool.hh,v 1.4 1999/11/16 02:15:20 stefan Exp $
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
#ifndef _ThreadPool_hh
#define _ThreadPool_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>
#include <vector>

namespace Prague
{

template <class Task, class Acceptor, class Handler>
class ThreadPool
{
  typedef vector<Thread *> tlist_t;
public:
  ThreadPool(Thread::Queue<Task> &t, Acceptor &a, size_t s) : tasks(t), acceptor(a), threads(s, 0)
    {
      for (tlist_t::iterator i = threads.begin(); i != threads.end(); i++)
	(*i) = new Thread(run, this);
    }
  ~ThreadPool()
  {
    for (tlist_t::iterator i = threads.begin(); i != threads.end(); i++) delete *i;
  }
  void start() { for (tlist_t::iterator i = threads.begin(); i != threads.end(); i++) (*i)->start();}
private:
  static void *run(void *X)
    {
      ThreadPool *pool = reinterpret_cast<ThreadPool *>(X);
      while (1)
	{
	  Task task = pool->tasks.pop();
	  Handler *handler = pool->acceptor.consume(task);
	  handler->process();
	  delete handler;
	  Thread::testcancel();
	}
    }
  Thread::Queue<Task> &tasks;
  Acceptor &acceptor;
  tlist_t threads;
};

};

#endif /* _ThreadPool_hh */
