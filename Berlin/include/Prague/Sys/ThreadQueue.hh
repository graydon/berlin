/*$Id: ThreadQueue.hh,v 1.1 1999/07/21 20:48:28 gray Exp $
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
#ifndef _ThreadQueue_hh
#define _ThreadQueue_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Time.hh>
#include <queue>

namespace Prague
{

template <class T>
class Thread::Queue : private priority_queue<T>
{
  typedef priority_queue<T> rep_type;
public:
  Queue(size_t capacity) : free(capacity) {}
  void push(const T &t)
    {
      free.wait();
      MutexGuard guard(mutex);
      rep_type::push(t);
      tasks.post();
    }
  T pop()
    {
      tasks.wait();
      MutexGuard guard(mutex);
      T t = rep_type::top();
      rep_type::pop();
      free.post();
      return t;
    }
  size_t size() { MutexGuard guard(mutex); return rep_type::size();}
protected:
private:
  Semaphore tasks;
  Semaphore free;
  Mutex mutex;
};

};

#endif /* _ThreadQueue_hh */
