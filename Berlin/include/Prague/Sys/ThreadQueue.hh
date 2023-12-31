/*$Id: ThreadQueue.hh,v 1.4 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _ThreadQueue_hh
#define _ThreadQueue_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Time.hh>
#include <queue>

namespace Prague
{

template <class T>
class Thread::Queue : private std::priority_queue<T>
{
  typedef std::priority_queue<T> rep_type;
public:
  Queue(size_t capacity) : free(capacity) {}
  void push(const T &t)
    {
      free.wait();
      Prague::Guard<Mutex> guard(mutex);
      rep_type::push(t);
      tasks.post();
    }
  T pop()
    {
      tasks.wait();
      Prague::Guard<Mutex> guard(mutex);
      T t = rep_type::top();
      rep_type::pop();
      free.post();
      return t;
    }
  size_t size() { Prague::Guard<Mutex> guard(mutex); return rep_type::size();}
protected:
private:
  Semaphore tasks;
  Semaphore free;
  Mutex mutex;
};

};

#endif /* _ThreadQueue_hh */
