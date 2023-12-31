/*$Id: Thread.cc,v 1.13 2001/02/06 19:46:17 tobias Exp $
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
#include "Prague/Sys/Thread.hh"
#include <unistd.h>

using namespace Prague;

pthread_key_t Thread::self_key;
pthread_key_t Thread::id_key;
Mutex         Thread::id_mutex;
unsigned long Thread::counter = 0;
Thread       *Thread::main = 0;
Thread::Guard Thread::guard;

void id_deallocator(void *id) { delete reinterpret_cast<unsigned long *>(id);}

//Mutex::Mutex(type t)
//{
//  pthread_mutexattr_t attr;
//  pthread_mutexattr_init(&attr);
//  pthread_mutex_init(&impl, &attr);
//  pthread_mutexattr_destroy(&attr);
//}

Thread::Guard::Guard()
{
  pthread_key_create(&Thread::self_key, 0);
  pthread_key_create(&Thread::id_key, id_deallocator);
  pthread_t pt = pthread_self();
  Thread::main = new Thread(pt);
  pthread_setspecific(Thread::self_key, Thread::main);
  id_mutex.lock();
  pthread_setspecific(Thread::id_key, new unsigned long (counter++));
  id_mutex.unlock();
}

Thread::Guard::~Guard()
{
  Thread::main->_state = canceled;
  delete Thread::main;
}

Thread::Thread(proc pp, void *a, priority_t prio)
  : p(pp), arg(a), _priority(prio), _state(ready), detached(false)
{
}

Thread::~Thread()
{
  if (this != self())
    {
      cancel();
      if (!detached) join(0);
    }
}

void Thread::start() throw (Exception)
{
  Prague::Guard<Mutex> guard(mutex);
  if ((_state) != ready) throw Exception("thread already running");
  if (pthread_create(&thread, 0, &start, this) != 0) throw Exception("can't create thread");
  else _state = running;
}

void Thread::join(void **status) throw (Exception)
{
  {
    Prague::Guard<Mutex> guard(mutex);
    if (_state == joined || _state == canceled || _state == ready) return;
    if (this == self()) throw Exception("can't join thread 'self'");
    if (detached) throw Exception("can't join detached thread");
    _state = joined;
  }
  void *s;
  pthread_join(thread, &s);
  if (s == PTHREAD_CANCELED)
    {
      Prague::Guard<Mutex> guard(mutex);
      _state = canceled;
    }
  if (status) *status = s;
}

void Thread::cancel()
{
  if (this != self() && state() == running)
    pthread_cancel(thread);
}

void Thread::detach()
{
  mutex.lock();
  detached = true;
  mutex.unlock();
  pthread_detach(thread);
}

void Thread::exit(void *r)
{
  Thread *me = self();
  if (me)
    {
      Prague::Guard<Mutex> guard(me->mutex);  
      me->_state = terminated;
    }
  pthread_exit(r);
}

void *Thread::start(void *X)
{
  Thread *thread = reinterpret_cast<Thread *>(X);
  pthread_setspecific(self_key, thread);
  id_mutex.lock();
  pthread_setspecific(id_key, new unsigned long (counter++));
  id_mutex.unlock();
  void *ret = thread->p(thread->arg);
  return ret;
}

bool Thread::delay(const Time &time)
{
  Time t(time);
  return select(0, 0, 0, 0, &t) == 0;
}
