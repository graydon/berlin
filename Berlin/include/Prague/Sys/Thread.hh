/*$Id: Thread.hh,v 1.12 1999/11/16 02:15:20 stefan Exp $
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
#ifndef _Prague_Thread_hh
#define _Prague_Thread_hh

#include <Prague/Sys/Time.hh>
#include <pthread.h>
#include <semaphore.h>
#include <cerrno>
#include <vector>
#include <string>
#include <pair.h>

namespace Prague
{

class Mutex : public pthread_mutex_t
{
public:
  class Attribute : public pthread_mutexattr_t
  {
  public:
    Attribute() { pthread_mutexattr_init (this);}
    ~Attribute() { pthread_mutexattr_destroy(this);}
  };
  enum type { fast = PTHREAD_MUTEX_FAST_NP,
	      recursive = PTHREAD_MUTEX_RECURSIVE_NP};
  Mutex() { pthread_mutex_init(this, 0);}
  Mutex(type t);
  ~Mutex() { pthread_mutex_destroy(this);}
  void lock() { pthread_mutex_lock(this);}
  void unlock() { pthread_mutex_unlock(this);}
  bool trylock() { return pthread_mutex_trylock(this);}
};

class MutexGuard
{
public:
  MutexGuard(Mutex &m) : mutex(m) { mutex.lock();}
  ~MutexGuard() { mutex.unlock();}
private:
  MutexGuard(const MutexGuard &);
  MutexGuard &operator = (const MutexGuard &);
  Mutex &mutex;
};

class Condition : public pthread_cond_t
{
public:
  class Attribute : public pthread_condattr_t
  {
  public:
    Attribute() { pthread_condattr_init (this);}
    ~Attribute() { pthread_condattr_destroy(this);}
  };
  Condition(Mutex &m) : mutex(m) { pthread_cond_init(this, 0);}
  ~Condition() { pthread_cond_destroy(this);}
  void broadcast() { pthread_cond_broadcast(this);}
  void signal() { pthread_cond_signal(this);}
  void wait() { pthread_cond_wait(this, &mutex);}
  void wait(const Time &t) { timespec ts = t; pthread_cond_timedwait(this, &mutex, &ts);}
private:
  Mutex &mutex;
};

class Semaphore : public sem_t
{
public:
  Semaphore(unsigned int v = 0) { sem_init(this, 0, v);}
  ~Semaphore() { sem_destroy(this);}
  void wait() { sem_wait(this);}
  bool trywait() { return sem_trywait(this);}
  void post() { sem_post(this);}
  int value() { int v; sem_getvalue(this, &v); return v;}
private:
  Semaphore(const Semaphore &);
  Semaphore &operator = (const Semaphore &);
};

class SemaphoreGuard
{
public:
    SemaphoreGuard(Semaphore &s) : semaphore(s) { semaphore.wait();}
    ~SemaphoreGuard() { semaphore.post();}
private:
  SemaphoreGuard(const SemaphoreGuard &);
  SemaphoreGuard &operator = (const SemaphoreGuard &);
  Semaphore &semaphore;
};

class Thread
{
  struct Guard
  {
    Guard();
    ~Guard();
  };
  friend struct Guard;
public:
  enum priority_t {low, normal, high};
  enum state_t {ready, running, terminated, canceled, joined};
  template <class T> class Data;
  template <class T> class Queue;
  typedef void *(*proc)(void *);
  class Attribute : public pthread_attr_t
  {
  public:
    Attribute() { pthread_attr_init(this);}
    ~Attribute() { pthread_attr_destroy(this);}
  };
  class Exception
  {
  public:
    Exception(const string &m) : msg(m) {}
    const string &what() const { return msg;}
  private:
    string msg;
  };
  Thread(proc, void *, priority_t = normal);
  ~Thread();
  priority_t priority() { MutexGuard guard(mutex); return _priority;}
  state_t state() { MutexGuard guard(mutex); return _state;}
  void start() throw (Exception);
  //. run the thread
  void join(void **) throw (Exception);
  //. wait for the thread to finish returning it's return value
  void cancel();
  //. cancel the thread
  void detach();
  //. detach the thread
  static void exit(void *);
  //. exit the calling thread with return value r
  static bool delay(const Time &);
  //. delay execution of the calling thread
  static void testcancel() { pthread_testcancel();}
  //. cancellation point
  static Thread *self();
  //. return a Thread pointer for the calling thread or 0 if it wasn't created by this Thread class
  static unsigned long id();
  //. return a thread id. This is supposed to give a valid number even for third party threads
private:
  Thread(pthread_t pt) : p(0), arg(0), thread(pt), _priority(normal), _state(running), detached(false) {}
  static void *start(void *);
  proc p;
  void *arg;
  pthread_t thread;
  priority_t _priority;
  state_t _state;
  bool detached;
  Mutex mutex;
  static unsigned long counter;
  static Thread *main;
  static Guard guard;
  static pthread_key_t self_key;
  static pthread_key_t id_key;
  static Mutex         id_mutex;
};

inline Thread *Thread::self()
{
  return reinterpret_cast<Thread *>(pthread_getspecific(self_key));
}

inline unsigned long Thread::id()
{
  unsigned long *_id = reinterpret_cast<unsigned long *>(pthread_getspecific(id_key));
  if (!_id)
    {
      MutexGuard guard(id_mutex);
      if (!_id)
	{
	  _id = new unsigned long (counter++);
	  pthread_setspecific(id_key, _id);
	}
    }
  return *_id;
}

};

#endif /* _Prague_Thread_hh */
