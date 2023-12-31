/*$Id: Thread.hh,v 1.6 1999/09/30 17:23:33 gray Exp $
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
#ifndef _Thread_hh
#define _Thread_hh

#include <Warsaw/config.hh>

#if 0 //def __OMNIORB2__
#include <omnithread.h>

typedef omni_mutex Mutex;
typedef omni_condition Condition;
typedef omni_semaphore Semaphore;
typedef omni_thread Thread;

class MutexGuard
{
public:
  MutexGuard(Mutex &m) : mutex(m) { mutex.lock();}
  ~MutexGuard() { mutex.unlock();}
private:
  Mutex mutex;
};

#else
#include <Prague/Sys/Thread.hh>

#if 1
typedef Prague::Mutex Mutex;
typedef Prague::Condition Condition;
typedef Prague::Semaphore Semaphore;
typedef Prague::Thread Thread;
typedef Prague::MutexGuard MutexGuard;
#endif
#endif

#endif /* _Thread_hh */
