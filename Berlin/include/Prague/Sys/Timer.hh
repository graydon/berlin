/*$Id: Timer.hh,v 1.7 1999/09/30 17:23:33 gray Exp $
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
#ifndef _Timer_hh
#define _Timer_hh

#include <Prague/Sys/Time.hh>
#include <Prague/Sys/Thread.hh>
#include <vector>

namespace Prague
{

class Timer
{
  struct comp;
  friend class comp;
  struct comp { bool operator () (const Timer *t1, const Timer *t2) { return t1->timeout > t2->timeout;}};
public:
  struct Notifier { virtual ~Notifier(){}; virtual void notify() = 0;};
  Timer(Notifier *n) : notifier(n) {}
  virtual ~Timer() {}
  void  start(const Time &, const Time & = Time::zero);
  void stop();
private:
  Notifier *notifier;
  Time timeout;
  Time interval;
  static void *start(void *);
  static void expire();
  static void schedule(Timer *);
  static void cancel(Timer *);
  static vector<Timer *> timers;
  static Thread server;
  static Mutex mutex;
  static Condition condition;
};

};

#endif /* _Timer_hh */
