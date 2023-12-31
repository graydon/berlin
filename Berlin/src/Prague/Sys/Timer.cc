/*$Id: Timer.cc,v 1.10 2001/03/21 06:28:55 stefan Exp $
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
#include "Prague/Sys/Timer.hh"
#include <algorithm>

using namespace Prague;

std::vector<Timer *> Timer::timers;
Thread               Timer::server(&Timer::start, 0);
Mutex                Timer::mutex;
Condition            Timer::condition(Timer::mutex);

void Timer::start(const Time &t, const Time &i)
{
  timeout = t;
  interval = i;
  Timer::schedule(this);
};

void Timer::stop()
{
  Timer::cancel(this);
};

void *Timer::start(void *)
{
  Prague::Guard<Mutex> guard(mutex);
  while (true)
    {
      if (!timers.size()) condition.wait();
      else
        {
          Time time = timers.front()->timeout;
          condition.wait(time);
        }
      expire();
    }
  return 0;
}

void Timer::expire()
{
  Time now = Time::currentTime();
  while (timers.size() && timers.front()->timeout <= now)
    {
      Timer *timer = timers.front();
      timer->notifier->notify();
      if (timer->interval != Time::zero)
	{
          do timer->timeout += timer->interval;
          while (timer->timeout <= now);
	  timers.erase(timers.begin());
	  timers.push_back(timer);
	  push_heap(timers.begin(), timers.end(), comp());
	}
    }
}

void Timer::schedule(Timer *timer)
{
  if (server.state() != Thread::running) server.start();
  Prague::Guard<Mutex> guard(mutex);
  timers.push_back(timer);
  push_heap(timers.begin(), timers.end(), comp());
  condition.signal();
}

void Timer::cancel(Timer *timer)
{
  Prague::Guard<Mutex> guard(mutex);
  std::vector<Timer *>::iterator i = find(timers.begin(), timers.end(), timer);
  if (i != timers.end()) timers.erase(i);
  condition.signal();
}
