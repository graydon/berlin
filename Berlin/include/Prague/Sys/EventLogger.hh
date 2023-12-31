/*$Id: EventLogger.hh,v 1.3 1999/10/22 02:54:50 gray Exp $
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
#ifndef _EventLogger_hh
#define _EventLogger_hh

#include <vector>
#include "Prague/Sys/Thread.hh"

// this class represents a lightweight circular logging stream
// similar to the one in GNU NANA except designed for use in 
// a multithreaded C++ environment with section guards.

namespace Prague
{

  struct LoggedEvent {
    double time;
    unsigned long thread;
    const char *type;
    const char *specifics;
  };

class EventLogger
{
public:
  EventLogger(size_t s) : events(s), next(0), wrapflag(true), wrapped(false) {}
  ~EventLogger() {}
  void wrap(bool flag) { wrapflag = flag;}

  void add(const char *ty, const char *n, double t) {
    MutexGuard guard(myMutex);
    if (next == events.size()) return;
    LoggedEvent e;
    e.time = t;
    e.thread = Thread::self()->id();
    e.type = ty;
    e.specifics = n;
    events[next] = e;
    next++;
    if (next == events.size() && wrapflag) 
      {
	next = 0;
	wrapped = true;
      }
  }

  void add(const char *n, double t)
    {
      add("", n, t);
    }

  void clear() { next = 0; wrapped = false;}
  void dump(ostream &os)
    {
      vector<LoggedEvent>::iterator event;
      os << "EventLogger::dump =\n";
      if (wrapped) {
	for (event = events.begin() + next; event != events.end(); event++) {
	  os << event->time << ':' << event->thread << '\t' << event->type << ' ' << event->specifics << endl;
	}
	for (event = events.begin(); event != events.begin() + next; event++) {
	  os << event->time << ':' << event->thread << '\t' << event->type << ' ' << event->specifics << endl;
	}
      }
    }
private:
  vector<LoggedEvent> events;

//   vector<const char *> events;
//   vector<double> times;

  Mutex myMutex;
  unsigned int next;
  bool wrapflag : 1;
  bool wrapped  : 1;
};

};

#endif /* _EventLogger_hh */
