/*$Id: Tracer.hh,v 1.5 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _Prague_Tracer_hh
#define _Prague_Tracer_hh

#include <Prague/config.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadData.hh>
#include <vector>

namespace Prague
{

class Trace;

//. this class represents a lightweight circular tracing stream
//. similar to the one in GNU NANA except designed for use in 
//. a multithreaded C++ environment with section guards.
class Tracer
{
  struct Event
  {
    double time;
    unsigned long thread;
    const char *type;
    const char *specifics;
    friend std::ostream &operator << (std::ostream &os, const Event &e)
    { return os << e.time << ':' << e.thread << '\t' << e.type << ' ' << e.specifics;}
  };
  friend class Trace;
public:
  static void resize(size_t s) { Prague::Guard<Mutex> guard(_mutex); _events.resize(s); if (_next >= s) _next = 0;}
  static void logging(bool l) { _log = l;}
  static void add(const char *n, const char *ty = "")
  {
    {
      Prague::Guard<Mutex> guard(_mutex);
      if (_next == _events.size()) return;
      _events[_next].time = Time::currentTime() - _start;
      _events[_next].thread = Thread::self()->id();
      _events[_next].type = ty;
      _events[_next].specifics = n;
      if (_log)
	{
	  for (unsigned short i = 0; i != *_indent; i++) std::cerr.put(' ');
	  std::cerr << _events[_next] << std::endl;
	}
      _next++;
      if (_next == _events.size()) 
	{
	  _next = 0;
	  _wrapped = true;
	}
    }
  }
  static void clear() { _next = 0; _wrapped = false;}
  static void dump(std::ostream &os)
  {
    std::vector<Event>::iterator e;
    os << "* Tracer::dump =\n";
    if (_wrapped) for (e = _events.begin() + _next; e != _events.end(); e++) os << *e << '\n';
    for (e = _events.begin(); e != _events.begin() + _next; e++) os << *e << '\n';
    os << "* end of Tracer::dump" << std::endl; 
  }
private:
  static std::vector<Event>           _events;
  static Time                         _start;
  static Thread::Data<unsigned short> _indent;
  static Mutex                        _mutex;
  static unsigned int                 _next;
  static bool                         _wrapped;
  static bool                         _log;
};

#ifdef TRACER
class Trace
{
public:
  Trace(const char *s) : section(s) { Tracer::add(section, "enter"); ++*Tracer::_indent;}
  ~Trace() { --*Tracer::_indent; Tracer::add(section, "leave");}
private:
  const char *section;
};
#else
class Trace
{
public:
  Trace(const char *) {}
  ~Trace() {}
};
#endif

};

#endif
