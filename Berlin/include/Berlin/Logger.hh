/*$Id: Logger.hh,v 1.13 1999/11/08 17:37:44 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _Logger_hh
#define _Logger_hh

#include "Warsaw/config.hh"
#include "Prague/Sys/logstream.hh"
#include "Prague/Sys/EventLogger.hh"
#include "Prague/Sys/Time.hh"
#include "Prague/Sys/Thread.hh"
#include "Prague/Sys/Profiler.hh"

class Logger
//. this is a rewrite of our debugging class to use some NANA features.  the idea
//. is that what was previously just a matter of writing to stderr, we now log
//. *everything* to an internal buffer, and only write to stderr if the group
//. you're writing to is enabled. Then when the system keels over, we dump the
//. in-core log and ask users to mail it to us, as a bug report
{
public:
  enum group
  { corba, loader, traversal, thread,
    main, agent, message, command,
    subject, observer, text, widget,
    image, figure, layout, drawing, desktop,
    picking, focus, geometry};
private:
  struct streamlock;
  friend struct streamlock;
  template <class T>
  static void write(group g, const T &t)
    {
      los << t;
      if (active[g]) cerr << t;
    }
  static const int numGroups = 20;
public:
  static void set(group g) { active[g] = true; }
  static void clear(group g) { active[g] = false; }
  static void setall() { for (int i = 0; i < numGroups; i++) active[i] = true;}
  static void note(const char *c)
    {
      events.add(c, Prague::Time::currentTime() - start);
    }
  static void note(const char *t, const char *c)
    {
      events.add(t, c, Prague::Time::currentTime() - start);
    }

  static streamlock log(group g)
    {
      streamlock slock(g);
      write(g, '[');
      write(g, static_cast<double>(Prague::Time::currentTime() - start));
      write(g, ':');
      write(g, Prague::Thread::id());
      write(g, ':');
      write(g, groupname[g]);
      write(g, "]\t");
      return slock;
    } 
  static void dump(ostream &);
protected:
private:
  struct streamlock
  {
    streamlock(group gg) : owner(true), g(gg), prf("logger") { Logger::mutex.lock();}
    streamlock(const streamlock &sl) : owner(true), g(sl.g), prf("logger") { sl.owner = false;}
    ~streamlock() { if (owner) Logger::mutex.unlock();}
    mutable bool owner;
    group g;
    Prague::Profiler prf;
  };
  friend const streamlock &operator << (const streamlock &sl, ostream & (func)(ostream &))
    { Logger::write(sl.g, func); return sl;}
  template <class T>
  friend const streamlock &operator << (const streamlock &sl, const T &t) { Logger::write(sl.g, t); return sl;}
  static bool active[numGroups];
  static const char *groupname[numGroups]; 
  static Prague::logbuf buf;
  static Prague::logstream los;
  static Prague::EventLogger events;    
  static Prague::Time start;
  static Prague::Mutex mutex;
};

#if 1
class SectionLog
{
public:
  SectionLog(const char *s) : section(s) { Logger::note("enter", section);}
  ~SectionLog() { Logger::note("leave", section);}
private:
  const char *section;
};
#else
class SectionLog
{
public:
  SectionLog(const char *) {}
  ~SectionLog() {}
};
#endif

#endif /* _Logger_hh */
