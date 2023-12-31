/*$Id: Profiler.hh,v 1.17 2001/03/21 06:28:22 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent A. Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org
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

#ifndef _Prague_Profiler_hh
#define _Prague_Profiler_hh

#include <Prague/Sys/ntree.hh>
#include <Prague/Sys/Thread.hh>
#include <ctime>
#include <iostream>
#include <string>
#include <iomanip.h>

namespace Prague
{

//. a poor man's profiler. The basic idea is to use a guard object which measures
//. the elapsed time between constructor and destructor and writes its findings into
//. a profiler data base (a scope tree).
class Profiler
{
public:
  struct CheckPoint
  {
    CheckPoint(const std::string &s) : name(s), count(0), start(0), stop(0), elapsed(0) {}
    CheckPoint(CheckPoint *cp)
      : name(cp->name),
	count(cp->count),
	start(cp->start),
	stop(cp->stop),
	elapsed(cp->elapsed)
    {}
    void indent(ostream &os, unsigned short ind) { while (ind--) os.put(' ');}
    void output(ostream &os, unsigned short ind);
    std::string name;
    long count;
    clock_t start;
    clock_t stop;
    double elapsed;
  };
private:
  typedef ntree<CheckPoint *> table_t;
  typedef ntree<CheckPoint *>::node item_t;
  typedef ntree<CheckPoint *>::node::child_iterator child_iterator;
  typedef ntree<CheckPoint *>::node::const_child_iterator const_child_iterator;
  typedef ntree<CheckPoint *>::node::up_iterator up_iterator;
  struct Guard
  {
    Guard()
      {
	Profiler::table = new table_t(new CheckPoint("RootEntry"));
	Profiler::current = &table->root();
      }
    ~Guard()
      {
	Profiler::clean(*Profiler::current);
	delete Profiler::table;
      }
  };
  friend struct Guard;
public:
  Profiler(const std::string &name)
    {
#ifdef profile
      Prague::Guard<Mutex> guard(mutex);
      child_iterator i = lookup(name);
      current = &*i;
      current->value->count++;
      current->value->start = clock(); 
#endif /* profile */
    }
  ~Profiler()
    {
#ifdef profile
      Prague::Guard<Mutex> guard(mutex);
      current->value->stop = clock();
      current->value->elapsed += (current->value->stop - current->value->start);
      up_iterator i = current->up_begin();
      current = &*++i;
#endif /* profile */
    }
  static void dump(std::ostream &);
private:
  static child_iterator lookup(const std::string &name)
    {
      for (child_iterator i = current->child_begin(); i != current->child_end(); i++)
	if ((*i).value->name == name) return i;
      return current->push_back(new CheckPoint(name));
    }
  static void dump(std::ostream &, const item_t &, unsigned short);
  static void clean(const item_t &root)
    {
      for (const_child_iterator i = root.child_begin(); i != root.child_end(); i++)
	clean(*i);
      delete root.value;
    }
  static Guard    guard;
  static table_t *table;
  static item_t  *current;
  static Mutex    mutex;
};

};

#endif
