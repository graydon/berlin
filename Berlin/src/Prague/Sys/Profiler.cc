/*$Id: Profiler.cc,v 1.9 2001/03/21 06:28:55 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent A. Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999,2000 Stefan Seefeld <stefan@berlin-consortium.org>
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

#include "Prague/Sys/Profiler.hh"
#include <vector>
#include <string>
#include <functional>
#include <algorithm>

using namespace Prague;

Profiler::Guard    Profiler::guard;
Profiler::table_t *Profiler::table = 0;
Profiler::item_t  *Profiler::current = 0;
Mutex              Profiler::mutex;

void Profiler::CheckPoint::output(std::ostream &os, unsigned short ind)
{
  if (elapsed > 0.)
    {
      indent(os, ind);
      os << name << ": " << setw(10) << count;
      os << " Times.  Total Time: ";
      os << setprecision(8) << setw(12);
      os.setf( ios::fixed, ios::floatfield);
      os << elapsed/CLOCKS_PER_SEC;
      os  << "  Avg/Iter.: ";
      os << std::setprecision(8) << std::setw(12);
      os << elapsed/count/CLOCKS_PER_SEC << std::endl;
    }
}

#if 1

typedef std::vector<Profiler::CheckPoint> chart;
struct CP_compare : public std::binary_function<Profiler::CheckPoint, Profiler::CheckPoint, bool> 
{
  bool operator()(const Profiler::CheckPoint &cp1, const Profiler::CheckPoint &cp2) const
  {
    return cp1.elapsed/cp1.count > cp2.elapsed/cp2.count;
  }
};
struct CP_find : public std::unary_function<Profiler::CheckPoint, bool>
{
  CP_find(const std::string &s) : scope(s) {}
  bool operator()(const Profiler::CheckPoint &cp) const { return scope == cp.name;}
  std::string scope;
};

void Profiler::dump(std::ostream &os)
{
  Prague::Guard<Mutex> guard(mutex);
  chart scopes;
  for (ntree<CheckPoint *>::iterator i = table->begin(); i != table->end(); i++)
    {
      /*
       * get a copy of the checkpoint and subtract all child checkpoints from it
       */
      CheckPoint cp = *(*i).value;
      for (ntree<CheckPoint *>::const_child_iterator j = (*i).child_begin(); j != (*i).child_end(); j++)
	cp.elapsed -= (*j).value->elapsed;
      /*
       * now insert it into the chart
       */
      chart::iterator j = find_if(scopes.begin(), scopes.end(), CP_find(cp.name));
      if (j == scopes.end()) scopes.push_back(cp);
      else
	{
	  (*j).elapsed += cp.elapsed;
	  (*j).count   += cp.count;
	}
    }
  /*
   * finally, sort the results
   */
  std::sort(scopes.begin(), scopes.end(), CP_compare());
  /*
   * now dump it to the ostream
   */
  for (chart::iterator i = scopes.begin(); i != scopes.end(); i++)
    (*i).output(os, 0);
}

#else

void Profiler::dump(std::ostream &os)
{
  Prague::Guard<Mutex> guard(mutex);
  dump(os, *current, 0);
}

#endif

void Profiler::dump(std::ostream &os, const item_t &root, unsigned short ind)
{
  for (const_child_iterator i = root.child_begin(); i != root.child_end(); i++)
    dump(os, *i, ind + 1);
  if (root.value) root.value->output(os, ind);
}
