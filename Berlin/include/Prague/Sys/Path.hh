/*$Id: Path.hh,v 1.2 1999/04/27 20:11:11 gray Exp $
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
#ifndef _Path_hh
#define _Path_hh

#include <string>
#include <vector>

namespace Prague
{

/* @Class{Path}
 *
 * @Description{represents a search path for easy file look up}
 */
class Path
{
  typedef vector<string> rep_type;
  typedef rep_type::iterator iterator;
public:
  struct predicate { virtual bool operator()(const string &name) const = 0;};
  Path(char c = ':') : paths(0), separator(c) {}
  Path(const string &, char c = ':');
  ~Path();
  void append(const string &path) { paths.push_back(path);}
  string lookupFile(const string &, predicate * = 0) const;
  static string expandUser(const string &);
  iterator begin() { return paths.begin();}
  iterator end() { return paths.end();}
  size_t size() { return paths.size();}
  const string &operator [] (size_t i) { return paths[i];}
protected:
  rep_type paths;
  char separator;
private:
};

};

#endif /* _Path_hh */
