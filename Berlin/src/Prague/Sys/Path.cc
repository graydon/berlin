/*$Id: Path.cc,v 1.3 1999/08/30 14:42:06 gray Exp $
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

#include "Prague/Sys/Path.hh"
#include "Prague/Sys/User.hh"

using namespace Prague;

/* @Method{Path::Path(char separator)}
 *
 * @Description{construct an empty Path whose tokens are separated by @var{separator}}
 */
/* @Method{Path::Path(const string &list, char separator)}
 *
 * @Description{construct a Path from @var{list} whose tokens are separated by @var{separator}}
 */
Path::Path(const string &list, char c)
  : separator(c)
{
  if (list.empty()) return;
  string::size_type b = 0;
  while (b < list.size())
    {
      string::size_type e = list.find(separator, b);
      paths.push_back(string(list, b, e-b));
      b = e == string::npos ? string::npos : e + 1;
    }
}

/* @Method{Path::~Path()}
 *
 * @Description{}
 */
Path::~Path()
{
}

/* @Method{string Path::lookupFile(const string &name, Path::predicate *p) const}
 *
 * @Description{find file @var{name} and return the absolute path for it. Return 0 if not found.}
 */
string Path::lookupFile(const string &name, Path::predicate *p) const
{
  if (name.empty() || name[0] == '/') return name;
  for (vector<string>::const_iterator i = paths.begin(); i != paths.end(); i++)
    {
      string result = *i + "/" + name;
      if (p && (*p)(result)) return result;
    }
  return string ();
};

/* @Method{string Path::expandUser(const string &path)}
 *
 * @Description{return the absolute path if @var{path} starts with @var{'~'}. If this fails, return @var{path} unchanged}
 */
string Path::expandUser(const string &path)
{
  if (path.empty() || path[0] != '~') return path;
  string pfx;
  string::size_type pos = path.find_first_of('/');
  if (path[1] == '\0' || pos == 1)
    {
      pfx = getenv("HOME");
      if (pfx.empty()) pfx = User().home();
    }
  else
    {
      string name(path,1,(pos==string::npos) ? string::npos : pos-1);
      User user(name.c_str());
      pfx = user.home();
    }
  if (pfx.empty()) return path;
  string result = pfx;
  if (pos == string::npos) return result;
  if (result.empty() || result[result.length()-1] != '/') result += '/';
  result += string(path).substr(pos+1);
  return result;
};

