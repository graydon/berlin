/*$Id: Path.cc,v 1.8 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/Sys/Path.hh"
#include "Prague/Sys/User.hh"

using namespace Prague;

Path::Path(const std::string &list, char separator)
{
  if (list.empty()) return;
  std::string::size_type b = 0;
  while (b < list.size())
    {
      std::string::size_type e = list.find(separator, b);
      _directories.push_back(std::string(list, b, e-b));
      b = e == std::string::npos ? std::string::npos : e + 1;
    }
}

std::string Path::expand_user(const std::string &path)
{
  if (path.empty() || path[0] != '~') return path;
  std::string pfx;
  std::string::size_type pos = path.find_first_of('/');
  if (path[1] == '\0' || pos == 1)
    {
      pfx = getenv("HOME");
      if (pfx.empty()) pfx = User().home();
    }
  else
    {
      std::string name(path,1,(pos==std::string::npos) ? std::string::npos : pos-1);
      User user(name.c_str());
      pfx = user.home();
    }
  if (pfx.empty()) return path;
  std::string result = pfx;
  if (pos == std::string::npos) return result;
  if (result.empty() || result[result.length()-1] != '/') result += '/';
  result += std::string(path).substr(pos+1);
  return result;
};
