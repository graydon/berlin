/*$Id: Path.hh,v 1.8 2001/04/10 01:49:29 stefan Exp $
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
#ifndef _Prague_Path_hh
#define _Prague_Path_hh

#include <Prague/Sys/File.hh>
#include <string>
#include <vector>

namespace Prague
{

//. Path implements the Unix Path functionality, i.e. file lookup.
class Path
{
  typedef std::vector<std::string> rep_type;
  struct Predicate { bool operator()(const std::string &name) { File file(name); return file.access() & File::ru;}};
public:
  typedef rep_type::iterator iterator;
  //. construct an empty path
  Path() {}
  //. construct a list of directories, using the given separator to tokenize the string
  Path(const std::string &path, char separator = ':');
  ~Path() {}
  //. append a directory
  void append(const std::string &directory) { _directories.push_back(directory);}
  //. look up a file, using the predicate functor, if non-zero
//   template <typename Predicate = dummy_predicate>
  std::string lookup_file(const std::string &) const;
  //. expand a directory, if it is provided as '~joe/foo'
  static std::string expand_user(const std::string &);
  //. return begin iterator
  iterator begin() { return _directories.begin();}
  //. return end iterator
  iterator end() { return _directories.end();}
  //. return the size, i.e. the number of directories contained in the path
  size_t size() { return _directories.size();}
  //. return ith directory
  const std::string &operator [] (size_t i) { return _directories[i];}
private:
  rep_type _directories;
};

// template <typename Predicate>
inline std::string Path::lookup_file(const std::string &name) const
{
  if (name.empty() || name[0] == '/') return name;
  Predicate predicate;
  for (std::vector<std::string>::const_iterator i = _directories.begin(); i != _directories.end(); i++)
    {
      std::string result = *i + "/" + name;
      if (predicate(result)) return result;
    }
  return std::string();
};

};

#endif
