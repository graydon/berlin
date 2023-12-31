/*$Id: Directory.hh,v 1.7 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _Prague_Directory_hh
#define _Prague_Directory_hh

#include <string>
#include <vector>
#include <algorithm>
#include <Prague/Sys/File.hh>

namespace Prague
{

//. Directory contains a list of all files that live in this directory
class Directory : public File
{
public:
  typedef std::vector<File *>::iterator iterator;
  typedef std::vector<File *>::const_iterator const_iterator;
  enum { unsorted = 0x0, dirsfirst = 0x1, size = 0x2, modtime = 0x4, acctime = 0x8, alpha = 0x10};
  enum { unfiltered = 0x0, nohidden = 0x1, 
	 readable = 0x2, writable = 0x4, executable = 0x8,
	 dirs = 0x10, nodirs = 0x20};
  Directory(const std::string &, int order, int filter = unfiltered);
  //. list all files according to <i>filter</i> in the order given by @var{order}
  Directory(const std::string &, int order, const std::string &);
  //. list all files according to @var{filter} in the order given by @var{order}
  Directory(const Directory &);
  ~Directory();
  //. return file with index i
  File *operator [] (unsigned int i) { return _children[i];}
  //. return number of files
  unsigned int children() const { return _children.size();}
  //. return begin iterator
  iterator begin() { return _children.begin();}
  //. return begin iterator
  const_iterator begin() const { return _children.begin();}
  //. return end iterator
  iterator end() { return _children.end();}
  //. return end iterator
  const_iterator end() const { return _children.end();}
protected:
  std::vector<File *> _children;
private:
};

};

#endif
