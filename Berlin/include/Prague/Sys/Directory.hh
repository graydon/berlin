/*$Id: Directory.hh,v 1.3 1999/10/19 21:07:52 gray Exp $
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
#ifndef _Directory_hh
#define _Directory_hh

#include <string>
#include <vector>
#include <algorithm>
#include <Prague/Sys/File.hh>

namespace Prague
{

/* @Class{Directory : public File}
 *
 * @Description{contains a list of all files found in this directory}
 */
class Directory : public File
{
public:
  typedef vector<File *>::iterator iterator;
  typedef vector<File *>::const_iterator const_iterator;
  enum { unsorted = 0x0, dirsfirst = 0x1, size = 0x2, modtime = 0x4, acctime = 0x8, alpha = 0x10};
  enum { unfiltered = 0x0, nohidden = 0x1, 
	 readable = 0x2, writable = 0x4, executable = 0x8,
	 dirs = 0x10, nodirs = 0x20};
  Directory(const string &, int order, int filter = unfiltered);
  //. list all files according to <i>filter</i> in the order given by @var{order}
  Directory(const string &, int order, const string &);
  //. list all files according to @var{filter} in the order given by @var{order}
  Directory(const Directory &);
  ~Directory();
  File *operator [] (unsigned int i) { return children[i];}
  unsigned int Children() const { return children.size();}
  iterator begin() { return children.begin();}
  const_iterator begin() const { return children.begin();}
  iterator end() { return children.end();}
  const_iterator end() const { return children.end();}
protected:
  vector<File *> children;
private:
};

};

#endif /* _Directory_hh */
