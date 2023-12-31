/*$Id: Directory.cc,v 1.6 2001/03/21 06:28:55 stefan Exp $
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

#include <Prague/Sys/Directory.hh>
#include <Prague/Sys/regex.hh>
#include <dirent.h>

using namespace Prague;

bool compSize(File *a, File *b) { return  (a->size() > b->size());}
bool compAccTime(File *a, File *b) { return a->acc_time() < b->acc_time();}
bool compModTime(File *a, File *b) { return a->mod_time() < b->mod_time();}
bool compAlpha(File *a, File *b) { return a->name() < b->name();}
bool compDirsFirst(File *a, File *b)
{
  if (a->is(File::dir) && !b->is(File::dir)) return true;
  else if (!a->is(File::dir) && b->is(File::dir)) return false;
  else return compAlpha(a, b);
};

Directory::Directory(const std::string &n, int order, int filter)
  : File(n)
{
  while (_longname.length() > 2 && _longname[_longname.length() - 1] == '.' && _longname[_longname.length() - 2] == '.')
    {
      std::string::size_type i = _longname.rfind('/', _longname.length() - 3);
      _longname.erase(i, _longname.length());
    }
  _shortname = File::base(_longname);
  if (get_status() && is(File::dir))
    {
      DIR *dir = opendir(_longname.c_str());
      for (struct dirent *entry = readdir(dir); entry; entry = readdir(dir))
	{
	  std::string childname = _longname + '/' + entry->d_name;
	  if (filter == unfiltered ||
	      filter == nohidden && entry->d_name[0] != '.' ||
	      filter == dirs && File(childname).is(File::dir) ||
	      filter == nodirs && !File(childname).is(File::dir))
	    _children.push_back(new File(childname));
	}
      closedir(dir);
      switch (order)
	{
	case dirsfirst: sort(_children.begin(), _children.end(), compDirsFirst); break;
	case size:      sort(_children.begin(), _children.end(), compSize); break;
	case modtime:   sort(_children.begin(), _children.end(), compModTime); break;
	case acctime:   sort(_children.begin(), _children.end(), compAccTime); break;
	case alpha:
	default:        sort(_children.begin(), _children.end(), compAlpha); break;
	}
    }
}

Directory::Directory(const std::string &n, int order, const std::string &pattern)
  : File(n)
{
  while (_longname.length() > 2 && _longname[_longname.length() - 1] == '.' && _longname[_longname.length() - 2] == '.')
    {
      std::string::size_type i = _longname.rfind('/', _longname.length() - 3);
      _longname.erase(i, _longname.length());
    }
  _shortname = File::base(n);
  if (get_status() && is(File::dir))
    {
      regex filter(pattern);
      DIR *dir = opendir(_longname.c_str());
      for (struct dirent *entry = readdir(dir); entry; entry = readdir(dir))
	{
	  if (filter.search(entry->d_name))
	    {
	      std::string childname = _longname + '/' + entry->d_name;
	      _children.push_back(new File(childname));
	    }
	}
      closedir(dir);
      switch (order)
	{
	case dirsfirst: sort(_children.begin(), _children.end(), compDirsFirst); break;
	case size:      sort(_children.begin(), _children.end(), compSize); break;
	case modtime:   sort(_children.begin(), _children.end(), compModTime); break;
	case acctime:   sort(_children.begin(), _children.end(), compAccTime); break;
	case alpha:
	default:        sort(_children.begin(), _children.end(), compAlpha); break;
	}
    }
}

Directory::Directory(const Directory &dir)
  : File(dir)
{
  for (std::vector<File *>::const_iterator i = dir._children.begin(); i != dir._children.end(); i++)
    _children.push_back(new File((*i)->name()));
}

Directory::~Directory()
{
  for (std::vector<File *>::iterator i = _children.begin(); i != _children.end(); i++) delete *i;
}

