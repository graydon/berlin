/*$Id: File.cc,v 1.3 1999/10/15 17:59:26 gray Exp $
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

#include "Prague/Sys/File.hh"
#include <string.h>
#include <unistd.h>
#include <iostream>

using namespace Prague;

File::File(const string &n)
  : longname (n),
    shortname(File::base(n)),
    error(0)
{
  getStatus();
}

File::File(const File &f)
  : longname(f.longname),
    shortname(f.shortname),
    error(0)
{
  getStatus();
}

File::~File() {}

File &File::operator = (const File &f)
{
  longname = f.longname;
  shortname = f.longname;
  getStatus();
  return *this;
}

File &File::operator = (const string &n)
{
  longname = n;
  shortname = File::base(n);
  getStatus();
  return *this;
}

File File::parent() const
{
  if (shortname == "/") return *this;
  return File(longname.substr(0, longname.find_last_of('/')));
}

bool File::chmod(access_t a)
{
  if (::chmod(longname.c_str(), a) == -1) error = errno;
  else if (!getStatus()) return false;
  else return true;
  return false;
}

bool File::mv(const string &name)
{
  if (rename(longname.c_str(), name.c_str()) == -1)
    {
      error = errno;
      return false;
    }
  else
    {
      longname  = name;
      shortname = File::base(name);
      return true;
    }
}

bool File::rm()
{
  if (remove(longname.c_str()) == -1)
    {
      error = errno;
      return false;
    }
  else
    {
      longname  = "";
      shortname = "";
      status.st_mode = 0;
      return true;
    }
}

const char *File::lastError() const { return sys_errlist[error];}

