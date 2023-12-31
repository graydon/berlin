/*$Id: File.cc,v 1.6 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/Sys/File.hh"
#include <cstring>
#include <iostream>
#include <unistd.h>

using namespace Prague;

File::File(const std::string &n)
  : _longname (n),
    _shortname(File::base(n)),
    _error(0)
{
  get_status();
}

File::File(const File &f)
  : _longname(f._longname),
    _shortname(f._shortname),
    _error(0)
{
  get_status();
}

File::~File() {}

File &File::operator = (const File &f)
{
  _longname = f._longname;
  _shortname = f._longname;
  get_status();
  return *this;
}

File &File::operator = (const std::string &n)
{
  _longname = n;
  _shortname = File::base(n);
  get_status();
  return *this;
}

File File::parent() const
{
  if (_shortname == "/") return *this;
  return File(_longname.substr(0, _longname.find_last_of('/')));
}

bool File::chmod(access_t a)
{
  if (::chmod(_longname.c_str(), a) == -1) _error = errno;
  else if (!get_status()) return false;
  else return true;
  return false;
}

bool File::mv(const std::string &name)
{
  if (rename(_longname.c_str(), name.c_str()) == -1)
    {
      _error = errno;
      return false;
    }
  else
    {
      _longname  = name;
      _shortname = File::base(name);
      return true;
    }
}

bool File::rm()
{
  if (remove(_longname.c_str()) == -1)
    {
      _error = errno;
      return false;
    }
  else
    {
      _longname  = "";
      _shortname = "";
      _status.st_mode = 0;
      return true;
    }
}

const char *File::last_error() const { return strerror(_error);}

