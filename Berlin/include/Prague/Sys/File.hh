/*$Id: File.hh,v 1.4 1999/10/15 17:59:07 gray Exp $
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
#ifndef _File_hh
#define _File_hh

#include <string>
#include <climits>
#include <sys/stat.h>
#include <sys/types.h>
#include <cerrno>
#include <cstdio>

namespace Prague
{

/* @Class{File}
 *
 * @Description{}
 */
class File
{
public:
  enum type_t { none = 0, 
		link = S_IFLNK, 
		reg  = S_IFREG, 
		dir  = S_IFDIR, 
		chr  = S_IFCHR, 
		blk  = S_IFBLK, 
		fifo = S_IFIFO, 
		sock = S_IFSOCK};
  enum access_t { ur = S_IRUSR,
		  uw = S_IWUSR,
		  ux = S_IXUSR,
		  gr = S_IRGRP,
		  gw = S_IWGRP,
		  gx = S_IXGRP,
		  or = S_IROTH,
		  ow = S_IWOTH,
		  ox = S_IXOTH,
		  all= ur|uw|ux|gr|gw|gx|or|ow|ox};
  File(const string &);
  File(const File &);
  virtual ~File();
  File &operator = (const File &);
  File &operator = (const string &);
  File parent() const;
  const string &name() const { return shortname;}
  const string &longName() const { return longname;}
  bool is(type_t t) const { return (status.st_mode & S_IFMT) == (mode_t) t;}
  long type() const { return (status.st_mode & S_IFMT);}
  long access() const { return (status.st_mode & (ur|uw|ux));}
  uid_t uid() const { return status.st_uid;}
  gid_t gid() const { return status.st_gid;}
  long  size() const { return  status.st_size;}
  time_t accTime() const { return status.st_atime;}
  time_t modTime() const { return status.st_mtime;}
  time_t chTime() const { return status.st_ctime;}

  bool chmod(access_t);
  bool mv(const string &);
  bool rm();
  static string base(const string &);
  static string tmp() { return ::tmpnam(0);}
protected:
  struct stat status;
  string longname;
  string shortname;
  bool getStatus();
  const char *lastError() const;
  int error;
private:
};

inline string File::base(const string &s)
{
  string::size_type p = s.find_last_of('/');
  return p == string::npos ? s : s.substr(p + 1);
}

inline bool File::getStatus()
{
  if (stat(longname.c_str(), &status) == -1) { status.st_mode = 0; error = errno; return false;} return true;
}

};

#endif /* _File_hh */
