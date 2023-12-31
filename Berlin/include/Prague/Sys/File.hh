/*$Id: File.hh,v 1.8 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _Prague_File_hh
#define _Prague_File_hh

#include <string>
#include <climits>
#include <sys/stat.h>
#include <sys/types.h>
#include <cerrno>
#include <cstdio>

namespace Prague
{

//. a File encapsulates common file based operations, such as lookup for state information
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
  enum access_t { ru = S_IRUSR,
		  wu = S_IWUSR,
		  xu = S_IXUSR,
		  rg = S_IRGRP,
		  wg = S_IWGRP,
		  xg = S_IXGRP,
		  ro = S_IROTH,
		  wo = S_IWOTH,
		  xo = S_IXOTH,
		  all= ru|wu|xu|rg|wg|xg|ro|wo|xo};
  File(const std::string &);
  File(const File &);
  virtual ~File();
  File &operator = (const File &);
  File &operator = (const std::string &);
  //. return the parent directory
  File parent() const;
  //. return the file's name
  const std::string &name() const { return _shortname;}
  //. return the file's long name
  const std::string &long_name() const { return _longname;}
  //. check whether the file is of the given type
  bool is(type_t t) const { return (_status.st_mode & S_IFMT) == (mode_t) t;}
  //. return the file's type
  long type() const { return (_status.st_mode & S_IFMT);}
  //. return the file's access permission flag
  long access() const { return (_status.st_mode & (ru|wu|xu));}
  //. return the file owner
  uid_t uid() const { return _status.st_uid;}
  //. return the owning group
  gid_t gid() const { return _status.st_gid;}
  //. return the file size
  long  size() const { return  _status.st_size;}
  //. return the access time
  time_t acc_time() const { return _status.st_atime;}
  //. return the modification time
  time_t mod_time() const { return _status.st_mtime;}
  //. return the change time
  time_t ch_time() const { return _status.st_ctime;}

  //. change the access permission
  bool chmod(access_t);
  //. rename the file
  bool mv(const std::string &);
  //. remove the file
  bool rm();
  //. static method to determine the base name for the given string
  static std::string base(const std::string &);
  //. generate a temporary file name
  static std::string tmp() { return ::tmpnam(0);}
protected:
  struct stat _status;
  std::string _longname;
  std::string _shortname;
  bool get_status();
  const char *last_error() const;
  int _error;
private:
};

inline std::string File::base(const std::string &s)
{
  std::string::size_type p = s.find_last_of('/');
  return p == std::string::npos ? s : s.substr(p + 1);
}

inline bool File::get_status()
{
  if (stat(_longname.c_str(), &_status) == -1) { _status.st_mode = 0; _error = errno; return false;} return true;
}

};

#endif
