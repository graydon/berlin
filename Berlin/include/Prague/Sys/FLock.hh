/*$Id: FLock.hh,v 1.7 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _FLock_hh
#define _FLock_hh

#include <Prague/Sys/File.hh>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <string>
#include <iostream>
#include <fstream.h>

namespace Prague
{

//. provide locking based on fcntl. FLocks can be activated in read and in write mode.
class FLock
{
public:
  enum mode {read = F_RDLCK, write = F_WRLCK};
  //. create a FLock using a temporary file
  FLock();
  //. create a FLock for a given file
  FLock(int);
  //. create a FLock using file @var{name}, create if @var{create} is true
  FLock(const std::string &, bool = true);
  //. Description{close the associated file, removes it if is a temporary one.
  ~FLock();
  const std::string &name() { return _file;}
  bool trylock(mode m = read) { return doit(m, false) == 0;}
  bool trylock(off_t offset, off_t length, mode m = read) { return doit(offset, length, m, false) == 0;}
  //. try to activate the lock for mode @var{m}, return @var{true} if it successful}
  bool lock(mode m = read) { return doit(m, true) == 0;}
  bool lock(off_t offset, off_t length, mode m = read) { return doit(offset, length, m, true) == 0;}
  bool unlock() { return doit(F_UNLCK, false) == 0;}
  pid_t locked(mode m) const;
private:
  int doit(int, bool);
  int doit(off_t, off_t, int, bool);
  std::string _file;
  bool        _close : 1;
  bool        _remove: 1;
  int         _fd;
};

inline FLock::FLock()
  : _file(File::tmp()), _close(true), _remove(true)
{
  _fd = open(_file.c_str(), O_RDWR|O_CREAT, 0600);
  ::write(_fd, "l", 1);
};

inline FLock::FLock(int f)
  : _close(false), _remove(false), _fd(f)
{
};

inline FLock::FLock(const std::string &f, bool create)
  : _file(f), _close(true), _remove(false)
{
  if (create) _fd = open(_file.c_str(), create ? O_RDWR|O_CREAT : O_RDWR, 0600);
  if (create) ::write(_fd, "l", 1);
};

inline FLock::~FLock()
{
  if (_close) ::close(_fd);
  if (_remove) ::remove(_file.c_str());
};

inline int FLock::doit(int type, bool flag)
{
  struct flock lock;
  lock.l_type  = type;
  lock.l_len   = 1;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  return fcntl(_fd, flag ? F_SETLKW : F_SETLK, &lock);
};

inline int FLock::doit(off_t offset, off_t length, int type, bool flag)
{
  struct flock lock;
  lock.l_type  = type;
  lock.l_len   = length;
  lock.l_start = offset;
  lock.l_whence = SEEK_SET;
  return fcntl(_fd, flag ? F_SETLKW : F_SETLK, &lock);
};

inline pid_t FLock::locked(mode m) const
//. return the ID of the process currently holding the lock or 0.
{
  struct flock lock;
  lock.l_type  = m;
  lock.l_len   = 1;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  if (fcntl(_fd, F_GETLK, &lock) < 0);// throw
  return lock.l_type == F_UNLCK ? 0 : lock.l_pid;
};

};

#endif /* _FLock_hh */
