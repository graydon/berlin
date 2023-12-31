/*$Id: FLock.hh,v 1.4 1999/10/15 17:59:07 gray Exp $
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

class FLock
 //. provide locking based on fcntl. Locks can be activated in read and in write mode.
{
public:
  enum mode {read = F_RDLCK, write = F_WRLCK};
  FLock();
  FLock(int);
  FLock(const string &, bool = true);
  ~FLock();
  const string &name() { return file;}
  bool trylock(mode m = read) { return doit(m, false) == 0;}
  bool trylock(off_t offset, off_t length, mode m = read) { return doit(offset, length, m, false) == 0;}
  bool lock(mode m = read) { return doit(m, true) == 0;}
  bool lock(off_t offset, off_t length, mode m = read) { return doit(offset, length, m, true) == 0;}
  bool unlock() { return doit(F_UNLCK, false) == 0;}
  pid_t locked(mode m) const;
private:
  int doit(int, bool);
  int doit(off_t, off_t, int, bool);
  string file;
  bool close : 1;
  bool remove: 1;
  int fd;
};

inline FLock::FLock()
//. create a FLock using a temporary file
  : file(File::tmp()), close(true), remove(true)
{
  fd = open(file.c_str(), O_RDWR|O_CREAT, 0600);
  ::write(fd, "l", 1);
};

inline FLock::FLock(int f)
//. create a FLock for a given file
  : close(false), remove(false), fd(f)
{
};

inline FLock::FLock(const string &f, bool create)
//. create a FLock using file @var{name}, create if @var{create} is true
  : file(f), close(true), remove(false)
{
  if (create) fd = open(file.c_str(), create ? O_RDWR|O_CREAT : O_RDWR, 0600);
  if (create) ::write(fd, "l", 1);
};

inline FLock::~FLock()
//. Description{close the associated file, removes it if is a temporary one.
{
  if (close) ::close(fd);
  if (remove) ::remove(file.c_str());
};

/* @Method{bool FLock::lock(FLock::mode m)}
 *
 * Description{try to activate the lock for mode @var{m}, return @var{true} if it successful}
 */
/* @Method{bool FLock::lock(FLock::mode m)}
 *
 * Description{same as @var{lock()} but block if the lock is already active untill it is released}
 */
inline int FLock::doit(int type, bool flag)
{
  struct flock lock;
  lock.l_type  = type;
  lock.l_len   = 1;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  return fcntl(fd, flag ? F_SETLKW : F_SETLK, &lock);
};

inline int FLock::doit(off_t offset, off_t length, int type, bool flag)
{
  struct flock lock;
  lock.l_type  = type;
  lock.l_len   = length;
  lock.l_start = offset;
  lock.l_whence = SEEK_SET;
  return fcntl(fd, flag ? F_SETLKW : F_SETLK, &lock);
};

inline pid_t FLock::locked(mode m) const
//. return the ID of the process currently holding the lock or 0.
{
  struct flock lock;
  lock.l_type  = m;
  lock.l_len   = 1;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  if (fcntl(fd, F_GETLK, &lock) < 0);// throw
  return lock.l_type == F_UNLCK ? 0 : lock.l_pid;
};

};

#endif /* _FLock_hh */
