/*$Id: ptybuf.cc,v 1.12 2001/03/25 08:25:16 stefan Exp $
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
#include <Prague/IPC/ptybuf.hh>
#include <cstdio>
#include <cerrno>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <termios.h>
#include <Prague/Sys/Thread.hh>

#if defined(__linux__) || defined(__FreeBSD__) // XXX: should use configure
#  define __bsd44__
#elif defined(__sgi__)
#  define __svr4__
#endif

using namespace Prague;

inline char ctrl(char c) { return c & 0x1f;}

class ptybuf::backup
{
public:
  backup(int fd)
    {
      /*
      fstat(fd, &mode);
      tcgetattr (fd, &tios);
      uid = geteuid();
      gid = getegid();
      */
    };
  struct stat mode;
  struct termios tios;
  uid_t uid;
  gid_t gid;
};

ptybuf::ptybuf()
  : ipcbuf(std::ios::in|std::ios::out), save(0)
{
  ptydev[0] = ttydev[0] = '\0';
}

ptybuf::~ptybuf()
{
//   fclose(ptystream);
  if (save && fd() != -1)
    {
      /*
      seteuid(save->uid);
      setegid(save->gid);
      chmod(ptydev, save->mode.st_mode);
      chown(ptydev, save->mode.st_uid, save->mode.st_gid);
      tcsetattr (fd(), TCSANOW, &save->tios);
      */
    }
  delete save;
}

std::streamsize ptybuf::sys_read(char *buf, std::streamsize len)
{
  std::streamsize rval = -1;
  do rval = ::read(fd(), buf, len);
  while (rval == -1 && errno == EINTR);
  if (rval == -1 && errno == EIO) return 0;
  if (rval == -1 && errno != EAGAIN) perror("ptybuf::read");
  return rval;
}

void ptybuf::setup()
{
//   ptystream = fdopen(fd(), "r+");
  if (!save) save = new backup(fd());
}

#if defined __bsd44__
#include "ptybuf.bsd44.cc"
#elif defined __svr4__
#include "ptybuf.svr4.cc"
#else
#error sorry, ptybuf not yet implemented for this architecture
#endif
