/*$Id: FdSet.hh,v 1.5 2001/01/15 02:49:18 stefan Exp $
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
#ifndef _FdSet_hh
#define _FdSet_hh

#include <sys/time.h>
#include <sys/types.h>
#include <cstring> // some platforms seem to need this (solaris, bsd)

namespace Prague
{

//. FdSet is a helper class used when selecting available fds for non-blocking i/o
class FdSet
{
public:
  FdSet() : m(-1) { FD_ZERO(&fds);}
  FdSet(const FdSet &F) : fds(F.fds), m(F.m) {}
  ~FdSet() {}
  FdSet &operator = (const FdSet &F) { fds = F.fds; m = F.m; return *this;}
  //. add a fd to the set
  void set(int fd) { FD_SET(fd, &fds); if (fd > m) m = fd;}
  //. return whether the given fd is available for non-blocking i/o
  bool isset(int fd) const { return FD_ISSET(fd, &fds);}
  //. clear fd from the set
  void clear(int fd) { FD_CLR(fd, &fds); if (fd == m) for (int i = 0; i < fd - 1; i++) if (isset(fd)) m = fd;}
  //. return max fd
  int max() const { return m;}
  operator fd_set *() { return &fds;}
protected:
  fd_set fds;
  int m;
};

};

#endif
