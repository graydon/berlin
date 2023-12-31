/*$Id: FdSet.hh,v 1.2 1999/04/27 20:11:10 gray Exp $
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
#ifndef _FdSet_hh
#define _FdSet_hh

#include <sys/time.h>
#include <sys/types.h>

#if defined __sgi__
#include <bstring.h>
#endif

namespace Prague
{

/* @Class{FdSet}
 *
 * @Description{a set of file descriptors such as used by @var{select()}}
 */
class FdSet
{
public:
  FdSet() : m(-1) { FD_ZERO(&fds);}
  FdSet(const FdSet &F) : fds(F.fds), m(F.m) {}
  ~FdSet() {}
  FdSet &operator = (const FdSet &F) { fds = F.fds; m = F.m; return *this;}
  void set(int fd) { FD_SET(fd, &fds); if (fd > m) m = fd;}
  bool isset(int fd) const { return FD_ISSET(fd, &fds);}
  void clear(int fd) { FD_CLR(fd, &fds); if (fd == m) for (int i = 0; i < fd - 1; i++) if (isset(fd)) m = fd;}
  int max() const { return m;}
  operator fd_set *() { return &fds;}
protected:
  fd_set fds;
  int m;
};

};

/* @Method{FdSet::FdSet()}
 * @Description{default constructor}
 */
/* @Method{FdSet::FdSet(const FdSet &F)}
 * @Description{copy constructor}
 */
/* @Method{FdSet::~FdSet()}
 * @Description{destructor}
 */
/* @Method{void FdSet::set(int fd)}
 * @Description{set the entry for file descriptor @var{fd}}
 */
/* @Method{void FdSet::clear(int fd)}
 * @Description{clear the entry for file descriptor @var{fd}}
 */
/* @Method{bool FdSet::isset(int fd)}
 * @Description{check the entry for file descriptor @var{fd}}
 */
/* @Method{void FdSet::max()}
 * @Description{maximal used file descriptor}
 */

#endif /* _FdSet_h */
