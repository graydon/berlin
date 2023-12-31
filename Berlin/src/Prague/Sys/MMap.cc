/*$Id: MMap.cc,v 1.8 2001/04/13 23:26:25 oxygene2000 Exp $
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

#ifdef sun
#define _XPG4_2
#endif

#include "Prague/Sys/MMap.hh"
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <iostream>
#include <cerrno>
#include <cstdio>

using namespace Prague;

MMap::MMap(int fd, int l, int prot, int share, void *addr, off_t offset)
  : _base(MAP_FAILED), _length(0)
{
  struct stat sb;
  _length = l > -1 ? l : fstat(fd, &sb) == -1 ? -1 : sb.st_size;
  if (l > static_cast<int>(_length))
    {
      _length = l;
      ftruncate(fd, _length);
    }
  else if (l > 0 && l < static_cast<int>(_length)) _length = l;
  _base = mmap(addr, _length, prot, share, fd, offset);
  if (_base == MAP_FAILED) perror("MMap::MMap");
}

MMap::MMap(const std::string &filename, int l, int prot, int share, void *addr, off_t offset)
  : _base(MAP_FAILED), _length(0)
{
  int fd = open(filename.c_str(), O_RDWR|O_CREAT, 0666);
  if (fd == -1)
    {
      std::cerr << "MMap::MMap: unable to open '" << filename << '\'' << std::endl;
      return;
    }
  struct stat sb;
  _length = fstat(fd, &sb) == -1 ? -1 : sb.st_size;
  if (l > static_cast<int>(_length))
    {
      _length = l;
      ftruncate(fd, _length);
    }
  else if (l > 0 && l < static_cast<int>(_length)) _length = l;
  _base = mmap(addr, _length, prot, share, fd, offset);
  if (_base == MAP_FAILED) std::perror("MMap::MMap");
  close(fd);
}

MMap::~MMap() { if (_base != MAP_FAILED) munmap(_base, _length);}
void MMap::sync(ssize_t len, bool wait) { msync(_base, len < 0 ? _length : len, wait ? MS_SYNC : MS_ASYNC);}
void MMap::sync(void *addr, size_t len, bool wait) { msync(addr, len, wait ? MS_SYNC : MS_ASYNC);}
void MMap::protect(ssize_t len, int prot) { mprotect(_base, len < 0 ? _length : len, prot);}
void MMap::protect(void *addr, size_t len, int prot) { mprotect(addr, len, prot);}
