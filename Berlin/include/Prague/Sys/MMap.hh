/*$Id: MMap.hh,v 1.1 1999/10/14 03:32:19 gray Exp $
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
#ifndef _MMap_hh
#define _MMap_hh

#include <unistd.h>
#include <sys/mman.h>
#include <string>

class MMap
{
public:
  enum protection_t { none = PROT_NONE, read = PROT_READ, write = PROT_WRITE, exec = PROT_EXEC};
  enum map_t { fixed = MAP_FIXED, shared = MAP_SHARED, priv = MAP_PRIVATE};
  class Exception
  {
  public:
    Exception(const string &m) : msg(m) {}
    const string &what() const { return msg;}
  private:
    string msg;
  }; 
  MMap(int, int = -1, int = read|write, int = priv, void * = 0, off_t = 0);
  MMap(const string &, int = -1, int = read|write, int = priv, void * = 0, off_t = 0);
  ~MMap();
  void *addr() const { return base;}
  size_t size() const { return length;}
  void sync(ssize_t = -1, bool = true);
  void sync(void *, size_t, bool = true);
  void protect(ssize_t = -1, int = read|write);
  void protect(void *, size_t, int = read|write);
private:
  MMap(const MMap &);
  void *base;
  size_t length;
};

#endif /* _MMap_hh */
