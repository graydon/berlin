/*$Id: gzstream.hh,v 1.7 2001/03/27 05:38:42 stefan Exp $
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
#ifndef _gzstream_hh
#define _gzstream_hh

#include <Prague/Filter/gzbuf.hh>
#include <iostream>

namespace Prague
{

class gzifstream : public std::istream
{
public:
  gzifstream() : std::istream(new gzbuf()) {}
  gzifstream(const char *name, int mode = std::ios::in) : std::istream(new gzbuf()) { open(name, mode);}
  gzifstream(int fd, int mode = std::ios::in) : std::istream(new gzbuf()) { attach(fd, mode);}
  virtual ~gzifstream() { delete rdbuf();}
  gzbuf *rdbuf() { return static_cast<gzbuf *>(std::istream::rdbuf());}
  void attach(int fd, int mode) { if (!gzifstream::rdbuf()->attach(fd, mode)) clear(std::ios::failbit|std::ios::badbit); else clear();}
  void open(const char *name, int mode) { if (!gzifstream::rdbuf()->open(name, mode)) clear(std::ios::failbit|std::ios::badbit); else clear();}
  void close() { if (!rdbuf()->close()) clear(std::ios::failbit | std::ios::badbit);}
};

class gzofstream : public std::ostream
{
  friend gzofstream &set_compressionlevel(gzofstream &, int);
  friend gzofstream &set_compressionstrategy(gzofstream &, int);
public:
  gzofstream() : std::ostream(new gzbuf()) {}
  gzofstream(const char *name, int mode = std::ios::out) : std::ostream(new gzbuf()) { open(name, mode);}
  gzofstream(int fd, int mode = std::ios::out) : std::ostream(new gzbuf()) { attach(fd, mode);}
  virtual ~gzofstream() { delete rdbuf();}
  gzbuf *rdbuf() { return static_cast<gzbuf *>(std::ostream::rdbuf());}
  void attach(int fd, int mode) { if (!gzofstream::rdbuf()->attach(fd, mode)) clear(std::ios::failbit|std::ios::badbit); else clear();}
  void open(const char *name, int mode) { if (!gzofstream::rdbuf()->open(name, mode)) clear(std::ios::failbit|std::ios::badbit); else clear();}
  void close() { if (!rdbuf()->close()) clear(std::ios::failbit | std::ios::badbit);}
};

template<class T> class gzomanip
{
  friend gzofstream &operator << <>(gzofstream &, const gzomanip<T> &);
public:
  gzomanip(gzofstream &(*f)(gzofstream &, T), T v) : func(f), val(v) { }
private:
  gzofstream &(*func)(gzofstream &, T);
  T val;
};

template<class T> gzofstream &operator << (gzofstream &s, const gzomanip<T> &m)
{
  return (*m.func)(s, m.val);
}

inline gzofstream &set_compressionlevel(gzofstream &s, int l)
{
  (s.rdbuf())->setcompressionlevel(l);
  return s;
}

inline gzofstream &set_compressionstrategy(gzofstream &s, int l)
{
  (s.rdbuf())->setcompressionstrategy(l);
  return s;
}

inline gzomanip<int> set_compressionlevel(int l)
{
  return gzomanip<int>(&set_compressionlevel,l);
}

inline gzomanip<int> set_compressionstrategy(int l)
{
  return gzomanip<int>(&set_compressionstrategy,l);
}

};

#endif /* _gzstream_hh */















