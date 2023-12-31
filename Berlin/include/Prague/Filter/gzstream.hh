/*$Id: gzstream.hh,v 1.4 1999/06/18 14:08:33 gray Exp $
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
#ifndef _gzstream_hh
#define _gzstream_hh

#include <Prague/Filter/gzbuf.hh>
#include <iostream>

namespace Prague
{

class gzstream_common : virtual public ios
{  
  friend class gzifstream;
  friend class gzofstream;
  friend gzofstream &setcompressionlevel(gzofstream &, int);
  friend gzofstream &setcompressionstrategy(gzofstream &, int);
public:
  virtual ~gzstream_common() {}
  void attach(int fd, int mode) { if (!buffer.attach(fd, mode)) clear(ios::failbit|ios::badbit); else clear();}
  void open(const char *name, int mode) { if (!buffer.open(name, mode)) clear(ios::failbit|ios::badbit); else clear();}
  void close() { if (!buffer.close()) clear(ios::failbit | ios::badbit);}
protected:
  gzstream_common() : ios(gzstream_common::rdbuf()) {}
private:
  gzbuf *rdbuf() { return &buffer;}
  gzbuf buffer;
};

class gzifstream : public gzstream_common, public istream
{
public:
  gzifstream() : ios(gzstream_common::rdbuf()) { clear(ios::badbit);}
  gzifstream(const char *name, int mode = ios::in) : ios(gzstream_common::rdbuf()) { gzstream_common::open(name, mode);}
  gzifstream(int fd, int mode = ios::in) : ios(gzstream_common::rdbuf()) { gzstream_common::attach(fd, mode);}
  virtual ~gzifstream() {}
};

class gzofstream : public gzstream_common, public ostream
{
public:
  gzofstream() : ios(gzstream_common::rdbuf()) { clear(ios::badbit);}
  gzofstream(const char *name, int mode = ios::out) : ios(gzstream_common::rdbuf()) { gzstream_common::open(name, mode);}
  gzofstream(int fd, int mode = ios::out) : ios(gzstream_common::rdbuf()) { gzstream_common::attach(fd, mode);}
  virtual ~gzofstream() {}
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

inline gzofstream &setcompressionlevel(gzofstream &s, int l)
{
  (s.rdbuf())->setcompressionlevel(l);
  return s;
}

inline gzofstream &setcompressionstrategy(gzofstream &s, int l)
{
  (s.rdbuf())->setcompressionstrategy(l);
  return s;
}

inline gzomanip<int> setcompressionlevel(int l)
{
  return gzomanip<int>(&setcompressionlevel,l);
}

inline gzomanip<int> setcompressionstrategy(int l)
{
  return gzomanip<int>(&setcompressionstrategy,l);
}

};

#endif /* _gzstream_hh */















