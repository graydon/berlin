/*$Id: mmapbuf.hh,v 1.4 2001/03/25 08:25:16 stefan Exp $
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
#ifndef _mmapbuf_hh
#define _mmapbuf_hh

#include <Prague/Sys/MMap.hh>
#include <Prague/Sys/FLock.hh>
#include <streambuf.h>

namespace Prague
{

class mmapbuf : public std::streambuf
//. a streambuf for memory mapped files.
//. since in this context buffering doesn't make sense,
//. the strategy is different: client and server lock
//. a window for reading and writing respectively and
//. shift these windows after reading/writing a block of n characters
{
public:
  typedef char              char_type;
  typedef std::streampos    pos_type;
  typedef std::streamoff    off_type;
  typedef int               int_type;
  typedef std::ios::seekdir seekdir;

  mmapbuf(int, int);
  mmapbuf(const std::string &, size_t, int);
  ~mmapbuf();
  bool readready() const;
  bool writeready() const;
  bool exceptionpending() const;
  void setnonblocking(bool);
  bool nonblocking() const;
//   int write(const void *, int);
//   int read(void *, int);
protected:
  virtual int             sync();
  virtual int             showmanyc() const;
  virtual int_type        overflow(int c = EOF);
  virtual int_type        underflow();
  virtual int_type        uflow();
  virtual int_type        pbackfail(int c = EOF);
  virtual std::streamsize xsputn(const char *, std::streamsize);
  virtual std::streamsize xsgetn(char *, std::streamsize);
private:
  mmapbuf(const mmapbuf &);
  mmapbuf &operator = (const mmapbuf &);
  MMap mmap;
  FLock lock;
};

};

#endif /* _ipcbuf_hh */
