/*$Id: gzbuf.cc,v 1.2 1999/04/27 20:09:49 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this file defines a C++ interface to zlib
 * written by Kevin Ruland <kevin@rodin.wustl.edu>
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
#include <memory.h>
#include "Prague/Filter/gzbuf.hh"

using namespace Prague;

gzbuf::gzbuf() : file(0), mode(0), fd_owner(false) {}
gzbuf::~gzbuf()
{
  sync();
  if (fd_owner) close();
}

gzbuf *gzbuf::open(const char *name, int io_mode)
{
  if (is_open()) return 0;
  char char_mode[10];
  char *p;
  memset(char_mode,'\0',10);
  p = char_mode;
  if (io_mode & ios::in)
    {
      mode = ios::in;
      *p++ = 'r';
    }
  else if
    (io_mode & ios::app)
    {
      mode = ios::app;
      *p++ = 'a';
    }
  else
    {
      mode = ios::out;
      *p++ = 'w';
    }
  if (io_mode & ios::binary)
    {
      mode |= ios::binary;
      *p++ = 'b';
    }
  // Hard code the compression level
  if (io_mode & (ios::out|ios::app))
    {
      *p++ = '9';
    }
  if ((file = gzopen(name, char_mode)) == 0) return 0;
  fd_owner = true;
  return this;
}

gzbuf *gzbuf::attach(int fd, int io_mode)
{
  if (is_open()) return 0;
  char char_mode[10];
  char *p;
  memset(char_mode,'\0',10);
  p = char_mode;
  if (io_mode & ios::in)
    {
      mode = ios::in;
      *p++ = 'r';
    }
  else if (io_mode & ios::app)
    {
      mode = ios::app;
      *p++ = 'a';
    }
  else
    {
      mode = ios::out;
      *p++ = 'w';
    }
  if (io_mode & ios::binary)
    {
      mode |= ios::binary;
      *p++ = 'b';
    }
  // Hard code the compression level
  if (io_mode & (ios::out|ios::app))
    {
      *p++ = '9';
    }
  if ((file = gzdopen(fd, char_mode)) == 0) return 0;
  fd_owner = false;
  return this;
}

gzbuf *gzbuf::close()
{
  if (is_open())
    {
      sync();
      gzclose(file);
      file = 0;
    }
  return this;
}

int gzbuf::setcompressionlevel(short comp_level)
{
  return gzsetparams(file, comp_level, -2);
}

int gzbuf::setcompressionstrategy(short comp_strategy)
{
  return gzsetparams(file, -2, comp_strategy);
}

streampos gzbuf::seekoff(streamoff off, ios::seek_dir dir, int which)
{
  return streampos(EOF);
}

int gzbuf::underflow()
{
  // If the file hasn't been opened for reading, error.
  if (!is_open() || !(mode & ios::in)) return EOF;
  // if a buffer doesn't exists, allocate one.
  if (!base())
    {
      if ((allocate()) == EOF) return EOF;
      setp(0,0);
    }
  else
    {
      if (in_avail()) return (unsigned char) *gptr();
      if (out_waiting())
	{
	  if (flushbuf() == EOF) return EOF;
	}
    }
  // Attempt to fill the buffer.
  int result = fillbuf();
  if (result == EOF)
    {
      // disable get area
      setg(0,0,0);
      return EOF;
    }
  return (unsigned char) *gptr();
}

int gzbuf::overflow(int c)
{
  if (!is_open() || !(mode & ios::out)) return EOF;
  if (!base())
    {
      if (allocate() == EOF) return EOF;
      setg(0,0,0);
    }
  else
    {
      if (in_avail()) return EOF;
      if (out_waiting())
	{
	  if (flushbuf() == EOF) return EOF;
	}
  }
  int bl = blen();
  setp( base(), base() + bl);
  if (c != EOF)
    {
      *pptr() = c;
      pbump(1);
    }
  return 0;
}

int gzbuf::sync()
{
  if (!is_open()) return EOF;
  if (out_waiting()) return flushbuf();
  return 0;
}

int gzbuf::flushbuf()
{
  int n;
  char *q;
  q = pbase();
  n = pptr() - q;
  if (gzwrite(file, q, n) < n) return EOF;
  setp(0,0);
  return 0;
}

int gzbuf::fillbuf()
{
  int required;
  char *p;
  p = base();
  required = blen();
  int t = gzread(file, p, required);
  if (t <= 0) return EOF;
  setg(base(), base(), base()+t);
  return t;
}
