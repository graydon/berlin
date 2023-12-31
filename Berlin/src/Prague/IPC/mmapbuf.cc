/*$Id: mmapbuf.cc,v 1.4 2001/03/25 08:25:16 stefan Exp $
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
#include "Prague/IPC/mmapbuf.hh"

using namespace Prague;

mmapbuf::mmapbuf(int fd, int mode)
  : mmap(fd, -1, MMap::read|MMap::write, MMap::shared), lock(fd)
{
  if (mode == std::ios::in)
    {
      char_type *gbuf = reinterpret_cast<char_type *>(mmap.addr());
      setg(gbuf, gbuf, gbuf + mmap.size());
    }
  else if (mode == std::ios::out)
    {
      char_type *pbuf = reinterpret_cast<char_type *>(mmap.addr());
      setp(pbuf, pbuf + mmap.size());
    }
  else std::cerr << "mmapbuf::mmapbuf : invalid open mode" << std::endl;
}

mmapbuf::mmapbuf(const std::string &file, size_t length, int mode)
  : mmap(file, length, MMap::read|MMap::write, MMap::shared), lock(file, false)
{
  if (mode == std::ios::in)
    {
      char_type *gbuf = reinterpret_cast<char_type *>(mmap.addr());
      if (gbuf) setg(gbuf, gbuf, gbuf + mmap.size());
    }
  else if (mode == std::ios::out)
    {
      char_type *pbuf = reinterpret_cast<char_type *>(mmap.addr());
      if (pbuf) setp(pbuf, pbuf + mmap.size());
    }
  else std::cerr << "mmapbuf::mmapbuf : invalid open mode" << std::endl;
}

mmapbuf::~mmapbuf()
{
  overflow (EOF); // flush write buffer
}

bool mmapbuf::readready() const
{
  return true;
}

bool mmapbuf::writeready() const
{
  return true;
}

bool mmapbuf::exceptionpending() const
{
  return false;
}

void mmapbuf::setnonblocking(bool flag)
{
}

bool mmapbuf::nonblocking() const
{
  return true;
}

int mmapbuf::sync()
{
  mmap.sync();
  return 0;
}

int mmapbuf::showmanyc() const
{
  if (gptr() && gptr() < egptr()) return egptr() - gptr();
  return 0;
}

mmapbuf::int_type mmapbuf::overflow(int c)
{
  return EOF;
}

mmapbuf::int_type mmapbuf::underflow()
{
  return EOF;
}

mmapbuf::int_type mmapbuf::uflow()
{
  int_type ret = underflow ();
  if (ret == EOF) return EOF;
  gbump(1);
  return ret;
}

mmapbuf::int_type mmapbuf::pbackfail(int c)
{
  return EOF;
}

std::streamsize mmapbuf::xsputn(const mmapbuf::char_type *s, std::streamsize n)
{
  int wval = epptr() - pptr();
  if (n <= wval)
    {
      memcpy(pptr(), s, n * sizeof (char_type));
      pbump(n);
      return n;
    }
  memcpy (pptr(), s, wval * sizeof (char_type));
  pbump(wval);
  if (overflow() != EOF) return wval + xsputn (s + wval, n - wval);
  return wval;
}

std::streamsize mmapbuf::xsgetn(mmapbuf::char_type *s, std::streamsize n)
{
  int rval = showmanyc();
  if (rval >= n)
    {
      memcpy(s, gptr(), n * sizeof (char_type));
      gbump(n);
      return n;
    }
  memcpy(s, gptr(), rval * sizeof (char_type));
  gbump (rval);
  if (underflow() != EOF) return rval + xsgetn(s + rval, n - rval);
  return rval;
  return 0;
}
