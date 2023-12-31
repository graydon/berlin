/*$Id: ipcbuf.cc,v 1.8 1999/11/18 04:44:53 stefan Exp $
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
#include "Prague/IPC/ipcbuf.hh"
#include "Prague/Sys/FdSet.hh"
#include "Prague/Sys/Time.hh"
#include "Prague/Sys/Memory.hh"
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <cerrno>
#include <unistd.h>
#include <stdio.h>

using namespace Prague;

ipcbuf::ipcbuf(int mode)
  : data(new control)
{
  if (mode & ios::in)
    {
      char_type *gbuf = new char_type [BUFSIZ];
      setg (gbuf, gbuf + BUFSIZ, gbuf + BUFSIZ);
      data->gend = gbuf + BUFSIZ;
    }
  if (mode & ios::out)
    {
      char_type *pbuf = new char_type [BUFSIZ];
      setp (pbuf, pbuf + BUFSIZ);
      data->pend = pbuf + BUFSIZ;
    }
}

ipcbuf::ipcbuf(const ipcbuf &ipc)
  : streambuf(ipc), data(ipc.data)
{
  data->count++;
}

ipcbuf::~ipcbuf()
{
  overflow (EOF); // flush write buffer
  if (--data->count) return;
  delete [] pbase ();
  delete [] eback ();
  if (fd() != -1 && close (fd()) == -1) perror("ipcbuf::~ipcbuf");
  delete data;
}

ipcbuf &ipcbuf::operator = (const ipcbuf &ipc)
{
  if (this != &ipc && data != ipc.data && data->fd != ipc.data->fd)
    {
      streambuf::operator = (ipc);
      this->ipcbuf::~ipcbuf();
      // the streambuf::operator = (const streambuf&) is assumed
      // to have handled pbase () and gbase () correctly.
      data  = ipc.data;
      data->count++;
    }
  return *this;
}

bool ipcbuf::readready() const
{
  FdSet fds;
  fds.set(fd());
  Time T;
  if (select (fds.max() + 1, fds, 0, 0, &T) == 0) return true;
  return false;
}

bool ipcbuf::writeready() const
{
  FdSet fds;
  fds.set(fd());
  Time T;
  if (select (fds.max() + 1, 0, fds, 0, &T) == 0) return true;
  return false;
}

bool ipcbuf::exceptionpending() const
{
  FdSet fds;
  fds.set(fd());
  Time T;
  if (select(fds.max() + 1, 0, 0, fds, &T) == 0) return true;
  return false;
}

void ipcbuf::setnonblocking(bool flag)
{
  int flags = fcntl(fd(), F_GETFL);
  if (flag) flags |= O_NONBLOCK;
  else flags &= ~(O_NONBLOCK);
  fcntl(fd(), F_SETFL, flags);
}

bool ipcbuf::nonblocking() const
{
  int flags = fcntl(fd(), F_GETFL);
  return flags | O_NONBLOCK;
}

int ipcbuf::sync()
{
  if (pptr() && pbase() < pptr() && pptr() <= epptr())
    {
      write(pbase(), pptr() - pbase());
      setp(pbase(), (char *) data->pend);
    }
  return 0;
}

int ipcbuf::showmanyc() const
{
  if (gptr() && gptr() < egptr()) return egptr() - gptr();
  return 0;
}

ipcbuf::int_type ipcbuf::overflow(int c)
{
  if (pbase() == 0) return EOF;
  if (c == EOF) return sync();
  if (pptr() == epptr()) sync();
  *pptr() = (char_type) c;
  pbump(1);
  return c;
}

ipcbuf::int_type ipcbuf::underflow()
{
  if (gptr() == 0) return EOF; // input stream has been disabled
  if (gptr() < egptr()) return *gptr();
  ssize_t rlen = read(eback(), data->gend - eback());
  switch (rlen)
    {
    case 0: data->eofbit = true;
    case EOF: return EOF; break;
    default: setg(eback(), eback(), eback() + rlen); return *gptr(); break;
    }
}

ipcbuf::int_type ipcbuf::uflow()
{
  int_type ret = underflow();
  if (ret == EOF) return EOF;
  gbump(1);
  return ret;
}

ipcbuf::int_type ipcbuf::pbackfail(int c)
{
  return EOF;
}

streamsize ipcbuf::xsputn(const ipcbuf::char_type *s, streamsize n)
{
  int wval = epptr() - pptr();
  if (n <= wval)
    {
      Memory::copy(s, pptr(), n * sizeof(char_type));
      pbump(n);
      return n;
    }
  Memory::copy(s, pptr(), wval * sizeof(char_type));
  pbump(wval);
  if (overflow() != EOF) return wval + xsputn(s + wval, n - wval);
  return wval;
}

streamsize ipcbuf::xsgetn(ipcbuf::char_type *s, streamsize n)
{
  int rval = showmanyc ();
  if (rval >= n)
    {
      Memory::copy(gptr(), s, n * sizeof(char_type));
      gbump(n);
      return n;
    }
  Memory::copy(gptr(), s, rval * sizeof(char_type));
  gbump(rval);
  if (underflow() != EOF) return rval + xsgetn(s + rval, n - rval);
  return rval;
}

int ipcbuf::write(const void *buf, int len)
{
//   if (!writeready ()) return 0;
  int wlen = 0;
  while(len > 0)
    {
      int wval = -1;
      do wval = ::write(fd(), (char*) buf, len);
      while (wval == -1 && errno == EINTR);
      if (wval == -1)
        {
          if (errno != EAGAIN) perror("ipcbuf::write");
          return EOF;
        }
      len -= wval;
      wlen += wval;
    }
  return wlen; // == len if every thing is all right
}

int ipcbuf::read(void *buf, int len)
{
  int rval = -1;
  do
    {
      rval = ::read(fd(), (char *)buf, len);
      cout << "read " << rval << endl;
    }
  while (rval == -1 && errno == EINTR);
  cout << rval << ' ' << errno << ' ' << '(' << EINTR << ')' << endl;
  if (rval == -1 && errno != EAGAIN) perror("ipcbuf::read");
  if (rval == 0) cerr << "ipcbuf::read returned 0" << endl;
  return rval;
}
