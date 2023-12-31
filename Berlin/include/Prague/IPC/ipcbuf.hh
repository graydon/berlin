/*$Id: ipcbuf.hh,v 1.12 2001/03/28 06:09:47 stefan Exp $
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
#ifndef _Prague_ipcbuf_hh
#define _Prague_ipcbuf_hh

#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Thread.hh>
#include <streambuf.h>

namespace Prague
{

//. ipcbuf is a streambuffer for inter process communication, i.e. pipes, sockets, ptys. 
//. The low level reading/writing is based on file descriptors. The difference between an 
//. ipcbuf and a normal filebuf is the meaning of EOF: in the context of IPC an EOF means 
//. that the connection is closed. This case is not handled by the stream but by the 
//. corresponding Agent. 
//. If the ipcbuf is in nonblocking mode, it returns eof if the underlying read/write 
//. causes an EAGAIN error (operation would block). If a real EOF is encountered, a flag 
//. is set so the corresponding agent may terminate the process (or reestablish the connection...)}
class ipcbuf : public std::streambuf
{
public:
  typedef char              char_type;
  typedef std::streampos    pos_type;
  typedef std::streamoff    off_type;
  typedef int               int_type;
  typedef std::ios::seekdir seekdir;
public:
  //. create a new ipcbuf for the given file descriptor
  ipcbuf(int);
  virtual ~ipcbuf();
  //. return true if read wouldn't block
  bool readready() const;
  //. return true if write wouldn't block
  bool writeready() const;
  bool exceptionpending() const;
  //. try to read n bytes into buf, return the number of bytes actually read
  virtual std::streamsize sys_read(char *buf, std::streamsize n);
  //. try to write n bytes from buf, return the number of bytes actually written
  virtual std::streamsize sys_write(const char *buf, std::streamsize n);
//   virtual int write (const void *, int);
//   virtual int read (void *, int);
  //. return the file descriptor for that buffer
  int  fd() const { return _fd;}
  //. set the file descriptor
  void fd(int f) { _fd = f;}
  bool oob() const { return _oobbit;}
  bool oob(bool f) { _oobbit = f;}
  //. set the buffer to nonblocking mode if flag is true, to blocking mode otherwise
  void async(bool flag);
  //. return true if the buffer is in nonblocking mode, false otherwise
  bool async() const;
  //. did we encounter EOF ?
  bool eof() const { return _eofbit;}
// protected:
  //. flush the buffer
  virtual int        sync();
  //. return the number of chars in the input sequence
  virtual int        showmanyc() const;
  //. if pbase () == 0, no write is allowed and thus return EOF.
  //. if c == EOF, we sync the output and return 0.
  //. if pptr () == epptr (), buffer is full and thus sync the output, insert c into buffer, and return c.
  virtual int_type   overflow(int c = EOF);
  virtual int_type   underflow();
  virtual int_type   uflow();
  virtual int_type   pbackfail(int c = EOF);
  virtual std::streamsize xsputn(const char *, std::streamsize);
  virtual std::streamsize xsgetn(char *, std::streamsize);
private:
  int	_fd;
  int   _stmo;        // -1==block, 0==poll, >0 == waiting time in secs
  int   _rtmo;        // -1==block, 0==poll, >0 == waiting time in secs
  bool  _oobbit : 1; // check for out-of-band byte while reading
  bool  _eofbit : 1; // connection closed
};

};

#endif
