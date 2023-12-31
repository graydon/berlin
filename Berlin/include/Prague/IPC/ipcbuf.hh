/*$Id: ipcbuf.hh,v 1.5 1999/11/12 16:41:32 stefan Exp $
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
#ifndef _ipcbuf_hh
#define _ipcbuf_hh

#include <streambuf.h>

namespace Prague
{

class ipcbuf : public streambuf
  //. ipcbuf is a streambuffer for inter process communication, i.e. pipes, sockets, ptys. 
  //. The low level reading/writing is based on file descriptors. The difference between an 
  //. ipcbuf and a normal filebuf is the meaning of EOF: in the context of IPC an EOF means 
  //. that the connection is closed. This case is not handled by the stream but by the 
  //. corresponding Agent. 
  //. If the ipcbuf is in nonblocking mode, it returns eof if the underlying read/write 
  //. causes an EAGAIN error (operation would block). If a real EOF is encountered, a flag 
  //. is set so the corresponding agent may terminate the process (or reestablish the connection...)}
{
public:
  typedef char          char_type;
  typedef streampos     pos_type;
  typedef streamoff     off_type;
  typedef int           int_type;
  typedef ios::seek_dir seekdir;

  ipcbuf(int);
  ipcbuf(const ipcbuf &);
  virtual ~ipcbuf();
  ipcbuf &operator = (const ipcbuf &);
  bool readready() const;
  //. return true if read wouldn't block
  bool writeready() const;
  //. return true if write wouldn't block
  bool exceptionpending() const;
  int write (const void *, int);
  int read (void *, int);
  int fd() const { return data->fd;}
  void setnonblocking(bool flag = true);
  //. set the buffer to nonblocking mode if <i>flag</i> is true, to blocking mode otherwise
  bool nonblocking() const;
  //. return true if the buffer is in nonblocking mode, false otherwise
  bool eof() const { return data->eofbit;}
// protected:
  virtual int        sync();
  //. flush the buffer
  virtual int        showmanyc() const;
  //. return the number of chars in the input sequence
  virtual int_type   overflow(int c = EOF);
  //. if pbase () == 0, no write is allowed and thus return EOF.
  //. if c == EOF, we sync the output and return 0.
  //. if pptr () == epptr (), buffer is full and thus sync the output, insert c into buffer, and return c.
  virtual int_type   underflow();
  //. ipcbuf::int_type ipcbuf::underflow ()
  virtual int_type   uflow();
  virtual int_type   pbackfail(int c = EOF);
  virtual streamsize xsputn(const char *, streamsize);
  virtual streamsize xsgetn(char *, streamsize);
protected:
  struct control
  {
    control() : fd(-1), count(1), stmo (-1), rtmo (-1), oobbit(false), eofbit(false), gend (0), pend (0) {}
    int	fd;
    int	count;
    int stmo;        // -1==block, 0==poll, >0 == waiting time in secs
    int rtmo;        // -1==block, 0==poll, >0 == waiting time in secs
    bool oobbit : 1; // check for out-of-band byte while reading
    bool eofbit : 1; // connection closed
    char_type *gend; // end of input buffer
    char_type *pend; // end of output buffer
  };
  control *data;  // counts the # refs to sock
};

};

#endif /* _ipcbuf_hh */
