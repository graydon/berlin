/*$Id: gzbuf.hh,v 1.4 2001/03/27 05:38:42 stefan Exp $
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
#ifndef _Prague_gzbuf_h
#define _Prague_gzbuf_h

#include <iostream>
#include "zlib.h"

namespace Prague
{

class gzbuf : public std::streambuf
{
public:
  gzbuf();
  virtual ~gzbuf();
  gzbuf *open(const char *, int);
  gzbuf *attach(int, int);
  gzbuf *close();
  int setcompressionlevel(short);
  int setcompressionstrategy(short);
  int is_open() const {return _file != 0;}
  virtual std::streampos seekoff(std::streamoff, std::ios::seekdir, int);
  virtual int sync();
protected:
  virtual int underflow();
  virtual int overflow(int = EOF);
private:
  int flushbuf();
  int fillbuf();
  gzFile _file;
  int    _mode;
  bool   _owner;
  char  *_buf;
};

};

#endif /* _gzbuf_h */















