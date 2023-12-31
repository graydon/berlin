/*$Id: logbuf.hh,v 1.5 2001/03/21 06:28:23 stefan Exp $
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
#ifndef _logbuf_hh
#define _logbuf_hh

#include <streambuf.h>
#include <cstring>

namespace Prague
{

class logbuf : public std::streambuf
{
  typedef char char_type;
  typedef int int_type;
public:
  logbuf(size_t size) : wrapped(false) { char_type *p = new char_type[size]; setp(p, p + size);}
  ~logbuf() { delete [] pbase();}
  void clear() { setp(pbase(), epptr()); wrapped = false;}
  void dump(std::ostream &);

  int_type sputc(char_type c);
  std::streamsize xsputn(const char_type *s, std::streamsize n);
private:
  bool wrapped  : 1;
};

};

#endif /* _logbuf_hh */
