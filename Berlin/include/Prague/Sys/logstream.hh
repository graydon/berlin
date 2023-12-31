/*$Id: logstream.hh,v 1.3 2001/04/18 06:07:26 stefan Exp $
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
#ifndef _logstream_hh
#define _logstream_hh

#include <Prague/Sys/logbuf.hh>
#include <iostream>

namespace Prague
{

class logstream : public std::ostream
{
public:
  logstream(logbuf *lb) : std::ostream(lb) {}
  logbuf *rdbuf () { return static_cast<logbuf *> (std::ostream::rdbuf());}
  logbuf *operator -> () { return rdbuf();}
  void dump(std::ostream &os) { rdbuf()->dump(os);}
private:
};

};

#endif /* _logstream_hh */
