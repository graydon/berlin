/*$Id: logbuf.cc,v 1.3 2001/03/21 06:28:55 stefan Exp $
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

#include <Prague/Sys/logbuf.hh>
#include <iostream>
#include <ctype.h>

using namespace Prague;

logbuf::int_type logbuf::sputc(logbuf::char_type c)
{
  if (pptr() == epptr()) return EOF;
  *pptr() = c;
  pbump(1);
  if(pptr() == epptr())
    {
      setp(pbase(), epptr());
      wrapped = true;
    }
  return static_cast<int_type>(c);
}

std::streamsize logbuf::xsputn(const logbuf::char_type *s, std::streamsize n)
{
  if (pptr() == epptr()) return EOF;
  std::streamsize length = epptr() - pptr();
  if (n <= length)
    {
      memcpy (pptr (), s, n * sizeof (char_type));
      if (length == n)
	{
	  setp(pbase(), epptr());
	  wrapped = true;
	}
      else pbump(n);
      return n;
    }
  else
    {
      memcpy (pptr (), s, length * sizeof (char_type));
      setp(pbase(), epptr());
      wrapped = true;
      return length + xsputn(s + length, n - length);
    }
}

void logbuf::dump(std::ostream &os)
{
  os << "* logbuf::dump =\n";
  if (wrapped)
    for (char_type *i = pptr(); i != epptr(); i++)
      {
	if (isprint(*i) || isspace(*i)) os.put(*i);
	else os << std::hex << *i;
      }
  for (char_type *i = pbase(); i != pptr(); i++)
    {
      if (isprint(*i) || isspace(*i)) os.put(*i);
      else os << std::hex << *i;
    }
  os << "* end of logbuf::dump" << std::endl;
}
