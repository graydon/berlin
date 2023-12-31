/*$Id: bzbuf.cc,v 1.2 1999/04/27 20:09:49 gray Exp $
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
#include "Prague/Filter/bzbuf.hh"
#include <iostream>

using namespace Prague;

bzbuf::bzbuf(streambuf *b, int mode)
  : comp(new char_type [BUFSIZ]), back(b)
{
  char_type *buf = new char_type [BUFSIZ];
  if (mode & ios::in)
    {
      setg (buf, buf + BUFSIZ, buf + BUFSIZ);
      next_in = comp;
      avail_in = BUFSIZ;
      pout = eback();
    }
  else if (mode & ios::out)
    {
      setp (buf, buf + BUFSIZ);
      next_out = comp;
      avail_out = BUFSIZ;
      pout = pbase();
    }
  bzalloc = 0;
  bzfree = 0;
  opaque = 0;
  int blocksize = 9;
  int verbosity = 4;
  int workFactor = 30;
  int small = 0;
  if (mode &= ios::out)
    int ret = bzCompressInit (this, blocksize, verbosity, workFactor);
  else
    int ret = bzDecompressInit (this, verbosity, small);
}

bzbuf::~bzbuf()
{
  overflow(EOF);
  if (pbase())
    {
      bool done;
      do
	{
	  done = bzCompress(this, BZ_FINISH) == BZ_STREAM_END;
	  if (avail_out) pout = next_out;
	  else
	    {
	      pout = next_out = cbase();
	      avail_out = BUFSIZ;
	    }
	  streamsize l = next_out - cout();//comp + BUFSIZ - next_out;
	  if (l) l = back->sputn(cout(), l), ::cout << "writing " << l << " bytes " << endl;
	  next_out -= l;
	  avail_out += l;
	}
      while (!done);
      bzCompressEnd(this);
      delete [] pbase();
    }
  else delete [] eback();
  delete [] comp;
  back->sync();
}

int bzbuf::sync()
{
  if (pptr() && avail_in)
    {
      bzCompress(this, BZ_RUN);
      if (cin() == epptr()) next_in = pbase();
      setp (cin(), epptr());
      
      streamsize l = next_out - cout();
      /*
       * what if sputn returns less than l ???
       */
      if (l) l = back->sputn(cout(), l), ::cout << "writing " << l << " bytes " << endl;
      pout += l;
      if (avail_out) pout = next_out;
      else
	{
	  pout = next_out = cbase();
	  avail_out = BUFSIZ;
	}
    }
  return 0;
}

bzbuf::int_type bzbuf::overflow (int c)
{
  if (pbase () == 0) return EOF;
  if (c == EOF) return sync();
  if (pptr () == epptr()) sync();
  *pptr() = (char_type) c;
  pbump (1);
  avail_in++;
  return c;
}

bzbuf::int_type bzbuf::underflow ()
{
  if (gptr() == 0) return EOF;
  if (gptr() < next_out) return (unsigned char) *gptr();
  if (!avail_out)
    {
      next_out = eback();
      avail_out = egptr() - eback();
    }
  if (!avail_in)
    {
      if (cin() == ecptr()) next_in = cbase();
      streamsize l = back->sgetn(cin(), ecptr() - cin());
      if (l == EOF) return EOF;
      avail_in += l;
    }
  bzDecompress(this);  
  setg (cout(), cout(), next_out);
  return (unsigned char) *gptr();
}

/* @Method{bzbuf::int_type ipcbuf::uflow ()}
 *
 * @Description{}
 */
bzbuf::int_type bzbuf::uflow ()
{
  int_type ret = underflow ();
  if (ret == EOF) return EOF;
  gbump(1);
  pout++;
  return ret;
}
