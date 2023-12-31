/*$Id: xdrstream.hh,v 1.2 1999/04/27 20:11:10 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on binio from Dietmar Kuehl:
 *
 * Copyright (C) 1996 Dietmar Kuehl
 * Universitaet Konstanz, Lehrstuhl fuer praktische Informatik I
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
#ifndef _xdrstream_h
#define _xdrstream_h

#include <string>
#include <stdlib.h>
#include <assert.h>
#include "Prague/Filter/xdrbuf.hh"

namespace Prague
{

/* @Class{xdrios}
 *
 * @Description{ios for xdr encoding and decoding}
 */
class xdrios
{
public:
  enum operation { encode, decode};
  inline xdrios(streambuf *, operation);
  virtual ~xdrios() { xdr_destroy(xdrs); operator delete(xdrs);}
  streambuf *rdbuf() const { return sbuf; }
  void       rdbuf(streambuf *sb) { sbuf = sb; xdrbuf_reseat(xdrs, sb);}
  operator bool () const { return state == TRUE;}

  // I decided that it makes not much sense to have this stream
  // 'protected'. It may be useful to directly encode data-types using
  // the XDR stream.
  XDR *xdrstream() { return xdrs;}

  size_t width() const { return len;}
  size_t width(size_t s){ size_t rc = len; len = s; return rc;}
protected:
  bool_t    state;
private:
  xdrios(xdrios const &);              // copying is not supported
  xdrios &operator = (xdrios const &); // assignment is not supported
  streambuf *sbuf;
  XDR       *xdrs;
  size_t    len; // used by the extractor for 'char*' similar to
		 // 'width' of 'ios', hence the naming of the
		 // corresponding functions below
};

/* @Class{oxdrstream : public xdrios}
 *
 * @Description{XDR encoder using stream notation}
 */
class oxdrstream : public xdrios
{
private:
  oxdrstream(oxdrstream const &);              // copying is not supported
  oxdrstream &operator = (oxdrstream const &); // assignment is not supported

public:
  oxdrstream(streambuf *sb) : xdrios(sb, encode) {}
  ~oxdrstream() {}

  oxdrstream &operator << (bool b)           { bool_t tbool = b ? TRUE : FALSE; state = state == TRUE && xdr_bool(xdrstream(), &tbool); return *this;}

  oxdrstream &operator << (char c)           { state = state == TRUE && xdr_char(xdrstream(), &c); return *this;}
  oxdrstream &operator << (unsigned char c)  { state = state == TRUE && xdr_u_char(xdrstream(), &c); return *this;}
  oxdrstream &operator << (signed char c)    { state = state == TRUE && xdr_char(xdrstream(), reinterpret_cast<char *> (&c)); return *this;}

  oxdrstream &operator << (double d)         { state = state == TRUE && xdr_double(xdrstream(), &d); return *this;}
  oxdrstream &operator << (float f)          { state = state == TRUE && xdr_float(xdrstream(), &f); return *this;}

  oxdrstream &operator << (int i)            { state = state == TRUE && xdr_int(xdrstream(), &i); return *this;}
  oxdrstream &operator << (long l)           { state = state == TRUE && xdr_long(xdrstream(), &l); return *this;}
  oxdrstream &operator << (short s)          { state = state == TRUE && xdr_short(xdrstream(), &s); return *this;}

  oxdrstream &operator << (unsigned int i)   { state = state == TRUE && xdr_u_int(xdrstream(), &i); return *this;}
  oxdrstream &operator << (unsigned long l)  { state = state == TRUE && xdr_u_long(xdrstream(), &l); return *this;}
  oxdrstream &operator << (unsigned short s) { state = state == TRUE && xdr_u_short(xdrstream(), &s); return *this;}

  inline oxdrstream &operator << (char const *);
  inline oxdrstream &operator << (const string &);
};

/* @Class{ofxdrstream : public oxdrstream}
 *
 * @Description{output data in XDR format to a file}
 */
class ofxdrstream : public oxdrstream
{
private:
  ofxdrstream(ofxdrstream const &);              // copying is not supported
  ofxdrstream &operator = (ofxdrstream const &); // assignment is not supported
public:
  ofxdrstream(char const *fname) : oxdrstream((new filebuf())->open(fname, ios::out|ios::binary)) {}
  ~ofxdrstream() { filebuf *fb = static_cast<filebuf *> (rdbuf()); rdbuf(0); delete fb;}
};

/* @Class{ixdrstream : public xdrios}
 *
 * @Description{XDR decoder using stream notation}
 */
class ixdrstream : public xdrios
{
private:
  ixdrstream(ixdrstream const &);              // copying is not supported
  ixdrstream &operator = (ixdrstream const &); // assignment is not supported

public:
  ixdrstream(streambuf *sb) : xdrios(sb, decode) {}
  ~ixdrstream() {}

  ixdrstream &operator >> (bool &b) { bool_t tbool; state = state == TRUE && xdr_bool(xdrstream(), &tbool); b = tbool == TRUE ? true : false; return *this;}

  ixdrstream &operator >> (char &c)           { state = state == TRUE && xdr_char(xdrstream(), &c); return *this;}
  ixdrstream &operator >> (unsigned char &c)  { state = state == TRUE && xdr_u_char(xdrstream(), &c); return *this;}
  ixdrstream &operator >> (signed char &c)    { state = state == TRUE && xdr_char(xdrstream(), reinterpret_cast<char *> (&c)); return *this; }

  ixdrstream &operator >> (double &d)         { state = state == TRUE && xdr_double(xdrstream(), &d); return *this; }
  ixdrstream &operator >> (float &f)          { state = state == TRUE && xdr_float(xdrstream(), &f); return *this; }

  ixdrstream &operator >> (int &i)            { state = state == TRUE && xdr_int(xdrstream(), &i); return *this; }
  ixdrstream &operator >> (long &l)           { state = state == TRUE && xdr_long(xdrstream(), &l); return *this; }
  ixdrstream &operator >> (short &s)          { state = state == TRUE && xdr_short(xdrstream(), &s); return *this; }

  ixdrstream &operator >> (unsigned int &i)   { state = state == TRUE && xdr_u_int(xdrstream(), &i); return *this; }
  ixdrstream &operator >> (unsigned long &l)  { state = state == TRUE && xdr_u_long(xdrstream(), &l); return *this; }
  ixdrstream &operator >> (unsigned short &s) { state = state == TRUE && xdr_u_short(xdrstream(), &s); return *this; }

  inline ixdrstream &operator >> (char *);
  inline ixdrstream &operator >> (string &);
};
  
/* @Class{ifxdrstream : public ixdrstream}
 *
 * @Description{input data in XDR format from a file}
 */
class ifxdrstream : public ixdrstream
{
private:
  ifxdrstream(ifxdrstream const &);             // copying is not supported
  ifxdrstream &operator = (ifxdrstream const &); // assignment is not supported
public:
  ifxdrstream(char const *fname) : ixdrstream((new filebuf())->open(fname, ios::in|ios::binary)) {}
  ~ifxdrstream() { filebuf *fb = static_cast<filebuf *> (rdbuf()); rdbuf(0); delete fb;}
};

inline xdrios::xdrios(streambuf *sb, xdrios::operation op)
  : state(TRUE), sbuf(sb), xdrs(static_cast<XDR *> (operator new(sizeof(XDR)))), len(0)
{
  xdrbuf_create(xdrs, sb, op == encode? XDR_ENCODE: XDR_DECODE);
}

inline oxdrstream &oxdrstream::operator << (char const *str)
{
  unsigned int len = strlen(str) + 1; // save the termination character, too
  state = state == TRUE && xdr_bytes(xdrstream(), const_cast<char **> (&str), &len, len);
  return *this;
}

inline oxdrstream &oxdrstream::operator << (const string &str)
{
  return *this << str.c_str();
}

inline ixdrstream &ixdrstream::operator >> (char *str)
{
  assert(width() != 0);
  size_t len = width();
  state = state == TRUE && xdr_bytes(xdrstream(), &str, &len, len);
  return *this;
}

inline ixdrstream &ixdrstream::operator >> (string &str)
{
  size_t len = width();
  char *buf = new char [len];
  state = state == TRUE && xdr_bytes(xdrstream(), &buf, &len, len);
  str = buf;
  delete [] buf;
  return *this;
}

};

#endif /* xdrstream_hh */
