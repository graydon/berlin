/*+P
 * This file is part of OffiX,
 * a C++ API for the X Window System and Unix
 * Copyright (C) 1995-98  Stefan Seefeld
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
 -P*/
/* $Id: cbuf.hh,v 1.1 1999/04/22 14:44:58 gray Exp $ */
#ifndef _cbuf_h
#define _cbuf_h

#include <streambuf.h>

/* @Class {cbuf : public streambuf}
 *
 * @Description {suppress comments from input}
 */
class cbuf: public streambuf
{
public:
  cbuf(streambuf *sb, char c = '#') :sbuf(sb), comment(c), newline(true) {}
protected:
  int sync() { return sbuf->sync();}
  inline int uflow();
  int sungetc() { return sbuf->sungetc();}
private:
  cbuf(cbuf const &);
  void operator= (cbuf const &);
  streambuf *sbuf;
  const char comment;
  bool newline;
};

inline int cbuf::uflow()
{
  int c = sbuf->sbumpc();
  if (c == '\n') newline = true;
  else if (c == comment)
    {
      do // for all lines starting with <comment>
	{
	  do c = sbuf->sbumpc();
	  while (c != EOF && c != '\n'); // for all letters of the line
	  if (newline && c == '\n') c = sbuf->sbumpc();
	}
      while (c == comment);
      if (c == '\n') newline = true;
    }
  else newline = false;
//   cerr << (char)c << endl;
  return c;
}

#endif /* _cbuf_h */
