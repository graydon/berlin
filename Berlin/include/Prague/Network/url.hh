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
/*$Id: url.hh,v 1.2 1999/08/30 14:42:06 gray Exp $*/
#ifndef _url_hh
#define _url_hh

#include <string>

namespace Prague
{

class url
{
public:
  url(const string &);
  url(const url &, const string &);
//   operator const string &() const;
  const string &scheme() const { return s;}
  const string &user() const { return u;}
  const string &password() const { return pw;}
  const string &hostname() const { return h;}
  const string &path() const { return p;}
  void setPath(const char *pp) { p = pp;}
  const string &fragment() const { return f;}
  const string &query() const { return q;}
  void setQuery(const string &qq) { q = qq;}
  const string &parameters() const { return pa;}
  int port() const { return po;}
  static void encode(string &);
  static void decode(string &);
protected:
  string s;
  string u;
  string pw;
  string h;
  string p;
  string f;
  string q;
  string pa;
  int    po;
  void parse(string);
};

};

#endif /* _url_hh */
