/*$Id: url.hh,v 1.4 2001/03/25 08:25:16 stefan Exp $
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
#ifndef _Prague_url_hh
#define _Prague_url_hh

#include <string>

namespace Prague
{

class url
{
public:
  url(const std::string &);
  url(const url &, const std::string &);
//   operator const std::string &() const;
  const std::string &scheme() const { return s;}
  const std::string &user() const { return u;}
  const std::string &password() const { return pw;}
  const std::string &hostname() const { return h;}
  const std::string &path() const { return p;}
  void setPath(const char *pp) { p = pp;}
  const std::string &fragment() const { return f;}
  const std::string &query() const { return q;}
  void setQuery(const std::string &qq) { q = qq;}
  const std::string &parameters() const { return pa;}
  int port() const { return po;}
  static void encode(std::string &);
  static void decode(std::string &);
private:
  void parse(const std::string &);
  std::string s;
  std::string u;
  std::string pw;
  std::string h;
  std::string p;
  std::string f;
  std::string q;
  std::string pa;
  int         po;
};

};

#endif /* _url_hh */
