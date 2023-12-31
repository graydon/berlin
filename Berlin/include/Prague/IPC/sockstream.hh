/*$Id: sockstream.hh,v 1.8 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * http://www.berlin-consortium.org
 *
 * this file is based on code from the socket++ library
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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
#ifndef _Prague_sockstream_hh
#define _Prague_sockstream_hh

#include <Prague/IPC/sockbuf.hh>
#include <iostream>

namespace Prague
{

//. an istream for sockets
class isockstream : public std::istream
{
public:
  isockstream(sockbuf *sb) : std::istream(sb) {}
  virtual ~isockstream() {}
  sockbuf *rdbuf() { return static_cast<sockbuf *> (std::istream::rdbuf());}
  sockbuf *operator ->() { return rdbuf();}
};

//. an ostream for sockets
class osockstream : public std::ostream
{
public:
  osockstream(sockbuf *sb) : std::ostream(sb) {}
  virtual ~osockstream() {}
  sockbuf *rdbuf() { return static_cast<sockbuf *> (std::ostream::rdbuf());}
  sockbuf *operator ->() { return rdbuf();}
};

//. an iostream for sockets
class iosockstream : public std::iostream
{
public:
  iosockstream(sockbuf* sb): std::iostream(sb) {}
  virtual ~iosockstream() {}
  sockbuf *rdbuf() { return static_cast<sockbuf *> (std::iostream::rdbuf());}
  sockbuf *operator ->() { return rdbuf();}
};

// manipulators
// inline osockstream &crlf (osockstream &o)
// {
//   o << "\r\n";
//   o.rdbuf ()->sync ();
//   return o;
// }

// inline osockstream &lfcr (osockstream &o)
// {
//   o << "\n\r";
//   o.rdbuf ()->sync ();
//   return o;
// }

//. an istream for unix sockets
class isockunix : public isockstream
{
public:
  isockunix(int s) : isockstream(new sockunixbuf(s)) {}
//   isockunix(const sockunixbuf& sb) : ios (new sockunixbuf (sb)) {}
  isockunix(sockbuf::type ty = sockbuf::sock_stream, int proto = 0) : isockstream(new sockunixbuf(ty, proto)) {}
  ~isockunix() { delete rdbuf();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *>(isockstream::rdbuf());}
};

//. an ostream for unix sockets
class osockunix : public osockstream
{
public:
  osockunix(int s) : osockstream(new sockunixbuf(s)) {}
//   osockunix (const sockunixbuf& sb) : ios (new sockunixbuf (sb)) {}
  osockunix(sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : osockstream(new sockunixbuf(ty, proto)) {}
  ~osockunix() { delete rdbuf();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *> (osockstream::rdbuf());}
};

//. an iostream for unix sockets
class iosockunix : public iosockstream
{
public:
  iosockunix (int s) : iosockstream(new sockunixbuf(s)) {}
//   iosockunix (const sockunixbuf &sb) : ios (new sockunixbuf (sb)) {}
  iosockunix (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : iosockstream(new sockunixbuf(ty, proto)) {}
  ~iosockunix () { delete rdbuf();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *>(iosockstream::rdbuf());}
};

//. an istream for internet sockets
class isockinet : public isockstream
{
public:
  isockinet (int s) : isockstream(new sockinetbuf(s)) {}
//   isockinet (const sockinetbuf &sb) : ios (new sockinetbuf (sb)) {}
  isockinet(sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : isockstream(new sockinetbuf(ty, proto)) {}
  ~isockinet() { delete rdbuf();}
  sockinetbuf *rdbuf() { return static_cast<sockinetbuf *> (isockstream::rdbuf());}
  sockinetbuf *operator -> () { return rdbuf();}
};

//. an ostream for internet sockets
class osockinet : public osockstream
{
public:
  osockinet (int s) : osockstream(new sockinetbuf(s)) {}
//   osockinet (const sockinetbuf &sb) : ios(new sockinetbuf(sb)) {}
  osockinet (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : osockstream(new sockinetbuf (ty, proto)) {}
  ~osockinet () { delete rdbuf();}
  sockinetbuf *rdbuf() { return static_cast<sockinetbuf *> (osockstream::rdbuf());}
  sockinetbuf *operator -> () { return rdbuf();}
};

//. an iostream for internet sockets
class iosockinet : public iosockstream
{
public:
  iosockinet (int s) : iosockstream(new sockinetbuf(s)) {}
//   iosockinet (const sockinetbuf &sb) : ios (new sockinetbuf (sb)) {}
  iosockinet (sockbuf::type ty = sockbuf::sock_stream, int proto = 0) : iosockstream(new sockinetbuf(ty, proto)) {}
  ~iosockinet () { delete rdbuf();}
  sockinetbuf *rdbuf() { return static_cast<sockinetbuf *> (iosockstream::rdbuf());}
  sockinetbuf *operator -> () { return rdbuf();}
};

};

#endif
