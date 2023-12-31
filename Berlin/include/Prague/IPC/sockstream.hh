/*$Id: sockstream.hh,v 1.5 1999/08/30 14:42:06 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _sockstream_hh
#define _sockstream_hh

#include <Prague/IPC/sockbuf.hh>
#include <iostream>

namespace Prague
{

class isockstream : public istream
  //. an istream for sockets
{
public:
  isockstream(sockbuf *sb) : ios (sb) {}
  virtual ~isockstream () {}        
  sockbuf *rdbuf () { return static_cast<sockbuf *> (ios::rdbuf()); }
  sockbuf *operator -> () { return rdbuf(); }
protected:
  isockstream () : ios (0) {}
};

class osockstream : public ostream
  //. an ostream for sockets
{
public:
  osockstream(sockbuf *sb) : ios (sb) {}
  virtual ~osockstream () {}
  sockbuf *rdbuf () { return static_cast<sockbuf *> (ios::rdbuf());}
  sockbuf *operator -> () { return rdbuf();}
protected:
  osockstream () : ios (0) {}
};

class iosockstream : public iostream
//. an iostream for sockets
{
public:
  iosockstream(sockbuf* sb): ios (sb) {}
  virtual ~iosockstream () {}
  sockbuf *rdbuf () { return static_cast<sockbuf *> (ios::rdbuf());}
  sockbuf *operator -> () { return rdbuf();}
protected:
  iosockstream () : ios (0) {}
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

class isockunix : public isockstream
//. an istream for unix sockets
{
public:
  isockunix (int s) : ios (new sockunixbuf (s)) {}
  isockunix (const sockunixbuf& sb) : ios (new sockunixbuf (sb)) {}
  isockunix (sockbuf::type ty = sockbuf::sock_stream, int proto = 0) : ios (new sockunixbuf (ty, proto)) {}
  ~isockunix() { delete ios::rdbuf ();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *> (rdbuf ()); }
};

class osockunix : public osockstream
//. an ostream for unix sockets
{
public:
  osockunix (int s) : ios (new sockunixbuf (s)) {}
  osockunix (const sockunixbuf& sb) : ios (new sockunixbuf (sb)) {}
  osockunix (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : ios (new sockunixbuf (ty, proto)) {}
  ~osockunix () { delete ios::rdbuf ();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *> (rdbuf()); }
};

class iosockunix : public iosockstream
//. an iostream for unix sockets
{
public:
  iosockunix (int s) : ios (new sockunixbuf (s)) {}
  iosockunix (const sockunixbuf &sb) : ios (new sockunixbuf (sb)) {}
  iosockunix (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : ios (new sockunixbuf (ty, proto)) {}
  ~iosockunix () { delete ios::rdbuf ();}
  sockunixbuf *operator -> () { return static_cast<sockunixbuf *> (rdbuf()); }
};

class isockinet : public isockstream
//. an istream for internet sockets
{
public:
  isockinet (int s) : ios (new sockinetbuf (s)) {}
  isockinet (const sockinetbuf &sb) : ios (new sockinetbuf (sb)) {}
  isockinet (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : ios (new sockinetbuf (ty, proto)) {}
  ~isockinet () { delete ios::rdbuf();}
  sockinetbuf *rdbuf () { return static_cast<sockinetbuf *> (ios::rdbuf());}
  sockinetbuf *operator -> () { return rdbuf();}
};

class osockinet : public osockstream
//. an ostream for internet sockets
{
public:
  osockinet (int s) : ios (new sockinetbuf (s)) {}
  osockinet (const sockinetbuf &sb) : ios (new sockinetbuf (sb)) {}
  osockinet (sockbuf::type ty=sockbuf::sock_stream, int proto = 0) : ios (new sockinetbuf (ty, proto)) {}
  ~osockinet () { delete ios::rdbuf();}
  sockinetbuf *rdbuf () { return static_cast<sockinetbuf *> (ios::rdbuf()); }
  sockinetbuf *operator -> () { return rdbuf();}
};

class iosockinet : public iosockstream
//. an iostream for internet sockets
{
public:
  iosockinet (int s) : ios (new sockinetbuf (s)) {}
  iosockinet (const sockinetbuf &sb) : ios (new sockinetbuf (sb)) {}
  iosockinet (sockbuf::type ty = sockbuf::sock_stream, int proto = 0) : ios (new sockinetbuf (ty, proto)) {}
  ~iosockinet () { delete ios::rdbuf();}
  sockinetbuf *rdbuf () { return static_cast<sockinetbuf *> (ios::rdbuf());}
  sockinetbuf *operator -> () { return rdbuf();}
};

};

#endif /* _sockstream_hh */
