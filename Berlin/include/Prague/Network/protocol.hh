/*$Id: protocol.hh,v 1.1 1999/07/23 21:06:48 gray Exp $
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

#ifndef _protocol_hh
#define _protocol_hh

#include <Prague/IPC/sockstream.hh>

namespace Prague
{

class protocol: public iosockstream
{
public:
  enum p_name
  {
    nil = 0,
    tcp = sockbuf::sock_stream,
    udp = sockbuf::sock_dgram
  };

  class protocolbuf: public sockinetbuf
  {
  private:
    protocol::p_name pn;
    void bind (sockaddr& sa) { sockbuf::bind (sa);}
    void connect (sockaddr& sa) { sockbuf::connect (sa);}
  public:
    protocolbuf (sockinetbuf& si): sockinetbuf (si), pn (protocol::nil) {}
    protocolbuf (protocol::p_name pname)
      : sockinetbuf ((sockbuf::type) pname, 0), pn (pname) {}

    void                bind () { serve_clients ();}
    void                connect ();
    void                connect (unsigned long addr);
    void                connect (const char *host);
    void                connect (const char *host, int portno);

    const char         *protocol_name () const;
    virtual void        serve_clients (int portno = -1) = 0;
    virtual const char *rfc_name () const = 0;
    virtual const char *rfc_doc  () const = 0;
  };
  protocol (): ios (0) {}
};

};

#endif /* _protocol_hh */
