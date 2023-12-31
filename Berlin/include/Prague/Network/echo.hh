/*$Id: echo.hh,v 1.4 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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

#ifndef _echo_hh
#define _echo_hh

#include <Prague/Network/protocol.hh>

namespace Prague
{

class echo: public protocol
{
public:
  class echobuf: public protocol::protocolbuf
  {
  public:
//     echobuf (sockinetbuf &si): protocol::protocolbuf(si) {}
    echobuf (protocol::p_name pname) : protocol::protocolbuf (pname) {}
    virtual void        serve_clients(int portno = -1);
    virtual const char *rfc_name() const { return "echo";}
    virtual const char *rfc_doc() const { return "rfc862";}
  };
// protected:
//   echo (): ios(0) {}
public:
  echo(protocol::p_name pname) : protocol(new echobuf(pname)) {}
  ~echo() { delete protocol::rdbuf();}
  echobuf *rdbuf() { return static_cast<echobuf *>(protocol::rdbuf());}
  echobuf *operator ->() { return rdbuf();}
};

};

#endif
