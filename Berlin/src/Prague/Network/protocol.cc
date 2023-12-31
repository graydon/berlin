/*$Id: protocol.cc,v 1.6 2001/03/25 08:25:16 stefan Exp $
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

#include <Prague/Network/protocol.hh>
#include <cerrno>

using namespace Prague;

const char *protocol::protocolbuf::protocol_name() const
{
  if (pn == protocol::tcp) return "tcp";
  if (pn == protocol::udp) return "udp";
  return 0;
}

void protocol::protocolbuf::connect()
{
  if (pn == protocol::nil) throw sockerr(EPROTONOSUPPORT);
  sockinetbuf::connect(sockinetaddr(localaddr().hostname(), rfc_name(), protocol_name()));
}

void protocol::protocolbuf::connect(unsigned long addr)
  // addr is in host byte order
{
  if (pn == protocol::nil) throw sockerr(EPROTONOSUPPORT);
  sockinetbuf::connect (sockinetaddr(addr, rfc_name(), protocol_name()));
}

void protocol::protocolbuf::connect(const std::string &host)
{
  if (pn == protocol::nil) throw sockerr(EPROTONOSUPPORT);
  sockinetbuf::connect(sockinetaddr(host, rfc_name(), protocol_name()));
}

void protocol::protocolbuf::connect(const std::string &host, int portno)
{
  if (pn == protocol::nil) throw sockerr(EPROTONOSUPPORT);
  sockinetbuf::connect(sockinetaddr(host, portno));
}

