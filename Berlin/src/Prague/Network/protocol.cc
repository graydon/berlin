/*$Id: protocol.cc,v 1.2 1999/07/23 19:01:38 gray Exp $
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

#include <Prague/Network/protocol.hh>
#include <cerrno>

using namespace Prague;

const char *protocol::protocolbuf::protocol_name () const
{
  if (pn == protocol::tcp) return "tcp";
  if (pn == protocol::udp) return "udp";
  return 0;
}

void protocol::protocolbuf::connect ()
{
  if (pn == protocol::nil) throw sockerr (EPROTONOSUPPORT);
  sockinetbuf::connect (localhost (), rfc_name (), protocol_name ());
}

void protocol::protocolbuf::connect (unsigned long addr)
  // addr is in host byte order
{
  if (pn == protocol::nil) throw sockerr (EPROTONOSUPPORT);
  sockinetbuf::connect (addr, rfc_name (), protocol_name ());
}

void protocol::protocolbuf::connect (const char* host)
{
  if (pn == protocol::nil) throw sockerr (EPROTONOSUPPORT);
  sockinetbuf::connect (host, rfc_name (), protocol_name ());
}

void protocol::protocolbuf::connect (const char* host, int portno)
{
  if (pn == protocol::nil) throw sockerr (EPROTONOSUPPORT);
  sockinetbuf::connect (host, portno);
}

