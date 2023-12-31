/*$Id: convert.hh,v 1.3 2001/01/16 01:39:42 stefan Exp $
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
#ifndef _Prague_convert_hh
#define _Prague_convert_hh

#include <arpa/inet.h>

namespace Prague
{

//. convert t from network to host byte order
template <class T> T net_to_host(const T &t);
//. convert t from host to network byte order
template <class T> T host_to_net(const T &t);

int host_to_net<int>(const int &t) { return htonl(t);}
int net_to_host<int>(const int &t) { return ntohl(t);}
long host_to_net<long>(const long &t) { return htonl(t);}
long net_to_host<long>(const long &t) { return ntohl(t);}
short host_to_net<short>(const short &t) { return htons(t);}
short net_to_host<short>(const short &t) { return ntohs(t);}
unsigned int host_to_net<unsigned int>(const unsigned int &t) { return htonl(t);}
unsigned int net_to_host<unsigned int>(const unsigned int &t) { return ntohl(t);}
unsigned long host_to_net<unsigned long>(const unsigned long &t) { return htonl(t);}
unsigned long net_to_host<unsigned long>(const unsigned long &t) { return ntohl(t);}
unsigned short host_to_net<unsigned short>(const unsigned short &t) { return htons(t);}
unsigned short net_to_host<unsigned short>(const unsigned short &t) { return ntohs(t);}

};

#endif
