/*$Id: convert.hh,v 1.1 1999/05/07 21:43:14 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _convert_hh
#define _convert_hh

#include <arpa/inet.h>

template <class T> T Net2Host(const T &);
template <class T> T Host2Net(const T &);

int Host2Net<int>(const int &t) { return htonl(t);}
int Net2Host<int>(const int &t) { return ntohl(t);}
long Host2Net<long>(const long &t) { return htonl(t);}
long Net2Host<long>(const long &t) { return ntohl(t);}
short Host2Net<short>(const short &t) { return htons(t);}
short Net2Host<short>(const short &t) { return ntohs(t);}
unsigned int Host2Net<unsigned int>(const unsigned int &t) { return htonl(t);}
unsigned int Net2Host<unsigned int>(const unsigned int &t) { return ntohl(t);}
unsigned long Host2Net<unsigned long>(const unsigned long &t) { return htonl(t);}
unsigned long Net2Host<unsigned long>(const unsigned long &t) { return ntohl(t);}
unsigned short Host2Net<unsigned short>(const unsigned short &t) { return htons(t);}
unsigned short Net2Host<unsigned short>(const unsigned short &t) { return ntohs(t);}

#endif /* _convert_hh */
