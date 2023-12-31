/*$Id: Memory.hh,v 1.6 2001/01/15 02:49:18 stefan Exp $
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
#ifndef _Prague_Memory_hh
#define _Prague_Memory_hh

#include <cstring>

namespace Prague
{

//. this is a portability wrapper, to hide platform differences,
//. as well as to hide some nasty casts
namespace Memory
{
template <class T>
T *copy(const T *from, T *to, unsigned long n) { return n > 0 ? reinterpret_cast<T *>(memmove(to, from, size_t(n))) : to;}
template <class T>
T *move(const T *from, T *to, unsigned long n) { return n > 0 ? reinterpret_cast<T *>(memmove(to, from, size_t(n))) : to;}
template <class T>
T *set(T *b, unsigned long n, T c) { return reinterpret_cast<T *>(memset(b, c, size_t(n)));}
template <class T>
T *zero(T *m, unsigned long l) { return reinterpret_cast<T *>(memset(m, 0, size_t(l)));}
template <class T>
int compare(const T *p, const T *q, unsigned long n) { return memcmp(p, q, size_t(n));}

};

};

#endif
