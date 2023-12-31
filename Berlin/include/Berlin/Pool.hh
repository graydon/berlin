/*$Id: Pool.hh,v 1.4 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _Pool_hh
#define _Pool_hh

#include <vector>

template <class T>
class Pool
{
public:
  Pool() {}
  inline ~Pool();
  inline T *allocate(unsigned int);
  inline void deallocate(T *);
private:
  struct bucket
  {
    bucket(T *d, unsigned int s) : data(d), size(s), used(true) {};
    T *data;
    unsigned int size;
    bool used;
  };
  std::vector<bucket> buckets;
};

template <class T>
inline Pool<T>::~Pool()
{
  for (std::vector<bucket>::iterator i = buckets.begin(); i != buckets.end(); ++i)
    delete (*i).data;
}

template <class T>
inline T *Pool<T>::allocate(unsigned int size)
{
  for (std::vector<bucket>::iterator i = buckets.begin(); i != buckets.end(); ++i)
    if (!(*i).used && (*i).size >= size)
      {
	(*i).used = true;
	return (*i).data;
      }
#if 1
  bucket b(static_cast<T *>(::operator new(static_cast<size_t>(size * sizeof(T)))), size);
#else
  bucket b(new T[size], size);
#endif
  buckets.push_back(b);
  return b.data;
}

template <class T>
inline void Pool<T>::deallocate(T *d)
{
  for (std::vector<bucket>::iterator i = buckets.begin(); i != buckets.end(); ++i)
    if ((*i).data == d)
      {
	(*i).used = false;
	break;
      }
};

#endif /* _Pool_hh */
