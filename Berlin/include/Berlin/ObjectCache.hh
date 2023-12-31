/*$Id: ObjectCache.hh,v 1.3 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _ObjectCache_hh
#define _ObjectCache_hh

#include <vector>
#include <list>
#include <map>
#include <algorithm>

template <class Remote, class Local>
class ObjectCache
{
  typedef std::vector<Local *> bucket_t;
  typedef long hash_t;
  typedef std::map<hash_t, bucket_t> cache_t;
  typedef std::list<Local *> counter_t;
  struct Predicate
  {
    Predicate(Remote r) : remote(r) {}
    bool operator () (Local *l) const { return remote->_is_equivalent(l->remote);}
    Remote remote;
  };
public:
  ObjectCache(int i, int b = 193) : _items(i), _buckets(b) {}
  ~ObjectCache() { reduce(0);}
  Local *lookup(Remote);
  void reduce(int);
private:
  int       _items;
  int       _buckets;
  cache_t   _cache;
  counter_t _counter;
};

/*
 * returns a cached form of r, creating it if necessary.
 * the local is moved to the end of the counter list so
 * we can eventually remove items from the head to reduce
 * the size of the cache elemminating the items not used
 * for the longest time
 */
template <class Remote, class Local>
inline Local *ObjectCache<Remote, Local>::lookup(Remote r)
{
  hash_t hash = r->_hash(_buckets);
  bucket_t &bucket = _cache[hash];
  bucket_t::iterator i = find_if(bucket.begin(), bucket.end(), Predicate(r));
  if (i == bucket.end())
    {
      Local *local = new Local(r);
      bucket.push_back(local);
      return bucket.back();
    }
  else
    {
      return *i;
    }
}

/*
 * reduce the cache to the new size
 * removing the items which have not been
 * used for the longest period first
 */
template <class Remote, class Local>
inline void ObjectCache<Remote, Local>::reduce(int size)
{
  //...to be implemented...
}

#endif /* _ObjectCache_hh */
