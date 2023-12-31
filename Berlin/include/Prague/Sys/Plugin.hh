/*$Id: Plugin.hh,v 1.7 1999/11/01 21:31:32 gray Exp $
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
#ifndef _Plugin_hh
#define _Plugin_hh

#include "DLL.hh"

namespace Prague {

template <class T>
class Plugin : public DLL
//. a special kind of a smart pointer
//. which implements a plugin behavior
{
public:
  Plugin(const string &file, const string &loader = "load") : DLL(file)
    {
      typedef T *(* DL) ();
      DL dl = (DL) resolve(loader);
      t = dl ? (T *) dl() : 0;
    }
  ~Plugin() { delete t;}
  T &operator *() const { return *t;}
  T *operator->() const { return  t;}
  operator T *() { return t; }
protected:
private:
  T *t;
};

}; // namespace

#define dload(T) extern "C" T *load() { return new T;}

#endif /* _Plugin_hh */
