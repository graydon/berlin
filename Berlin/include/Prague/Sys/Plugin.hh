/*$Id: Plugin.hh,v 1.13 2001/03/25 08:25:16 stefan Exp $
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
#ifndef _Prague_Plugin_hh
#define _Prague_Plugin_hh

#include <Prague/Sys/DLL.hh>

namespace Prague
{

//. a special kind of a smart pointer which implements a plugin behavior.
//. It assumes a special layout of the library, with a special factory
//. that manufactures objects of type T.
template <class T>
class Plugin : public DLL
{
public:
  //. create a Plugin from the fiven file, using a factory with name loader
  //. to create the actual object
  Plugin(const std::string &file, const std::string &loader = "load") : DLL(file)
    {
      typedef T *(* DL) ();
      DL dl = (DL) resolve(loader);
      t = dl ? (T *) dl() : 0;
    }
  ~Plugin() { delete t;}
  T &operator *() const { return *t;}
  T *operator->() const { return  t;}
  T *get() const { return t;}
private:
  T *t;
};

}

#define dload(T) extern "C" T *load() { return new T;}

#endif
