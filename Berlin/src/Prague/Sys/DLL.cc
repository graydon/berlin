/*$Id: DLL.cc,v 1.3 1999/05/19 17:01:24 gray Exp $
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

#include "Prague/Sys/DLL.hh"
#if 1
#include <dlfcn.h>
#else /* _aix_ */
#include <dl.h>
#endif

using namespace Prague;

/* @Method{void DLL::open(const string &name, bool now = true)}
 *
 * @Description{open the library @var{name}}}
 */
void DLL::open(const string &name, bool now = true)
{
  lib = name;
#if 1
  handle = dlopen(lib.c_str(), now ? RTLD_NOW : RTLD_LAZY);
  if (!handle) err = dlerror();
#else /* _aix_ */
  shl_t shl_handle = shl_load (lib.c_str(), (now ? BIND_DEFERRED : BIND_IMMEDIATE) | BIND_NONFATAL | BIND_VERBOSE, 0);
  if (!shl_handle) err = strerror(errno);
  else handle = shl_handle;
#endif
}

/* @Method{void DLL::close()}
 *
 * @Description{close the library}
 */
void DLL::close()
{
#if 1
  if (handle) dlclose(handle);
#else /* _aix_ */
  if (handle) shl_unload (reinterpret_cast<shl_t>(handle));
#endif
  handle = 0;
}

/* @Method{void *DLL::resolve(const string &symbol)}
 *
 * @Description{return a reference to an object with name @var{symbol}}
 */
void *DLL::resolve(const string &symbol)
{
  if (!handle) return 0;
#if 1
  void *tmp = dlsym(handle, symbol.c_str());
  if (!tmp) err = dlerror();
#else /* _aix_ */
  void *tmp;
  if (shl_findsym (reinterpret_cast<shl_t *>(&handle), symbol.c_str(), TYPE_UNDEFINED, &tmp) != 0 || handle == 0 || tmp == 0)
    err = strerror(errno);
#endif
  return tmp;
};
