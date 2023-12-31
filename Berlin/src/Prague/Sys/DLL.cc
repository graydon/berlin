/*$Id: DLL.cc,v 1.8 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/config.hh"
#include "Prague/Sys/DLL.hh"
#if defined(HAVE_DLFCN)
#include <dlfcn.h>
#elif defined(HAVE_DLAIX)
#include <dl.h>
#endif

using namespace Prague;

void DLL::open(const std::string &name, bool now)
{
  lib = name;
  if (lib != "")
    {
#if defined(HAVE_DLFCN)
      int flags = now ? RTLD_NOW : RTLD_LAZY;
      flags |= RTLD_GLOBAL;
      handle = dlopen(lib.c_str(), flags);
      if (!handle) err = dlerror();
#elif defined(HAVE_DLAIX)
      shl_t shl_handle = shl_load (lib.c_str(), (now ? BIND_DEFERRED : BIND_IMMEDIATE) | BIND_NONFATAL | BIND_VERBOSE, 0);
      if (!shl_handle) err = strerror(errno);
      else handle = shl_handle;
#endif
    }
  else
    {
      handle = 0;
      err = "Empty Filename given.";
    }
}

void DLL::close()
{
#if defined(HAVE_DLFCN)
  if (handle) dlclose(handle);
#elif defined(HAVE_AIX)
  if (handle) shl_unload (reinterpret_cast<shl_t>(handle));
#endif
  handle = 0;
}

void *DLL::resolve(const std::string &symbol)
{
  if (!handle) return 0;
#if defined(HAVE_DLFCN)
  void *tmp = dlsym(handle, symbol.c_str());
  if (!tmp) err = dlerror();
#elif defined(HAVE_DLAIX)
  void *tmp;
  if (shl_findsym (reinterpret_cast<shl_t *>(&handle), symbol.c_str(), TYPE_UNDEFINED, &tmp) != 0 || handle == 0 || tmp == 0)
    err = strerror(errno);
#endif
  return tmp;
};
