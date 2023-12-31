/*$Id: DLL.hh,v 1.3 1999/05/19 17:01:24 gray Exp $
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
#ifndef _DLL_hh
#define _DLL_hh

#include <string>

namespace Prague
{

/* @Class{DLL}
 *
 * @Description{DLL represents a dynamic library}
 */
class DLL
{
public:
  DLL() : handle(0) {}
  DLL(const string &name, bool now = true) { open(name, now);}
  ~DLL() { close();}
  void open(const string &, bool = true);
  void close();
  void *resolve(const string &);
  const string &name() const { return lib;}
  const string &error() const { return err;}
  operator bool () const { return handle;}
protected:
private:
  string lib;
  string err;
  void *handle;
};

};

#endif /* _DLL_hh */
