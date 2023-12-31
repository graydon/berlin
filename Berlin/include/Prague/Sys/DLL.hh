/*$Id: DLL.hh,v 1.7 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _DLL_hh
#define _DLL_hh

#include <string>

namespace Prague
{

//. DLL represents a dynamic library
class DLL
{
public:
  //. create a nil library handle
  DLL() : handle(0) {}
  //. create a library handle for the named library
  DLL(const std::string &name, bool now = true) { open(name, now);}
  ~DLL() { close();}
  //. open the given library
  void open(const std::string &, bool = true);
  //. close the library
  void close();
  //. resolve the given symbol
  void *resolve(const std::string &);
  //. return the library's name
  const std::string &name() const { return lib;}
  //. return the last error (should we replace that with an exception ?)
  const std::string &error() const { return err;}
  //. return true if the handle is valid
  operator bool () const { return handle;}
private:
  std::string lib;
  std::string err;
  void *handle;
};

}

#endif
