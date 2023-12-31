/*$Id: Warsaw.hh,v 1.4 2000/07/21 19:25:04 stefan Exp $
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
#ifndef _Warsaw_hh
#define _Warsaw_hh

#include <Warsaw/config.hh>
#include <Warsaw/resolve.hh>
#include <Warsaw/ClientContextImpl.hh>
#include <iostream>

class Warsaw
{
public:
  Warsaw(int, char **);
  ~Warsaw();
  template <class T>
  typename T::_ptr_type resolve(const char *name) { return resolve_kit<T>(server, name);}
  template <class T>
  typename T::_ptr_type resolve(const char *name, const Kit::PropertySeq &p) { return resolve_kit<T>(server, name, p);}
private:
  CORBA::ORB_var orb;
  ClientContextImpl *client;
  ServerContext_var  server;
};

#endif /* _Warsaw_hh */
