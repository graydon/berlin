/*$Id: ClientContextImpl.cc,v 1.3 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
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
#include "Warsaw/ClientContextImpl.hh"
#include <iostream>
#include <string>

using namespace Warsaw;

ClientContextImpl::ClientContextImpl()
  : user(new Prague::User())
{};  
  
Unistring *ClientContextImpl::userName()
{
  std::string name = user->name();
  Unistring *ustring = new Unistring;
  ustring->length(name.length());
  for(unsigned int i = 0; i < name.length(); i++) ustring[i] = name[i];
  return ustring;
}
