/*$Id: IdentifiableImpl.cc,v 1.2 2000/09/22 20:58:59 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/Tracer.hh>
#include "Berlin/IdentifiableImpl.hh"
#include <cassert>

using namespace Prague;
using namespace Warsaw;

CORBA::Boolean IdentifiableImpl::is_identical(Identifiable_ptr id)
{
  Trace trace("IdentifiableImpl::is_identical");
  PortableServer::POA_var _poa = poa;
  if (CORBA::is_nil(poa)) _poa = _default_POA();
  try
    {
      PortableServer::Servant s = _poa->reference_to_servant(id);
      if (s == this) return 1;
    }
  catch (const PortableServer::POA::ObjectNotActive &) {}
  catch (const PortableServer::POA::WrongAdapter &) {}
  catch (const CORBA::OBJECT_NOT_EXIST &) {}
  return 0;
}
