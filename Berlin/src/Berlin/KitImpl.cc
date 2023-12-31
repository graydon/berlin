/*$Id: KitImpl.cc,v 1.4 2000/10/06 21:36:48 stefan Exp $
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
#include "Berlin/KitImpl.hh"
#include "Berlin/KitFactory.hh"
#include "Berlin/ServantBase.hh"

using namespace Prague;
using namespace Warsaw;

KitImpl::KitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : factory(f), props(p), refcount(1)
{
  Trace trace("KitImpl::KitImpl");
  factory->increment();
}

KitImpl::~KitImpl()
{
  Trace trace("KitImpl::~KitImpl");  
  poa->destroy(true, true);
  factory->decrement();
}

Warsaw::Kit::PropertySeq *KitImpl::properties()
{
  return new Warsaw::Kit::PropertySeq(props);
}

CORBA::Boolean KitImpl::supports(const Warsaw::Kit::PropertySeq &p)
{
  Trace trace("KitImpl::supports");
  return KitFactory::supports(props, p);
}

void KitImpl::activate(::ServantBase *servant)
{
  Trace trace("KitImpl::activate(PortableServer::Servant)");
  PortableServer::ObjectId *oid = poa->activate_object(servant);
  servant->poa = PortableServer::POA::_duplicate(poa);
  servant->_remove_ref();
  delete oid;
  servant->activate_composite();
}

void KitImpl::deactivate(::ServantBase *servant)
{
  Trace trace("KitImpl::deactivate(PortableServer::Servant)");
  PortableServer::ObjectId *oid = poa->servant_to_id(servant);
  poa->deactivate_object(*oid);
  delete oid;
}

void KitImpl::deactivate()
{
  Trace trace("KitImpl::deactivate()");
  PortableServer::ObjectId *oid = poa->servant_to_id(this);
  poa->deactivate_object(*oid);
  delete oid;
}


