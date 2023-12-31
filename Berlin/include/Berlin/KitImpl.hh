/*$Id: KitImpl.hh,v 1.4 2000/08/31 18:52:31 stefan Exp $
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
#ifndef _KitImpl_hh
#define _KitImpl_hh

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Kit.hh>
#include <Berlin/KitFactory.hh>

class ServerContextImpl;
class ServantBase;
class KitFactory;

class KitImpl : public virtual POA_Warsaw::Kit,
		public virtual PortableServer::RefCountServantBase
{
  friend class ServerContextImpl;
  friend class ::ServantBase;
  friend class KitFactory;
public:
  KitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  ~KitImpl();
  virtual Warsaw::Kit::PropertySeq *properties();
  virtual void bind(Warsaw::ServerContext_ptr) {};
  virtual CORBA::Boolean supports(const Warsaw::Kit::PropertySeq &);

  void activate(::ServantBase *);
  void deactivate(::ServantBase *);

  virtual void increment() { refcount++;}
  virtual void decrement() { if (!--refcount) deactivate();}
private:
  void activate(PortableServer::POA_ptr);
  void deactivate();
  KitFactory                     *factory;
  PortableServer::POA_var         poa;
  const Warsaw::Kit::PropertySeq &props;
  int                             refcount;
};

#endif
