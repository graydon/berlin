/*$Id: KitFactory.hh,v 1.3 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _KitFactory_hh
#define _KitFactory_hh

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Kit.hh>
#include <string>

class ServerContextImpl;
class KitImpl;

class KitFactory
{
  friend class KitImpl;
public:
  KitFactory(const std::string &, const std::string *, unsigned short);
  virtual ~KitFactory() { delete _props;}
  const std::string &type() const { return _id;}
  Warsaw::Kit::PropertySeq *properties() const { return new Warsaw::Kit::PropertySeq(*_props);}  
  virtual KitImpl *create(const Warsaw::Kit::PropertySeq &, PortableServer::POA_ptr) = 0;
  virtual bool supports(const Warsaw::Kit::PropertySeq &p) { return supports(*_props, p);}
  static bool supports(const Warsaw::Kit::PropertySeq &, const Warsaw::Kit::PropertySeq &);
protected:
  void increment() { _counter++;}
  void decrement() { _counter--;}
  static void activate(KitImpl *, PortableServer::POA_ptr);
  Warsaw::Kit::PropertySeq *_props;
private:
  const std::string _id;
  unsigned short    _counter;
};

template <class T>
class KitFactoryImpl : public KitFactory
{
 public:
  KitFactoryImpl(const std::string &type, const std::string *properties, unsigned short size)
    : KitFactory(type, properties, size) {}
  virtual KitImpl *create(const Warsaw::Kit::PropertySeq &, PortableServer::POA_ptr poa)
  {
    Prague::Trace trace("KitFactoryImpl::create");
    T *t = new T(this, *_props);
    KitFactory::activate(t, poa);
    return t;
  }
};

#endif
