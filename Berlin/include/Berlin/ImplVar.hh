/*$Id: ImplVar.hh,v 1.8 2000/10/20 17:45:01 stefan Exp $
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
#ifndef _Impl_var_hh
#define _Impl_var_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Prague/Sys/Tracer.hh>

//.these smart pointers take care of the activation/deactivation of
//.servants they get assigned to
//.as such, it is *not* meant to be used for RefCountBase objects as
//.they deactivate themselfs when the ref counter drops to zero
//.
//.The Impl_var class is similar to auto_ptr, i.e. it owns the assigned
//.object. If you assign one Impl_var to another, the first will lose
//.ownership.
template <class T>
class Impl_var
{
public:
  explicit Impl_var(T *tt = 0) : t(tt) { if (t) activate(t);}
  Impl_var(Impl_var<T> &i) : t(i._retn()) {}
  ~Impl_var() { if (t) deactivate(t);}
  Impl_var<T> &operator = (Impl_var<T> &i) { if (&i != this) { if (t) deactivate(t); t = i._retn();} return *this;}
  Impl_var<T> &operator = (T *tt) { if (t) deactivate(t); t = tt; if (t) activate(t); return *this;}
  T &operator *() const { return *t;}
  T *operator->() const { return  t;}
  operator T *() const { return  t;}
  T *_retn() { T *tmp = t; t = 0; return tmp;}
  static void activate(T *);
  static void deactivate(T *);
private:
  T *t;
};

template <class T>
inline void Impl_var<T>::activate(T *t)
{
  Prague::Trace trace("Impl_var::activate");
  PortableServer::POA_var poa = t->_default_POA();
  PortableServer::ObjectId *oid = poa->activate_object(t);
  t->_remove_ref();
  delete oid;
}

template <class T>
inline void Impl_var<T>::deactivate(T *t)
{
  Prague::Trace trace("Impl_var::deactivate");
  PortableServer::POA_var poa = t->_default_POA();
  PortableServer::ObjectId *oid = poa->servant_to_id(t);
  poa->deactivate_object(*oid);
  delete oid;
}

#endif
