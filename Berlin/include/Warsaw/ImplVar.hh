/*$Id: ImplVar.hh,v 1.2 2000/04/04 19:14:33 stefan Exp $
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

template <class T>
class Impl_var
//. a special kind of a smart pointer
//. which hides the BOA/POA details
{
public:
  explicit Impl_var(T *tt = 0) : t(tt) { if (t) t->_obj_is_ready(CORBA::BOA::getBOA());}
  Impl_var(Impl_var &i) : t(i.release()) {}
  Impl_var &operator = (Impl_var &i) { if (&i != this) { if (t) t->_dispose(); t = i.release();} return *this;}
  ~Impl_var() { if (t) t->_dispose();}
  Impl_var &operator = (T *tt) { if (t) t->_dispose(); t = tt; t->_obj_is_ready(CORBA::BOA::getBOA()); return *this;}
  T *get() const { return t;}
  T &operator *() const { return *t;}
  T *operator->() const { return  t;}
  operator T *() const { return  t;}
  T *release() { T *tmp = t; t = 0; return tmp;}
  void reset(T *tt = 0) { if (t) t->_dispose(); t = tt; t->_obj_is_ready(CORBA::BOA::getBOA());}
private:
  T *t;
};

#endif
