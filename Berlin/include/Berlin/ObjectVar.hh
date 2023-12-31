/*$Id: ObjectVar.hh,v 1.2 2000/07/24 23:32:11 stefan Exp $
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
#ifndef _Object_var_hh
#define _Object_var_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>

template <typename T>
class Object_var
{
  typedef typename T::_ptr_type T_ptr;
public:
  explicit Object_var(T_ptr tt = 0) : t(tt) {}
  Object_var(const Object_var &o) : t(T::_duplicate(o.t)) { if (!CORBA::_is_nil(t)) t->increment();}
  Object_var &operator = (Object_var &o)
    {
      if (&o != this)
        {
          if (!CORBA::_is_nil(t)) t->decrement(), CORBA::release(t);
          t = T::_duplicate(o.t);
	  if (!CORBA::_is_nil(t)) t->increase();
        }
      return *this;
    }
  ~Object_var() { if (!CORBA::_is_nil(t)) t->decrement(), CORBA::release(t);}
  Object_var &operator = (T_ptr tt)
    {
      if (!CORBA::_is_nil(t)) t->decrement(), CORBA::release(t);
      t = tt;
      return *this;
    }
  T_ptr get() const { return t;}
  T &operator *() const { return *t;}
  T *operator->() const { return  t;}
  operator T_ptr () const { return  t;}
  T_ptr _retn() { T_ptr tmp = t; t = 0; return tmp;}
private:
  T_ptr t;
};

#endif
