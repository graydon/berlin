/*$Id: TelltaleImpl.hh,v 1.9 1999/10/19 21:07:52 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _TelltaleImpl_hh
#define _TelltaleImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Telltale.hh"
#include "Berlin/SubjectImpl.hh"
#include "Prague/Sys/Thread.hh"
#include <vector>

class TelltaleImpl : implements(Telltale), public SubjectImpl
{
 public:
  TelltaleImpl(TelltaleConstraint_ptr, unsigned long f = 0);
  virtual ~TelltaleImpl();
  virtual void set(Telltale::Flag);
  virtual void clear(Telltale::Flag);
  virtual CORBA::Boolean test(Telltale::Flag);
  virtual void modify(Telltale::Flag, CORBA::Boolean);
  unsigned long state() { return flags;}
  virtual void constraint(TelltaleConstraint_ptr c);
  virtual TelltaleConstraint_ptr constraint();

 protected:
  unsigned long flags;
  TelltaleConstraint_var myConstraint;
  Prague::Mutex mutex;
};

class TelltaleConstraintImpl : implements(TelltaleConstraint)
{
  typedef vector<Telltale_var> tlist_t;
 public:
  TelltaleConstraintImpl() {}
  virtual ~TelltaleConstraintImpl() {}
  void add(Telltale_ptr);
  void remove(Telltale_ptr);
  virtual void trymodify(Telltale_ptr, Telltale::Flag, CORBA::Boolean) = 0;
 protected:
  tlist_t telltales;
  Prague::Mutex mutex;
};

class ExclusiveChoice : virtual public TelltaleConstraintImpl
{
public:
  ExclusiveChoice(Telltale::Flag = Telltale::chosen);
  virtual void trymodify(Telltale_ptr, Telltale::Flag, CORBA::Boolean);  
private:
  Telltale::Flag flag;
};

class SelectionRequired : virtual public TelltaleConstraintImpl
{
public:
  SelectionRequired();
  virtual void trymodify(Telltale_ptr, Telltale::Flag, CORBA::Boolean);  
};

#endif /* _TelltaleImpl_hh */
