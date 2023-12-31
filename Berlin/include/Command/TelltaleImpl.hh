/*$Id: TelltaleImpl.hh,v 1.6 2001/04/18 06:07:25 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Warsaw/config.hh>
#include <Warsaw/Telltale.hh>
#include <Berlin/SubjectImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <vector>

class TelltaleImpl : public virtual POA_Warsaw::Telltale,
		     public SubjectImpl
{
 public:
  TelltaleImpl(Warsaw::TelltaleConstraint_ptr, CORBA::ULong m = 0);
  virtual ~TelltaleImpl();
  virtual void set(Warsaw::Telltale::Mask);
  virtual void clear(Warsaw::Telltale::Mask);
  virtual CORBA::Boolean test(Warsaw::Telltale::Mask);
  virtual void modify(Warsaw::Telltale::Mask, CORBA::Boolean);
  CORBA::ULong state() { return _mask;}

  virtual void constraint(Warsaw::TelltaleConstraint_ptr);
  virtual Warsaw::TelltaleConstraint_ptr constraint();

 protected:
  Prague::Mutex                  _mutex;
  CORBA::ULong                   _mask;
  Warsaw::TelltaleConstraint_var _constraint;
};

class TelltaleConstraintImpl : public virtual POA_Warsaw::TelltaleConstraint,
			       public virtual PortableServer::RefCountServantBase,
			       public virtual RefCountBaseImpl
{
  typedef std::vector<Warsaw::Telltale_var> tlist_t;
 public:
  TelltaleConstraintImpl() {}
  virtual ~TelltaleConstraintImpl() {}
  void add(Warsaw::Telltale_ptr);
  void remove(Warsaw::Telltale_ptr);
  virtual void trymodify(Warsaw::Telltale_ptr, Warsaw::Telltale::Mask, CORBA::Boolean) = 0;
 protected:
  Prague::Mutex _mutex;
  tlist_t       _telltales;
};

class ExclusiveChoice : public TelltaleConstraintImpl
{
public:
  ExclusiveChoice(Warsaw::Telltale::Mask);
  virtual void trymodify(Warsaw::Telltale_ptr, Warsaw::Telltale::Mask, CORBA::Boolean);  
private:
  Warsaw::Telltale::Mask _mask;
};

class SelectionRequired : public TelltaleConstraintImpl
{
public:
  SelectionRequired(Warsaw::Telltale::Mask);
  virtual void trymodify(Warsaw::Telltale_ptr, Warsaw::Telltale::Mask, CORBA::Boolean);  
private:
  Warsaw::Telltale::Mask _mask;
};

class ExclusiveRequired : public TelltaleConstraintImpl
{
public:
  ExclusiveRequired(Warsaw::Telltale::Mask);
  virtual void trymodify(Warsaw::Telltale_ptr, Warsaw::Telltale::Mask, CORBA::Boolean);  
private:
  Warsaw::Telltale::Mask _mask;
};

#endif
