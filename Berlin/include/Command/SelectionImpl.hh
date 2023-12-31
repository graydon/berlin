/*$Id: SelectionImpl.hh,v 1.3 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _SelectionImpl_hh
#define _SelectionImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Telltale.hh>
#include <Warsaw/Selection.hh>
#include <Berlin/SubjectImpl.hh>
#include <Berlin/RefCountVar.hh>
#include <vector>
#include <algorithm>
#include <functional>

class SelectionImpl : public virtual POA_Warsaw::Selection,
	              public SubjectImpl
{
  class Observer;
  friend class Observer;
  typedef std::vector<Observer *> list_t;
  struct Id_eq : public std::unary_function<Observer *, bool>
  {
    Id_eq(Warsaw::Tag t) : id(t) {}
    bool operator()(const Observer *) const;
    Warsaw::Tag id;
  };
 public:
  SelectionImpl(Warsaw::Selection::Policy, Warsaw::TelltaleConstraint_ptr);
  virtual ~SelectionImpl();
  virtual Warsaw::Selection::Policy type();
  virtual void type(Warsaw::Selection::Policy);
  virtual Warsaw::Tag add(Warsaw::Telltale_ptr);
  virtual void remove(Warsaw::Tag);
  virtual Warsaw::Selection::Items *toggled();
 private:
  void update(Warsaw::Tag, bool);
  void remove_observer(Warsaw::Tag);
  Warsaw::Tag uniqueId();
  CORBA::Long id_to_index(Warsaw::Tag);
  Prague::Mutex mutex;
  Warsaw::Selection::Policy policy;
  RefCount_var<Warsaw::TelltaleConstraint> constraint;
  list_t items;
};

#endif
