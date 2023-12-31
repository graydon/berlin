/*$Id: PolyGraphic.hh,v 1.20 2000/10/15 16:23:04 velco Exp $
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
#ifndef _PolyGraphic_hh
#define _PolyGraphic_hh

#include <Berlin/GraphicImpl.hh>
#include <Berlin/Pool.hh>
#include <vector>

class PolyGraphic : public GraphicImpl
{
  class Iterator;
  friend class Iterator;
public:
  PolyGraphic();
  virtual ~PolyGraphic();

  virtual void append_graphic(Warsaw::Graphic_ptr);
  virtual void prepend_graphic(Warsaw::Graphic_ptr);
  virtual void remove_graphic(Warsaw::Tag);
  virtual void remove_child_graphic(Warsaw::Tag);
  virtual Warsaw::Graphic::Iterator_ptr first_child_graphic();
  virtual Warsaw::Graphic::Iterator_ptr last_child_graphic();

  virtual void need_resize();
  virtual void need_resize(Warsaw::Tag);
protected:
  CORBA::Long num_children();
  Warsaw::Tag unique_child_id();
  glist_t::iterator child_id_to_iterator(Warsaw::Tag);
  CORBA::Long child_id_to_index(Warsaw::Tag);
  Warsaw::Graphic::Requisition *children_requests();
  void deallocate_requisitions(Warsaw::Graphic::Requisition *);
  void child_extension(size_t, const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
// private:
  static Pool<Warsaw::Graphic::Requisition> _pool;
  glist_t _children;
  Prague::Mutex _mutex;
};

/*
 * the following methods are inlined for speed.
 * Attention : they must be used within a PolyGraphic::childMutex locked section !
 */
inline Warsaw::Tag PolyGraphic::unique_child_id()
{
  Warsaw::Tag localId;
  for (localId = 0;
       find_if (_children.begin(), _children.end(), localId_eq(localId)) != _children.end();
       localId++);
      return localId;
}

inline PolyGraphic::glist_t::iterator PolyGraphic::child_id_to_iterator(Warsaw::Tag localId)
{
  return find_if(_children.begin(), _children.end(), localId_eq(localId));
}

inline CORBA::Long PolyGraphic::child_id_to_index(Warsaw::Tag localId)
{
  return find_if(_children.begin(), _children.end(), localId_eq(localId)) - _children.begin();
}

#endif 
