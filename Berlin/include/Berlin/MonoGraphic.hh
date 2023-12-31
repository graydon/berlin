/*$Id: MonoGraphic.hh,v 1.14 2000/09/19 21:11:03 stefan Exp $
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
#ifndef _MonoGraphic_hh
#define _MonoGraphic_hh

#include <Berlin/GraphicImpl.hh>

class MonoGraphic : public GraphicImpl
{
public:
  MonoGraphic();
  virtual ~MonoGraphic();

  virtual Warsaw::Graphic_ptr body();
  virtual void body(Warsaw::Graphic_ptr);
  virtual void append_graphic(Warsaw::Graphic_ptr);
  virtual void prepend_graphic(Warsaw::Graphic_ptr);
  virtual void remove_graphic(Warsaw::Tag);
  virtual void remove_child_graphic(Warsaw::Tag);
  virtual Warsaw::Graphic::Iterator_ptr first_child_graphic();
  virtual Warsaw::Graphic::Iterator_ptr last_child_graphic();

  virtual Warsaw::Transform_ptr transformation();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void shape(Warsaw::Region_ptr);

  virtual void traverse(Warsaw::Traversal_ptr);
protected:
  Edge          _child;
  Prague::Mutex _mutex;
};

#endif 
