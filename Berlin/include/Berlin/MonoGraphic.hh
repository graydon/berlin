/*$Id: MonoGraphic.hh,v 1.10 1999/09/30 17:23:33 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
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

  virtual Graphic_ptr body();
  virtual void body(Graphic_ptr);
  virtual void append(Graphic_ptr);
  virtual void prepend(Graphic_ptr);

  virtual Transform_ptr transformation();
  virtual void request(Requisition &);
  virtual void extension(const Allocation::Info &, Region_ptr);
  virtual void shape(Region_ptr);

  virtual void traverse(Traversal_ptr);
protected:
  Graphic_var child;
  Prague::Mutex childMutex;
};

#endif /* _MonoGraphic_hh */
