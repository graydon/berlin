/*$Id: BoundedRangeImpl.hh,v 1.6 1999/09/30 17:23:33 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _BoundedRangeImpl_hh
#define _BoundedRangeImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/BoundedRange.hh"
#include "Berlin/SubjectImpl.hh"
#include "Prague/Sys/Thread.hh"
#include <vector>

class BoundedRangeImpl : implements(BoundedRange), virtual public SubjectImpl
{
 public:
  BoundedRangeImpl(Coord, Coord, Coord, Coord, Coord, Coord);
  virtual ~BoundedRangeImpl();
  virtual Coord lower();
  virtual void lower(Coord);
  virtual Coord upper();
  virtual void upper(Coord);
  virtual Coord step();
  virtual void step(Coord);
  virtual Coord page();
  virtual void page(Coord);
  virtual Coord lvalue();
  virtual void lvalue(Coord);
  virtual Coord uvalue();
  virtual void uvalue(Coord);

  virtual void forward();
  virtual void backward();
  virtual void fastforward();
  virtual void fastbackward();
  virtual void begin();
  virtual void end();
  virtual void adjust(Coord);
 protected:
  Coord l, u, lv, uv;
  Coord s, p;
  Prague::Mutex mutex;
};

#endif /* _BoundedRangeImpl_hh */
