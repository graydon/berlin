/*$Id: LogoDemo.hh,v 1.1 1999/11/07 19:46:25 stefan Exp $
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
#ifndef _LogoDemo_hh
#define _LogoDemo_hh

#include "Warsaw/config.hh"
#include "Warsaw/Command.hh"
#include "Warsaw/BoundedValue.hh"
#include "Warsaw/Transformator.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Demo.hh"

class Forward : implements(Command)
{
 public:
  Forward(BoundedValue_ptr v) : value(BoundedValue::_duplicate(v)) {}
  void execute(const CORBA::Any &) { value->forward();}
 private:
  BoundedValue_var value;
};

class Backward : implements(Command)
{
 public:
  Backward(BoundedValue_ptr v) : value(BoundedValue::_duplicate(v)) {}
  void execute(const CORBA::Any &) { value->backward();}
 private:
  BoundedValue_var value;
};

class Rotator : implements(Observer)
{
 public:
  Rotator(BoundedValue_ptr, Transformator_ptr, Graphic_ptr, Coord);
  void update(Subject_ptr, const CORBA::Any &);
 private:
  BoundedValue_var value;
  Transformator_var transformator;
  Graphic_var parent;
  Coord zdegree;
};

class LogoDemo : public Demo
{
public:
  LogoDemo(Application *);
  Graphic_ptr makeController(BoundedValue_ptr, const Color &);
private:
  BoundedValue_var bv1;
  BoundedValue_var bv2;
  BoundedValue_var bv3;
  Impl_var<TransformImpl> tx1;
  Impl_var<TransformImpl> tx2;
  Impl_var<TransformImpl> tx3;
  Impl_var<Rotator> rotator1;
  Impl_var<Rotator> rotator2;
  Impl_var<Rotator> rotator3;
};

#endif
