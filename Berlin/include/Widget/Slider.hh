/*$Id: Slider.hh,v 1.2 1999/10/21 20:23:51 gray Exp $
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
#ifndef _Slider_hh
#define _Slider_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Warsaw/BoundedValue.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/ImplVar.hh>
#include <Widget/Stepper.hh>

class BVController : public ControllerImpl, implements(View)
{
protected:
  class Modifier : implements(Command) {};
public:
  BVController();
  void init(Controller_ptr);
  virtual void update(Subject_ptr);
  virtual void draw(DrawTraversal_ptr);
  virtual void pick(PickTraversal_ptr);
  virtual void allocate(Tag, const Allocation::Info &);
//protected:
  virtual void allocateThumb(const Allocation::Info &) = 0;
  virtual Stepper *stepper(PickTraversal_ptr, const Event::Pointer *) = 0;
  virtual void press(PickTraversal_ptr, const Event::Pointer *);
  virtual void release(PickTraversal_ptr, const Event::Pointer *);
  Controller_var    thumb;
private:
  Stepper          *current;
};

class Slider : public BVController
{
public:
  Slider(Axis);
  void init(Controller_ptr, BoundedValue_ptr);
//protected:
  void allocateThumb(const Allocation::Info &);
  Stepper *stepper(PickTraversal_ptr, const Event::Pointer *);
private:
  BoundedValue_var  value;
  Impl_var<Modifier> backward;
  Impl_var<Modifier> forward;
  Impl_var<Stepper> backwardStepper;
  Impl_var<Stepper> forwardStepper;
  Coord             offset;
  Axis              axis;
};

class XYSlider : public BVController
{
  enum { left, right, top, bottom, lefttop, righttop, leftbottom, rightbottom};
public:
  XYSlider();
  void init(Controller_ptr, BoundedValue_ptr, BoundedValue_ptr);
//protected:
  void allocateThumb(const Allocation::Info &);
  Stepper *stepper(PickTraversal_ptr, const Event::Pointer *);
private:
  BoundedValue_var  xvalue;
  BoundedValue_var  yvalue;
  Impl_var<Modifier> modifiers[8];
  Impl_var<Stepper> steppers[8];
  Coord             xoffset;
  Coord             yoffset;
  Vertex            thumbSize;
};

#endif /* _Slider_hh */
