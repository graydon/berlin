/*$Id: Stepper.cc,v 1.5 1999/11/06 20:23:08 stefan Exp $
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
#include "Berlin/Vertex.hh"
#include "Berlin/Logger.hh"
#include "Widget/Stepper.hh"

using namespace Prague;

class Stepper::Notifier : public Timer::Notifier
{
public:
  Notifier(Stepper *s) : stepper(s) {}
  virtual void notify() { stepper->step();}
private:
  Stepper *stepper;
};

Stepper::Stepper()
  : delay(500), delta(300), notifier(new Notifier(this)), timer(notifier)
{
}

Stepper::~Stepper()
{
  stop();
  delete notifier;
}

void Stepper::press(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  SectionLog section("Stepper::press");
  ControllerImpl::press(traversal, pointer);
  start();
}

void Stepper::release(PickTraversal_ptr traversal, const Event::Pointer *pointer)
{
  SectionLog section("Stepper::release");
  stop();
  ControllerImpl::release(traversal, pointer);
}

void Stepper::step()
{
  SectionLog section("Stepper::step");
  CORBA::Any any;
  execute(any);
}

void Stepper::start()
{
  timer.start(Prague::Time::currentTime() + delay, delta);
}

void Stepper::stop()
{
  timer.stop();
}
