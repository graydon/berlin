/*$Id: Stepper.cc,v 1.6 2000/09/22 20:58:59 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Tool/Stepper.hh"
#include <Berlin/Vertex.hh>
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

class Stepper::Notifier : public Timer::Notifier
{
public:
  Notifier(Stepper *s) : _stepper(s) {}
  virtual void notify() { _stepper->step();}
private:
  Stepper *_stepper;
};

Stepper::Stepper()
  : _delay(500), _delta(300), _notifier(new Notifier(this)), _timer(_notifier)
{
}

Stepper::~Stepper()
{
  Trace trace("Stepper::~Stepper");
  stop();
  delete _notifier;
}

void Stepper::press(PickTraversal_ptr traversal, const Input::Event &event)
{
  Trace trace("Stepper::press");
  ControllerImpl::press(traversal, event);
  start();
}

void Stepper::release(PickTraversal_ptr traversal, const Input::Event &event)
{
  Trace trace("Stepper::release");
  stop();
  ControllerImpl::release(traversal, event);
}

void Stepper::step()
{
  Trace trace("Stepper::step");
  CORBA::Any any;
  execute(any);
}

void Stepper::start()
{
  _timer.start(Prague::Time::currentTime() + _delay, _delta);
}

void Stepper::stop()
{
  _timer.stop();
}
