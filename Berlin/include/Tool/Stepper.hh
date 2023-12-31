/*$Id: Stepper.hh,v 1.4 2000/09/22 20:58:59 stefan Exp $
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
#ifndef _Stepper_hh
#define _Stepper_hh

#include <Prague/Sys/Time.hh>
#include <Prague/Sys/Timer.hh>
#include <Tool/TriggerImpl.hh>

class Stepper : public TriggerImpl
//. The Stepper class implements a button with autorepeat.
{
  class Notifier;
  friend class Notifier;
public:
  Stepper();
  ~Stepper();
//protected:
  virtual void press(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void release(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual void step();
private:
  void start();
  void stop();
  Prague::Time  _delay;
  Prague::Time  _delta;
  Notifier     *_notifier;
  Prague::Timer _timer;
};

#endif
