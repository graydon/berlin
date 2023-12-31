/*$Id: Manipulator.hh,v 1.1 2000/09/05 21:12:00 stefan Exp $
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
#ifndef _Manipulator_hh
#define _Manipulator_hh

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Window.hh>
#include <Warsaw/Desktop.hh>
#include <Berlin/CommandImpl.hh>

class Manipulator : public CommandImpl
{
public:
  Manipulator(Warsaw::Window_ptr w) : window(Warsaw::Window::_duplicate(w)) {}
  virtual ~Manipulator() { Prague::Trace trace("Manipulator::~Manipulator");}
  virtual void execute(const CORBA::Any &) = 0;
protected:
  Warsaw::Window_var window;
};

class Mover : public Manipulator
{
public:
  Mover(Warsaw::Window_ptr window) : Manipulator(window) {}
  virtual void execute(const CORBA::Any &);
};

class Resizer : public Manipulator
{
public:
  Resizer(Warsaw::Window_ptr window) : Manipulator(window) {}
  virtual void execute(const CORBA::Any &);
};

class MoveResizer : public Manipulator
{
public:
  MoveResizer(Warsaw::Window_ptr, Warsaw::Desktop_ptr, Warsaw::Alignment, Warsaw::Alignment, CORBA::Short);
  virtual void execute(const CORBA::Any &);
private:
  Warsaw::Desktop_var desktop;
  Warsaw::Alignment xalign, yalign;
  CORBA::Short border;
};

class Relayerer: public Manipulator
{
public:
  Relayerer(Warsaw::Window_ptr window) : Manipulator(window) {}
  virtual void execute(const CORBA::Any &);
};

class Mapper : public Manipulator
{
public:
  Mapper(Warsaw::Window_ptr window) : Manipulator(window) {}
  virtual void execute(const CORBA::Any &);
};

class Unmapper : public Manipulator
{
public:
  Unmapper(Warsaw::Window_ptr window) : Manipulator(window) {}
  virtual void execute(const CORBA::Any &);
};

#endif
