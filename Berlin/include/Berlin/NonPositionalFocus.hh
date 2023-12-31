/*$Id: NonPositionalFocus.hh,v 1.13 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _NonPositionalFocus_hh
#define _NonPositionalFocus_hh

#include <Warsaw/config.hh>
#include <Warsaw/Controller.hh>
#include <Warsaw/Region.hh>
#include <Berlin/ImplVar.hh>
#include <Prague/Sys/Thread.hh>
#include <Berlin/FocusImpl.hh>
#include <vector>

class ScreenImpl;

class NonPositionalFocus : public FocusImpl
{
  typedef std::vector<Warsaw::Controller_var> cstack_t;
 public:
  NonPositionalFocus(Warsaw::Input::Device, Warsaw::Controller_ptr);
  virtual ~NonPositionalFocus();

  virtual void grab() {}
  virtual void ungrab() {}
  virtual void add_filter(Warsaw::Input::Filter_ptr);

  virtual bool request(Warsaw::Controller_ptr);
  virtual void restore(Warsaw::Region_ptr) {}
  virtual void damage(Warsaw::Region_ptr) {}
  virtual void dispatch(Warsaw::Input::Event &);
protected:
  virtual void activate_composite();
private:
  cstack_t      _controllers;
  Prague::Mutex _mutex;
};

#endif 
