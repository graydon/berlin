/*$Id: Observer.hh,v 1.2 1999/04/27 20:11:11 gray Exp $
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
#ifndef _Observer_hh
#define _Observer_hh

#include <Prague/Sys/Action.hh>

namespace Prague
{

/*
 * @Class{Observer<Argument> : public Action<Argument>}
 *
 * @Description{represents a parametrized pair of a class @var{T} and a method of @var{T} with the same signature as @var{Action<Argument>. It is called by @code{execute(const Argument &)}.}
 */
template <class Class, class T>
class Observer : public Action<T>
{
public:
  typedef void (Class::*method)(const T &);
  Observer(Class *cc, method mm) : c(cc), m(mm) {}
  Observer(const Observer &OO) : c(OO.c), m(OO.m) {}
  virtual ~Observer() {}
  virtual void execute(const T &t) { (c->*m)(t);}
protected:
  Class *c;
  method m;
private:
};

/* @Method{Observer<Argument>::Observer(Class *c, Observer::method m)}
 *
 * @Description{constructor}
 */

/* @Method{void Observer<Argument>::execute(const T &t)}
 *
 * @Description{calls the method it is wrapping}
 */

};

#endif /* _Observer_hh */
