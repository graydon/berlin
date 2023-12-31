/*$Id: Subject.hh,v 1.2 1999/04/27 20:11:11 gray Exp $
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
#ifndef _Subject_hh
#define _Subject_hh

#include <Prague/Sys/Action.hh>
#include <vector>

namespace Prague
{

/* @Class{Subject}
 *
 * @Description{}
 */
template <class T>
class Subject
{
public:
  Subject() {}
  virtual ~Subject() {}
  virtual void notify(const T &t)
    {
      for (rep_type::iterator i = list.begin(); i != list.end(); i++)
	(*i)->execute(t);
    }
  void bind(Action<T> *action) { list.push_back(action);}
  void release(Action<T> *action)
    {
      for (rep_type::iterator i = list.begin(); i != list.end(); i++)
	if ((*i) == action)
	  {
	    list.erase(i);
	    break;
	  }
    }
protected:
  typedef vector<Action<T> *> rep_type;
  rep_type list;
};

};

#endif /* _Subject_hh */
