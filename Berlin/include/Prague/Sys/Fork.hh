/*$Id: Fork.hh,v 1.2 1999/07/23 21:06:11 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this file is based on code from the socket++ library
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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

#ifndef _Fork_hh
#define _Fork_hh

#include <Prague/Sys/Signal.hh>
#include <unistd.h>
#include <sys/types.h>

namespace Prague
{

class Fork
{
  struct Process;
 public:
  Fork (bool = false, bool = false);
  ~Fork();
  bool   child() const;
  bool   parent() const;
  pid_t  pid() const;
  static void suicideOnSignal(int signo = Signal::terminate);
 private:
  Process *process;
  Fork (const Fork &);
  Fork &operator = (const Fork &);
};

};

#endif /* _Fork_hh */
