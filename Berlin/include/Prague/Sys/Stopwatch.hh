/*$Id: Stopwatch.hh,v 1.4 2001/01/15 02:49:18 stefan Exp $
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
#ifndef _Prague_Stopwatch_hh
#define _Prague_Stopwatch_hh

#include <sys/types.h>
#include <ctime>

namespace Prague
{

//. measure the real/cpu/sys time spend between two given moments.
class Stopwatch
{
  enum state { undef, stopped, running};
  struct interval
  {
    clock_t begin;
    clock_t end;
  };
public:
  Stopwatch();
  ~Stopwatch(){}
  //. start the stopwatch
  void start();
  //. stop the stopwatch
  void stop();
  //. return real elapsed time
  double real_time();
  //. return cpu elapsed time
  double cpu_time();
  //. return sys elapsed time
  double sys_time();
private:
  interval       _real;
  interval       _cpu;
  interval       _sys;
  static clock_t _ticks;
  state          _state;
};

};

#endif
