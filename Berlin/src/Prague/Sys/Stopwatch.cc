/*$Id: Stopwatch.cc,v 1.2 1999/04/27 20:09:50 gray Exp $
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
#include "Prague/Sys/Stopwatch.hh"
#include <iostream>

#include <sys/times.h>
#include <stdio.h>
#include <unistd.h>

using namespace Prague;

clock_t Stopwatch::ticks = 0;

/* @Method{Stopwatch::Stopwatch()}
 *
 * @Description{}
 */
Stopwatch::Stopwatch()
  : s(undef)
{
  if (!ticks) ticks = CLK_TCK;
  start();
};

/* @Method{void Stopwatch::start()}
 *
 * @Description{}
 */
void Stopwatch::start()
{
  s = running;
  struct tms cpt;
  realbegin = times(&cpt);
  cpubegin  = cpt.tms_utime;
  sysbegin  = cpt.tms_stime;
  if (realbegin == -1) perror("Stopwatch::start");
};

/* @Method{void Stopwatch::stop()}
 *
 * @Description{}
 */
void Stopwatch::stop()
{
  s = stopped;
  struct tms cpt;
  realend = times(&cpt);
  cpuend  = cpt.tms_utime;
  sysend  = cpt.tms_stime;
  if (realend == -1) perror("Stopwatch::stop");
};

/* @Method{double Stopwatch::realStopwatch()}
 *
 * @Description{}
 */
double Stopwatch::realTime()
{
  if (s == undef) cerr << "Stopwatch::realTime: no starting point set" << endl;
  else if (s == running) stop();
  return (double) (realend - realbegin)/ticks;
};

/* @Method{double Stopwatch::cpuTime()}
 *
 * @Description{}
 */
double Stopwatch::cpuTime()
{
  if (s == undef) cerr << "Stopwatch::cpuTime: no starting point set" << endl;
  else if (s == running) stop();
  return (double) (cpuend - cpubegin)/ticks;
};

/* @Method{double Stopwatch::sysTime()}
 *
 * @Description{}
 */
double Stopwatch::sysTime()
{
  if (s == undef) cerr << "Stopwatch::sysTime: no starting point set" << endl;
  else if (s == running) stop();
  return (double) (sysend - sysbegin)/ticks;
};
