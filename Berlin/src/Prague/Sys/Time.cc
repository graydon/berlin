/*$Id: Time.cc,v 1.4 2000/09/23 21:18:36 stefan Exp $
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
#include "Prague/Sys/Time.hh"

using namespace Prague;

const Time Time::zero(0);

/* @Method{Time::Time(int hour, int minute, int second, int millisecond)}
 *
 * @Description{}
 */
Time::Time(int h, int m, int s, int ms)
{
  tm r;
  r.tm_hour = h;
  r.tm_min  = m;
  r.tm_sec  = s;
  tv_sec = mktime(&r), tv_usec = 1000*ms;
}

/* @Method{Time::Time(int year, int month, int day, int hour, int minute, int second, int millisecond)}
 *
 * @Description{}
 */
Time::Time(int y, int mo, int d, int h, int m, int s, int ms)
{
  tm r;
  r.tm_year = y - 1900;
  r.tm_mon  = mo;
  r.tm_mday = d;
  r.tm_hour = h;
  r.tm_min  = m;
  r.tm_sec  = s;
  r.tm_isdst= 1;
  tv_sec = mktime(&r), tv_usec = 1000*ms;
}

/* @Method{Time Time::currentTime()}
 *
 * @Description{returns the current time}
 */
Time Time::currentTime()
{
  struct timeval current;
  gettimeofday(&current, 0);
  return Time (current);
};
