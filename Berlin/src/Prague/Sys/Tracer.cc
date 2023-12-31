/*$Id: Tracer.cc,v 1.2 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/Sys/Tracer.hh"
#include "Prague/Sys/Time.hh"

using namespace Prague;

std::vector<Tracer::Event>   Tracer::_events(256);
Time                         Tracer::_start = Time::currentTime();
Thread::Data<unsigned short> Tracer::_indent(0);
Mutex                        Tracer::_mutex;
unsigned int                 Tracer::_next = 0;
bool                         Tracer::_wrapped = false;
bool                         Tracer::_log = false;
