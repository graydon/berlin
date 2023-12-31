/*$Id: StreamBufferImpl.cc,v 1.3 1999/09/30 17:23:34 gray Exp $
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

#include <Widget/StreamBufferImpl.hh>

using namespace Prague;

CORBA::Long StreamBufferImpl::size()
{
  MutexGuard guard(mutex);
  return length;
}

CORBA::Long StreamBufferImpl::available()
{
  MutexGuard guard(mutex);
  return buffer.size();
}

void StreamBufferImpl::write(const Data &data)
{
  bool overflow = false;
  {
    MutexGuard guard(mutex);
    unsigned long l = data.length();
    unsigned long s = buffer.size();
    if (s + l > buffer.capacity()) buffer.reserve(s + l);
    for (unsigned long i = 0; i != l; i++) buffer.push_back(data[i]);
    if (buffer.size() >= length) overflow = true;
  }
  if (overflow)
    {
      CORBA::Any any;
      notify(any);
    }
}

void StreamBufferImpl::flush()
{
  bool overflow = false;
  {
    MutexGuard guard(mutex); 
    if (buffer.size()) overflow = true;
  }
  if (overflow)
    {
      CORBA::Any any;
      notify(any);
    }  
}

StreamBuffer::Data *StreamBufferImpl::read()
{
  MutexGuard guard(mutex);
  Data *data = new Data; data->length(buffer.size());
  for (unsigned long i = 0; i != buffer.size(); i++) (*data)[i] = buffer[i];
  buffer.erase(buffer.begin(), buffer.end());
  buffer.reserve(length);
  return data;
}

