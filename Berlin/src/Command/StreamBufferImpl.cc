/*$Id: StreamBufferImpl.cc,v 1.4 2001/02/06 19:46:16 tobias Exp $
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

#include <Command/StreamBufferImpl.hh>

using namespace Prague;
using namespace Warsaw;

CORBA::Long StreamBufferImpl::size()
{
  Prague::Guard<Mutex> guard(mutex);
  return length;
}

CORBA::Long StreamBufferImpl::available()
{
  Prague::Guard<Mutex> guard(mutex);
  return buffer.size();
}

void StreamBufferImpl::write(const Warsaw::StreamBuffer::Data &data)
{
  bool overflow = false;
  {
    Prague::Guard<Mutex> guard(mutex);
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
    Prague::Guard<Mutex> guard(mutex); 
    if (buffer.size()) overflow = true;
  }
  if (overflow)
    {
      CORBA::Any any;
      notify(any);
    }  
}

Warsaw::StreamBuffer::Data *StreamBufferImpl::read()
{
  Prague::Guard<Mutex> guard(mutex);
  Warsaw::StreamBuffer::Data_var data = new Warsaw::StreamBuffer::Data;
  data->length(buffer.size());
  for (unsigned long i = 0; i != buffer.size(); i++) data[i] = buffer[i];
  buffer.erase(buffer.begin(), buffer.end());
  buffer.reserve(length);
  return data._retn();
}

