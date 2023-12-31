/*$Id: TextBufferImpl.cc,v 1.8 2001/04/15 15:15:15 tobias Exp $
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

#include <Command/TextBufferImpl.hh>
#include <iostream>

using namespace Prague;
using namespace Warsaw;

TextBufferImpl::TextBufferImpl() {}
TextBufferImpl::~TextBufferImpl() {}

CORBA::Long TextBufferImpl::size()
{
  Prague::Guard<Mutex> guard(mutex);
  return buffer.size();
}

Unistring *TextBufferImpl::value()
{
  Prague::Guard<Mutex> guard(mutex);
  Unistring *us = new Unistring(buffer.size(), buffer.size(), const_cast<Unichar *>(buffer.get()), false);
  return us;
}

Unistring *TextBufferImpl::get_chars(CORBA::ULong pos, CORBA::ULong len)
{
  Prague::Guard<Mutex> guard(mutex);
  CORBA::ULong fin = buffer.size();
  CORBA::ULong start = pos > fin ? fin : pos;
  CORBA::ULong end = start + len > fin ? fin : start + len;
  Unistring *us = new Unistring(end-start, end-start, const_cast<Unichar *>(buffer.get() + start), false);
  return us;
}


CORBA::Long TextBufferImpl::position()
{
  Prague::Guard<Mutex> guard(mutex);
  return buffer.position();
}

void TextBufferImpl::position(CORBA::Long p)
{
  Prague::Guard<Mutex> guard(mutex);
  buffer.position(p);
}

void TextBufferImpl::forward()
{
  Warsaw::TextBuffer::Change ch;  
  {
    Prague::Guard<Mutex> guard(mutex);
    buffer.forward();
    ch.pos = buffer.position();
  }
  ch.len = 0;
  ch.type = Warsaw::TextBuffer::cursor;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::backward()
{
  Warsaw::TextBuffer::Change ch;  
  {
    Prague::Guard<Mutex> guard(mutex);
    buffer.backward();
    ch.pos = buffer.position();
  }
  ch.len = 0;
  ch.type = Warsaw::TextBuffer::cursor;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::shift(CORBA::Long d)
{
  Warsaw::TextBuffer::Change ch;  
  {
    Prague::Guard<Mutex> guard(mutex);
    buffer.shift(d);
    ch.pos = buffer.position();
  }
  ch.len = 0;
  ch.type = Warsaw::TextBuffer::cursor;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::insert_char(Unichar u)
{
  Warsaw::TextBuffer::Change ch;  
  {
    Prague::Guard<Mutex> guard(mutex);
    ch.pos = buffer.position();
    buffer.insert(u);
  }
  ch.len = 1;
  ch.type = Warsaw::TextBuffer::insert;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::insert_string(const Unistring &s)
{
  if (s.length() == 0) return;

  Warsaw::TextBuffer::Change ch;  
  ch.len = s.length();
  Unichar u[ch.len];
  for (long i = 0; i < ch.len; i++) u[i] = s[i];

  {
    Prague::Guard<Mutex> guard(mutex);
    ch.pos = buffer.position();
    buffer.insert(u,ch.len);
  }

  ch.type = Warsaw::TextBuffer::insert;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::remove_backward(CORBA::Long n)
{
  Warsaw::TextBuffer::Change ch;  
  ch.len = -n;

  {
    Prague::Guard<Mutex> guard(mutex);
    ch.pos = buffer.position();
    buffer.remove_backward(n);
  }

  ch.type = Warsaw::TextBuffer::remove;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::remove_forward(CORBA::Long n)
{
  Warsaw::TextBuffer::Change ch;  
  ch.len = n;

  {
    Prague::Guard<Mutex> guard(mutex);
    ch.pos = buffer.position();
    buffer.remove_forward(n);
  }
  ch.type = Warsaw::TextBuffer::remove;
  CORBA::Any any;
  any <<= ch;
  notify(any);
}

void TextBufferImpl::clear() {
    Warsaw::TextBuffer::Change ch;
    ch.type = Warsaw::TextBuffer::remove;
    {
	Prague::Guard<Mutex> guard(mutex);
	ch.len = buffer.size();
	ch.pos = 0;
	buffer.clear_buffer();
    }
    CORBA::Any any;
    any <<= ch;
    notify(any);
}
