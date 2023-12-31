/*$Id: StreamBufferImpl.hh,v 1.5 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _StreamBufferImpl_hh
#define _StreamBufferImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/StreamBuffer.hh>
#include <Berlin/SubjectImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <vector>

class StreamBufferImpl : public virtual POA_Warsaw::StreamBuffer,
			 public SubjectImpl
{
 public:
  StreamBufferImpl(long l) : length(l) { buffer.reserve(length);}
  virtual ~StreamBufferImpl() {}
  virtual CORBA::Long size();
  virtual CORBA::Long available();
  virtual Warsaw::StreamBuffer::Data *read();
  virtual void write(const Warsaw::StreamBuffer::Data &);
  virtual void flush();
 private:
  size_t length;
  std::vector<CORBA::Octet> buffer;
  Prague::Mutex mutex;
};

#endif
