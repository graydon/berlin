/*$Id: TextBufferImpl.hh,v 1.4 1999/09/30 17:23:33 gray Exp $
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
#ifndef _TextBufferImpl_hh
#define _TextBufferImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/TextBuffer.hh>
#include <Berlin/SubjectImpl.hh>
#include <Berlin/GapBuffer.hh>
#include <Prague/Sys/Thread.hh>

class TextBufferImpl : implements(TextBuffer), public SubjectImpl
{
 public:
  TextBufferImpl() {}
  virtual ~TextBufferImpl() {}
  virtual CORBA::Long size();
  virtual CORBA::Long position();
  virtual void position(CORBA::Long);
  virtual void forward();
  virtual void backward();
  virtual void shift(CORBA::Long d);
  virtual void insertChar(Unichar);
  virtual void insertString(const Unistring &);
  virtual void removeLeft(CORBA::Long);
  virtual void removeRight(CORBA::Long);
 private:
  GapBuffer<Unichar, 32> buffer;
  Prague::Mutex mutex;
};

#endif /* _TextBuffer_hh */
