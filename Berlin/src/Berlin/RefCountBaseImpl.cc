/*$Id: RefCountBaseImpl.cc,v 1.8 2001/02/06 19:46:16 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Berlin/RefCountBaseImpl.hh"
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

static Mutex mutex;

RefCountBaseImpl::RefCountBaseImpl() : refcount(1) {}
RefCountBaseImpl::~RefCountBaseImpl() { Trace trace("RefCountBaseImpl::~RefCountBaseImpl");}
void RefCountBaseImpl::increment()
{
  Trace trace("RefCountBaseImpl::increment");
  Prague::Guard<Mutex> guard(mutex);
  refcount++;
}

void RefCountBaseImpl::decrement()
{
  Trace trace("RefCountBaseImpl::decrement");
  Prague::Guard<Mutex> guard(mutex);
  if (!--refcount) deactivate();
}

