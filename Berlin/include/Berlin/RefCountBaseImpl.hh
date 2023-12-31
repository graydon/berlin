/*$Id: RefCountBaseImpl.hh,v 1.2 2000/08/31 18:52:31 stefan Exp $
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
#ifndef _RefCountBaseImpl_hh
#define _RefCountBaseImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/RefCountBase.hh>
#include <Prague/Sys/Tracer.hh>
#include <Berlin/ServantBase.hh>

//.implement distributed reference counting
class RefCountBaseImpl : public virtual POA_Warsaw::RefCountBase,
                         public virtual ServantBase
{
public:
  RefCountBaseImpl();
  virtual ~RefCountBaseImpl();
  virtual void increment();
  virtual void decrement();
private:
  int refcount;
};

#endif
