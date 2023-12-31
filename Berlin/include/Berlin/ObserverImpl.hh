/*$Id: ObserverImpl.hh,v 1.1 2000/09/12 19:25:59 stefan Exp $
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
#ifndef _ObserverImpl_hh
#define _ObserverImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Observer.hh>
#include <Warsaw/Subject.hh>
#include <Berlin/ServantBase.hh>
#include <Berlin/IdentifiableImpl.hh>

class ObserverImpl : public virtual POA_Warsaw::Observer,
		     public virtual ServantBase,
		     public virtual IdentifiableImpl
{
public:
  virtual void destroy() { deactivate();}
};

#endif 
