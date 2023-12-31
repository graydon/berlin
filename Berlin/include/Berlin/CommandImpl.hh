/*$Id: CommandImpl.hh,v 1.1 2000/09/05 21:12:00 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _CommandImpl_hh
#define _CommandImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Command.hh>
#include <Berlin/ServantBase.hh>

class CommandImpl : public virtual POA_Warsaw::Command,
		    public virtual ServantBase
{
public:
  virtual void execute(const CORBA::Any &) = 0;
  virtual void destroy() { deactivate();}
};

#endif
