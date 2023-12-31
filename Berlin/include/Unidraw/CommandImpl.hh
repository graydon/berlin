/*$Id: CommandImpl.hh,v 1.2 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _CommandImpl_hh
#define _CommandImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/FigureKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/UnidrawKit.hh>
#include <Berlin/ServantBase.hh>

class CommandImpl : public virtual POA_Unidraw::Command,
		    public ServantBase
{
public:
  CommandImpl();
  virtual ~CommandImpl();
  virtual void execute();
  virtual void store(Unidraw::Model_ptr, const CORBA::Any &);
  virtual CORBA::Any *recall(Unidraw::Model_ptr);
  virtual void destroy() { deactivate();}
};

#endif
