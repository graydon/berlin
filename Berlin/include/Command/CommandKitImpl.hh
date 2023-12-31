/*$Id: CommandKitImpl.hh,v 1.4 1999/09/10 20:57:37 gray Exp $
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
#ifndef _CommandKitImpl_hh
#define _CommandKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/CommandKit.hh>
#include <Berlin/CloneableImpl.hh>

class CommandKitImpl : lcimplements(CommandKit), virtual public CloneableImpl
{
public:
  
  CommandKitImpl();
  virtual ~CommandKitImpl();
  Reactor_ptr asyncReactor(const ReactorBindingList &);
  Reactor_ptr syncReactor(const ReactorBindingList &);
  Command_ptr sendMessage(const Message &, MessageListener_ptr);
  Command_ptr forwardMessage(MessageListener_ptr);
  Command_ptr log(const char *);
};

#endif
