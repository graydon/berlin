/*$Id: Agent.cc,v 1.5 1999/11/16 02:15:20 stefan Exp $
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
#include "Prague/IPC/Agent.hh"
#include "Prague/IPC/Dispatcher.hh"

using namespace Prague;

Agent::Agent()
  : iomask(none), running(false)
{}

Agent::~Agent()
{
  if (running)
    {
      Dispatcher::instance()->release(this);
      running = false;
    }
};

void Agent::start()
{
  running = true;
  if (iomask & in && ibuf()) Dispatcher::instance()->bind(ibuf()->fd(), this, in);
  if (iomask & out && obuf()) Dispatcher::instance()->bind(obuf()->fd(), this, out);
  if (iomask & err && ebuf()) Dispatcher::instance()->bind(ebuf()->fd(), this, err);
}

void Agent::mask(short m)
{
  if (iomask == m) return;
  if (running)
    {
      if ((iomask ^ m) & in)
	if (iomask & in && ibuf()) Dispatcher::instance()->release(ibuf()->fd());
	else  Dispatcher::instance()->bind(ibuf()->fd(), this, in);
      if ((iomask ^ m) & out)
	if (iomask & out && obuf()) Dispatcher::instance()->release(obuf()->fd());
	else  Dispatcher::instance()->bind(obuf()->fd(), this, out);
      if ((iomask ^ m) & err)
	if (iomask & err && ebuf()) Dispatcher::instance()->release(ebuf()->fd());
	else  Dispatcher::instance()->bind(ebuf()->fd(), this, err);
    }
  iomask = m;
}

void Agent::stop()
{
  mask(none);
  Dispatcher::instance()->release(this);
  running = false;
}
