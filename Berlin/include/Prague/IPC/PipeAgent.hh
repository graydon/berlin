/*$Id: PipeAgent.hh,v 1.5 1999/11/17 02:03:39 stefan Exp $
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
#ifndef _PipeAgent_hh
#define _PipeAgent_hh

#include <Prague/IPC/Coprocess.hh>

namespace Prague
{

char *sigName(int);
char *statusName(int);

class PipeAgent : public Coprocess
{
public:
  PipeAgent(const string &, IONotifier *, EOFNotifier * = 0);
  virtual      ~PipeAgent();
  virtual void  start();
private:
  PipeAgent(const PipeAgent &);
  PipeAgent &operator = (const PipeAgent &);
  Mutex mutex;
};

};

#endif /* _PipeAgent_hh */
