/*$Id: PipeAgent.hh,v 1.8 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Prague_PipeAgent_hh
#define _Prague_PipeAgent_hh

#include <Prague/IPC/Coprocess.hh>

namespace Prague
{

//. a PipeAgent uses a pipe to communicate with the coprocess
class PipeAgent : public Coprocess
{
public:
  PipeAgent(const std::string &, IONotifier *, EOFNotifier * = 0);
  virtual      ~PipeAgent();
  //. spawns a child process after creating a pipe, then redirects i/o to it
  virtual void  start();
private:
  PipeAgent(const PipeAgent &);
  PipeAgent &operator = (const PipeAgent &);
};

};

#endif
