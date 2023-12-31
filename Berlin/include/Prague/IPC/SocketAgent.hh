/*$Id: SocketAgent.hh,v 1.4 2001/03/27 05:38:42 stefan Exp $
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
#ifndef _Prague_SocketAgent_hh
#define _Prague_SocketAgent_hh

#include <Prague/IPC/Agent.hh>
#include <Prague/IPC/sockbuf.hh>

namespace Prague
{

//. a SocketAgent is a socket based Agent. It is conceptually lightweight,
//. such that it can be used to generate a pipeline, i.e. similar to the
//. State pattern, SocketAgents can implement a specific strategy in the
//. process method, and then create new SocketAgents for the given socket
//. to get a 'state transition' effect. Since Agents are reference counted,
//. the Dispatcher will take care of deleting them.
class SocketAgent : public Agent
{
public:
  SocketAgent(sockbuf *);
  virtual         ~SocketAgent();
  virtual sockbuf *ibuf() { return _socket;}
  virtual sockbuf *obuf() { return _socket;}
  virtual sockbuf *ebuf() { return 0;}
  sockbuf *release_buf();
private:
  sockbuf *_socket;
};

};

#endif
