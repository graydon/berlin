/*$Id: Agent.hh,v 1.15 2001/03/27 05:38:42 stefan Exp $
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
#ifndef _Prague_Agent_hh
#define _Prague_Agent_hh

#include <Prague/IPC/ipcbuf.hh>

namespace Prague
{

//. Agents are asynchronous i/o event handlers. Together with the Dispatcher
//. class, they implement the Reactor pattern. 
class Agent
{
  friend class Dispatcher;
public:
  enum iomask {none = 0x00, outready = 0x01, inready = 0x02, errready = 0x04,
	       outexc = 0x10, inexc = 0x20, errexc = 0x40,
	       out = 0x11, in = 0x22, err = 0x44,
	       asyncio = 0xff};
  Agent();
  virtual ~Agent();

  //. bind the Agent to the Dispatcher, which increments the ref counter, and
  //. registers the i/o channels as specified with the iomask.
  virtual void start();
  //. release the Agent from the Dispatcher, which decrements the ref counter.
  virtual void stop();
  bool running() const { return _running;}
  //. set a new iomask, to be used by the Dispatcher to determine what events
  //. this agent is interested in.
  void mask(short);
  //. return the current iomask.
  short mask() const { return _iomask;}
  //. return the buffer referring to the peer's input - i.e. it is an output buffer.
  virtual ipcbuf *ibuf() = 0;
  //. return the buffer referring to the peer's output - i.e. it is an input buffer.
  virtual ipcbuf *obuf() = 0;
  //. return the buffer referring to the peer's error - i.e. it is an input buffer.
  virtual ipcbuf *ebuf() = 0;
  //. increment the ref counter
  void add_ref() { ++_refcount;}
  //. decrement the ref counter. If the counter becomes zero, the Agent is deleted.
  void remove_ref() { if (!--_refcount) delete this;}
private:
  Agent(const Agent &);
  Agent &operator = (const Agent &);
  //. the actual event handler. It is called with the fd on which the event occured,
  //. and the iomask telling the kind of event. Overwrite that method in derived classes
  //. to implement a specific behavior.
  virtual bool process(int, iomask) = 0;
  short _refcount;
  short _iomask;
  bool  _running : 1;
};

};

#endif
