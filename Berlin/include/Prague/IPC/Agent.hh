/*$Id: Agent.hh,v 1.8 1999/11/16 02:15:20 stefan Exp $
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
#ifndef _Agent_hh
#define _Agent_hh

#include <Prague/IPC/ipcbuf.hh>

namespace Prague
{

class Agent
{
  friend class Dispatcher;
public:
  enum iomask_t {none = 0x00, outready = 0x01, inready = 0x02, errready = 0x04,
		 outexc = 0x10, inexc = 0x20, errexc = 0x40,
		 out = 0x11, in = 0x22, err = 0x44,
		 asyncio = 0xff};
  Agent();
  virtual ~Agent();

  virtual void start();
  virtual void stop();
  void mask(short);
  short mask() const { return iomask;}
  virtual ipcbuf *ibuf() = 0;
  virtual ipcbuf *obuf() = 0;
  virtual ipcbuf *ebuf() = 0;
protected:
  virtual bool processIO(int, iomask_t) = 0;
private:
  Agent(const Agent &);
  Agent &operator = (const Agent &);
  short iomask;
  bool  running : 1;
};

};

#endif /* _Agent_hh */
