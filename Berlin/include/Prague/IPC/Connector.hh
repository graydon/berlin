/*$Id: Connector.hh,v 1.2 1999/04/27 20:11:10 gray Exp $
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
#ifndef _Connector_hh
#define _Connector_hh

#include <Prague/IPC/Agent.hh>

namespace Prague
{

/* @Class {Connector : public Agent}
 *
 * @Description {creates and handles (client/server) connections}
 */
class Connector : public Agent
{
public:
  Connector(Notifier *) {}
  Connector(const Connector &C) : Agent(C) {}
  virtual        ~Connector() {}
  virtual ipcbuf *ibuf() { return 0;}
  virtual ipcbuf *obuf() { return 0;}
  virtual ipcbuf *ebuf() { return 0;}
  virtual bool  pending() { return false;}
  virtual void  dispatchpending() {}
protected:
  virtual void  outputEOF() = 0;
  virtual void  errorEOF() = 0;
private:
};

};

#endif /* _Connector_hh */
