/*$Id: TTYAgent.hh,v 1.4 1999/11/17 02:03:39 stefan Exp $
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
#ifndef _TTYAgent_hh
#define _TTYAgent_hh

#include <Prague/IPC/Coprocess.hh>

namespace Prague
{

class TTYAgent : public Coprocess
{
public:
  TTYAgent(const string &cmd, IONotifier *, EOFNotifier * = 0);
  virtual ~TTYAgent();
  virtual void  start();
  void setWindowSize(unsigned short, unsigned short);
private:
  TTYAgent(const TTYAgent &);
  TTYAgent &operator = (const TTYAgent &);
};

};

#endif /* _TTYAgent_hh */
