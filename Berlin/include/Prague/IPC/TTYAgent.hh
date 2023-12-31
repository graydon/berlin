/*$Id: TTYAgent.hh,v 1.8 2001/03/25 08:25:16 stefan Exp $
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
#ifndef _Prague_TTYAgent_hh
#define _Prague_TTYAgent_hh

#include <Prague/IPC/Coprocess.hh>

namespace Prague
{

//. a Coprocess that uses a tty/pty pair for communication
class TTYAgent : public Coprocess
{
public:
  TTYAgent(const std::string &cmd, IONotifier *, EOFNotifier * = 0);
  virtual ~TTYAgent();
  //. spawns a child process after creating a tty/pty pair, then redirects i/o to it
  virtual void  start();
  //. since the tty preformats the coprocess' output, it needs to
  //. know the geometry for the text to fit in
  void set_window_size(unsigned short, unsigned short);
private:
  TTYAgent(const TTYAgent &);
  TTYAgent &operator = (const TTYAgent &);
  bool _running;
};

};

#endif /* _Prague_TTYAgent_hh */
