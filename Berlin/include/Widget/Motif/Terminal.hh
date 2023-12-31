/*$Id: Terminal.hh,v 1.6 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _Motif_Terminal_hh
#define _Motif_Terminal_hh

#include <Warsaw/config.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/StreamBuffer.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/ObserverImpl.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/RefCountVar.hh>
#include <Prague/IPC/TTYAgent.hh>
#include <vector>

namespace Motif
{

class Terminal : public MonoGraphic
{
  class Input : public ObserverImpl
  {
  public:
    Input(Terminal *t) : terminal(t) {}
    virtual void update(const CORBA::Any &);
  private:
    Terminal *terminal;
  };
  class Output : public Prague::Coprocess::IONotifier
  {
  public:
    Output(Terminal *t) : terminal(t) {}
    virtual bool notify(Prague::Agent::iomask);
  private:
    Terminal *terminal;
  };
  friend class Input;
  friend class Output;
 public:
  Terminal(Warsaw::CommandKit_ptr);
  Warsaw::StreamBuffer_ptr input() { return Warsaw::StreamBuffer::_duplicate(ibuf);}
  Warsaw::StreamBuffer_ptr output() { return Warsaw::StreamBuffer::_duplicate(obuf);}
 private:
  Input *_input;
  Output *_output;
  Prague::TTYAgent *agent;
  RefCount_var<Warsaw::StreamBuffer> ibuf;
  RefCount_var<Warsaw::StreamBuffer> obuf;
};

};
#endif
