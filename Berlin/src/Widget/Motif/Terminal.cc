/*$Id: Terminal.cc,v 1.9 2001/04/18 06:07:28 stefan Exp $
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

#include "Widget/Motif/Terminal.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;
using namespace Motif;

void Terminal::Input::update(const CORBA::Any &)
{
  Trace trace("Terminal::Input::update");
  /*
   * the source
   */
  Warsaw::StreamBuffer::Data_var data = terminal->ibuf->read();
  char *begin = (char *)data->get_buffer();
  char *end   = begin + data->length();
  /*
   * the sink
   */
  std::ostream os(terminal->agent->ibuf());
  for (char *i = begin; i != end; i++) os.put(*i);
  os.flush();
}


bool Terminal::Output::notify(Agent::iomask mask)
{
  Trace trace("Terminal::output::notify");
  if (mask != Agent::outready) return false;
  /*
   * the source
   */
  std::istream is(terminal->agent->obuf());
  std::string line;
  while (getline(is, line))
    {
      StreamBuffer::Data data(line.length(), line.length(), (CORBA::Octet *)line.data(), false);
      terminal->obuf->write(data);
      terminal->obuf->flush();
    }
  return true;
}

Terminal::Terminal(CommandKit_ptr command)
  : // ControllerImpl(false),
    _input(new Input(this)),
    _output(new Output(this)),
    agent(new TTYAgent("sh", _output, 0)),
    ibuf(RefCount_var<StreamBuffer>::increment(command->stream(1))),
    obuf(RefCount_var<StreamBuffer>::increment(command->stream(1024)))
{
  Trace trace("Terminal::Terminal");
  ibuf->attach(Observer_var(_input->_this()));
  agent->start();
}
