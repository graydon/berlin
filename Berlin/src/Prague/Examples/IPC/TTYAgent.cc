/*$Id: TTYAgent.cc,v 1.2 1999/11/17 02:03:18 stefan Exp $
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
#include <Prague/IPC/TTYAgent.hh>
#include <string>

using namespace Prague;

Coprocess *agent;

class Output : public Coprocess::IONotifier
{
public:
  virtual bool notify(Agent::iomask_t mask)
  {
    if (mask != Agent::outready) return false;
    istream is(agent->obuf());
    string line;
    getline(is, line);
    cout << "output : '" <<  line << '\'' << endl;
    return true;
  }
};

class ConnectionClosed : public Coprocess::EOFNotifier
{
public:
  virtual void notify(Agent::iomask_t mask)
  {
    if (mask == Agent::outready)
      cout << "output : connection closed";
  }
};

int main (int argc, char **argv)
{
  Output *out = new Output;
  ConnectionClosed *eof = new ConnectionClosed;
  agent = new TTYAgent("sh", out, eof);
  agent->start();
  while (cin && agent->ibuf())
    {
      ostream os(agent->ibuf());
      string line;
      cout << "input :";
      getline(cin, line);
      os << line << endl;
      Thread::delay(500);
    }
  if (!agent->ibuf())
    {
      if (agent->state() == Coprocess::exited)
	cerr << "process exited with value " << agent->value() << endl;
      else if (agent->state() == Coprocess::signaled)
	cerr << "process killed with signal " << agent->value() << endl;
    }
  delete agent;
  delete eof;
  delete out;
}
