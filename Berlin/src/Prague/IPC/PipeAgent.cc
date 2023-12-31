/*$Id: PipeAgent.cc,v 1.9 2001/03/25 08:25:16 stefan Exp $
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
#include "Prague/IPC/PipeAgent.hh"
#include "Prague/IPC/pipebuf.hh"
#include <cerrno>
#include <cstdio>
#include <unistd.h>

using namespace Prague;

PipeAgent::PipeAgent(const std::string &cmd, IONotifier *io, EOFNotifier *eof)
  : Coprocess(cmd, io, eof)
{}

PipeAgent::~PipeAgent()
{
  shutdown(in|out|err);
}

void PipeAgent::start()
{
  if (_id >= 0)
    {
      terminate();
      pipebuf *pin  = new pipebuf(std::ios::out); // the stdin for the child is an output stream for the parent...
      pipebuf *pout = new pipebuf(std::ios::in);  // the stdout for the child is an input stream for the parent...
      pipebuf *perr = new pipebuf(std::ios::in);  // the stderr for the child is an input stream for the parent...
      int fin = pin->open();
      int fout = pout->open();
      int ferr = perr->open();
//       if (fin == -1 || fout == -1 || ferr == -1) { Error("communication setup failed", true); return;}
      switch(_id = fork())
	{
	case -1:
	  _id = 0;
// 	  SystemError("cannot fork", true);
	  return;
	case  0:
	  dup2(fin, fileno(stdin)); close(fin);
	  dup2(fout, fileno(stdout)); close(fout);
	  dup2(ferr, fileno(stderr)); close(ferr);
	  const char *argv[4];
	  argv[0] = "/bin/sh";
	  argv[1] = "-c";
	  argv[2] = _path.c_str();
	  argv[3] = 0;
	  execvp ("/bin/sh", (char**) argv);
	  std::perror("/bin/sh");
	  _exit(EXIT_FAILURE);
	  break;
	default:
 	  _inbuf = pin; close(fin);
 	  _outbuf = pout; close(fout);
 	  _errbuf = perr; close(ferr);
	  _inbuf->async(true);
	  _outbuf->async(true);
	  _errbuf->async(true);
 	  break;
	}
    }
//   mask(in|out|err);
  mask(out|err);
  Coprocess::start();
};
