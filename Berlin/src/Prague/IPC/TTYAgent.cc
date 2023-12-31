/*$Id: TTYAgent.cc,v 1.5 1999/11/17 02:03:18 stefan Exp $
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
#include "Prague/IPC/ptybuf.hh"
#include "Prague/IPC/TTYAgent.hh"
#include <unistd.h>
#include <cstdio>

using namespace Prague;

TTYAgent::TTYAgent(const string &cmd, IONotifier *io, EOFNotifier *eof)
  : Coprocess(cmd, io, eof)
{}

TTYAgent::~TTYAgent()
{
  shutdown(in|out|err);
}

void TTYAgent::start()
{
  if (id >= 0)
    {
      terminate();
      ptybuf *pty  = new ptybuf;
      int fd = pty->openpty();
      switch(id = fork())
	{
	case -1:
	  id = 0;
// 	  SystemError("cannot fork", true);
	  return;
	case  0:
	  {
            int fds = pty->opentty();
	    if (fds < 0) _exit(EXIT_FAILURE);
            close(fd);
            dup2(fds, fileno(stdin));
            dup2(fds, fileno(stdout));
            dup2(fds, fileno(stderr));
            if (fds > fileno(stderr)) close(fds);
            const char *argv[4];
            argv[0] = "/bin/sh";
            argv[1] = "-c";
            argv[2] = path.c_str();
            argv[3] = 0;
            execvp ("/bin/sh", (char**) argv);
            perror("/bin/sh");
            _exit(EXIT_FAILURE);
            break;
	  }
	default:
	  pty->setup();
 	  inbuf = outbuf = pty;
	  errbuf = 0;
	  inbuf->setnonblocking();
 	  break;
	}
    }
  mask(out);
  Coprocess::start();
};

void TTYAgent::setWindowSize(unsigned short columns, unsigned short rows)
{
#if defined(HAVE_IOCTL)
  struct winsize ws;
  ws.ws_col = columns;
  ws.ws_row = rows;
  ws.ws_xpixel = ws.ws_ypixel = 0;
  if (ioctl (master, TIOCSWINSZ, &ws) != 0);
#endif
};
