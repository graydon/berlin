/*$Id: echo.cc,v 1.6 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Prague/Network/echo.hh>
#include <Prague/Sys/Fork.hh>
#include <cerrno>

using namespace Prague;

void echo::echobuf::serve_clients(int portno)
{
  if (protocol_name())
    {
      if (portno < 0) sockinetbuf::bind(sockinetaddr((unsigned long) INADDR_ANY, "echo", protocol_name()));
      else if (portno <= 1024)
	{
	  sockinetbuf::bind(sockinetaddr((unsigned long) INADDR_ANY, portno));
	  sockinetaddr local = localaddr();
	  std::cout << "Host: " << local.hostname() << '\n' << "Port: " << local.port() << std::endl;
	}
      else sockinetbuf::bind(sockinetaddr((unsigned long) INADDR_ANY, portno));
      // act as a server now
      listen(sockbuf::somaxconn);
      // commit suicide when we receive SIGTERM
      Fork::suicide_on_signal(Signal::terminate);
      for (;;)
	{
	  sockinetbuf *socket = accept();
	  Fork f (1, 1); // kill my children when I get terminated.
	  if (f.child())
	    {
	      char buf [1024];
	      int  rcnt;
	      while ((rcnt = socket->read(buf, 1024)) > 0)
		while (rcnt != 0)
		  {
		    int wcnt = socket->write(buf, rcnt);
		    if (wcnt == -1) throw sockerr(errno);
		    rcnt -= wcnt;
		  }
	      delete socket;
	      sleep(300);
	      exit (0);
	    }
	  else delete socket;
	}
    }
}
  
