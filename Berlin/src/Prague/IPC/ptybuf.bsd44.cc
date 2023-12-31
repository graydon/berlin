/*$Id: ptybuf.bsd44.cc,v 1.3 1999/11/16 02:15:20 stefan Exp $
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
#include <cerrno>
#include <cstdio>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace Prague;

int ptybuf::openpty()
{
  ptydev = "/dev/ptyXY";
  ttydev = "/dev/ttyXY";
  for (char *c = "pqrstuvwxyzPQRST"; *c != 0; c++)
    {
      ptydev[8] = *c;
      ptydev[9] = '0';
      /*
       * If this name, "/dev/ptyX0" doesn't even exist,
       * then we can quit now.  It means the system doesn't
       * have /dev entries for this group of 16 ptys.
       */
      struct stat statbuff;
      if (stat(ptydev.c_str(), &statbuff) < 0) break;
      for (char *i = "0123456789abcdef"; *i != 0; i++)
	{
	  ptydev[9] = *i;
	  data->fd = open(ptydev.c_str(), O_RDWR);
          if (data->fd >= 0)
	    {
	      ttydev[8] = ptydev[8];
	      ttydev[9] = ptydev[9];
	      if (access(ttydev.c_str(), R_OK | W_OK) == 0) return data->fd;
	    }
	  else if (errno == ENOENT) return -1;
	}
    }
  return -1;
}

int ptybuf::opentty()
{
  pid_t pid = setsid();
  if (pid < 0) perror("ptybuf::opentty");
  /*
   * Open the slave half of a pseudo-terminal.
   * Note that the master half of a pty is a single-open device,
   * so there isn't a race condition between opening the master
   * above and opening the slave below.  The only way the slave
   * open will fail is if someone has opened the slave without
   * first opening the master.
   */
  int slavefd = open(ttydev.c_str(), O_RDWR);
  if (slavefd < 0)
    {
      perror(ttydev.c_str());
      close(fd());
      slavefd = -1;
    }
  return slavefd;
}
