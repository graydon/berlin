/*$Id: ptybuf.svr4.cc,v 1.2 1999/04/27 20:09:49 gray Exp $
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

#include <fcntl.h>

using namespace Prague;

extern char *ptsname(int); /* prototype not in any system header */
extern int grantpt(int);
extern int unlockpt(int);

int ptybuf::openpty()
{
  char *ptr;
  strcpy(ptydev, "/dev/ptmx");	/* in case open fails */
  int fdm = open(ptydev, O_RDWR);
  if (fdm < 0) return -1;
  if (grantpt(fdm) < 0)	/* grant access to slave */
    {
      close(fdm);
      return -2;
    }
  if (unlockpt(fdm) < 0) /* clear slave's lock flag */
    {
      close(fdm);
      return -3;
    }
  if ((ptr = ptsname(fdm)) == 0) /* get slave's name */
    {
      close(fdm);
      return -4;
    }
  strcpy(ptydev, ptr);	/* return name of slave */
  data->fd = fdm;
  return fdm;	     	/* return fd of master */
}

int ptybuf::opentty()
{
  int fds;
  /* following should allocate controlling terminal */
  if ((fds = open(ptydev, O_RDWR)) < 0)
    {
      close(fd());
      return(-5);
    }
  /*
  if (ioctl(fds, I_PUSH, "ptem") < 0)
    {
      close(fd());
      close(fds);
      return(-6);
    }
  if (ioctl(fds, I_PUSH, "ldterm") < 0)
    {
      close(fd());
      close(fds);
      return(-7);
    }
  if (ioctl(fds, I_PUSH, "ttcompat") < 0)
    {
      close(fd());
      close(fds);
      return(-8);
    }
  */
  return(fds);
}
