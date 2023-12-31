/*$Id: SHM.hh,v 1.5 2001/01/15 02:49:18 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _Prague_SHM_hh
#define _Prague_SHM_hh

#include <sys/types.h>
#include <sys/ipc.h>

namespace Prague
{

struct SHM
{
  static int   allocate(key_t, size_t, int = IPC_CREAT | 0666);
  static int   allocate(size_t, int = IPC_CREAT | 0666);
  static void  deallocate(int);
  static void *attach(int);
  static void  detach(void *);
  int id;
  unsigned int count;
};

};

#endif /* _SHM_hh */
