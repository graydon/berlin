/*$Id: ptybuf.hh,v 1.5 2001/03/25 08:25:16 stefan Exp $
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
#ifndef _ptybuf_hh
#define _ptybuf_hh

#include <Prague/IPC/ipcbuf.hh>
#include <string>

namespace Prague
{

class ptybuf : public ipcbuf
{
  class backup;
public:
  ptybuf();
  virtual ~ptybuf();
  virtual std::streamsize sys_read(char *, std::streamsize);
  const std::string &name() const { return ptydev;}
  int openpty();
  int opentty();
  void setup();
protected:
  std::string ptydev;
  std::string ttydev;
private:
  backup *save;
};

};

#endif
