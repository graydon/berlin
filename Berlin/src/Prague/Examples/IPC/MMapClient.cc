/*$Id: MMapClient.cc,v 1.1 1999/10/15 17:59:25 gray Exp $
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

#include <Prague/Sys/File.hh>
#include <Prague/IPC/mmapbuf.hh>
#include <unistd.h>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 2)
    {
      cerr << "Usage : " << argv[0] << " <filename>\n";
      exit(-1);
    }
  string file = argv[1];
  streambuf *mbuf = new mmapbuf(file, -1, ios::in);
  istream is(mbuf);
  string buf;
  getline(is, buf);
  cout << "read : " << buf << endl;
  delete mbuf;
}
