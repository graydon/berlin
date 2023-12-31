/*$Id: DataType.cc,v 1.1 1999/11/12 16:35:57 stefan Exp $
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

#include <Prague/Sys/DataTypeManager.hh>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 3)
    {
      cerr << "Usage : " << argv[0] << " <database> <file>" << endl;
      exit(-1);
    }
  DataTypeManager types(argv[1]);
  cout << argv[2] << " is of type " << types.match(argv[2]) << endl;
}
