/*$Id: Regex.cc,v 1.2 2000/09/23 21:18:36 stefan Exp $
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

#include <Prague/Sys/regex.hh>
#include <iostream>

using namespace Prague;

main()
{
  regex rxfile("[_a-zA-Z0-9\\-\\.]*\\.cc$|"
	       "[_a-zA-Z0-9\\-\\.]*\\.C$|"
	       "[_a-zA-Z0-9\\-\\.]*\\.cxx$|"
	       "[_a-zA-Z0-9\\-\\.]*\\.c$|"
	       "[_a-zA-Z0-9\\-\\.]*\\.h$");
  string file1 = "a-a.cc";
  string file2 = "a-a.C~";
  cout << rxfile.match(file1) << endl;
  cout << rxfile.search(file1) << ' ' << rxfile.search(file2) << endl;
}
