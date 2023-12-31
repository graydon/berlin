 /*$Id: get-nameservice-ior.cc,v 1.1 2000/02/23 08:34:05 njs Exp $
 *                                                     
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Nathaniel Smith <njs@uclink4.berkeley.edu>
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

#include <Warsaw/config.hh>
#include <Warsaw/resolve.hh>
#include <omniORB2/CORBA.h>

#include <iostream>

int main(int argc, char **argv) {
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CosNaming::NamingContext_var context = resolve_init<CosNaming::NamingContext>(orb, "NameService");
  cout << orb->object_to_string(context) << endl;
}
/*
To compile:
g++ -pthread -Wall -Dprofile -O3 -g -ggdb -DGDB -I../include -I../include/Warsaw -D__x86__ -D__linux__ -D__OSVERSION__=2   -ggdb -DGDB -L../lib -g -DGDB  get-nameservice-ior.cc   -o get-nameservice-ior -lomniLC -ltcpwrapGK -lomniORB2
works
*/
