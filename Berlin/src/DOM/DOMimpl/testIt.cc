//
// $id:$
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 The Berlin Consortium 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
// Author: ANOQ of the Sun
// EMail: anoq@berlin-consortium.org or anoq@vip.cybercity.dk

#include "support/dom/impl/domText_impl.hh"

int main(int argc, char *argv[])
{
    //Only execute these 3 lines, if you are running CORBA...
    CORBA::ORB_var orb = CORBA::ORB_init(argc,argv,"omniORB2");
    CORBA::BOA_var boa = orb->BOA_init(argc,argv,"omniORB2_BOA");
    boa->impl_is_ready(0,1);

    
  domText_impl *test = new domText_impl();

  wstring *wStr1 = cStringToDOMString("Hello!");
  wstring *wStr2 = cStringToDOMString("Q");
  wstring *wStr3 = cStringToDOMString("This is a very long string to play with");
  
  test->data(*wStr1);

  displayDOMString(test->data(),1);

  test->_delete(2,2);

  displayDOMString(test->data(),1);
  
  test->append(*wStr2);

  displayDOMString(test->data(),1);

  test->append(*wStr3);
  
  displayDOMString(test->data(),1);

  for(int a = 0; a < 20;a++)
  {
      test->append(*wStr2);
      displayDOMString(test->data(),1);
  }

  for(int a = 0; a < 20;a++)
      test->append(*wStr2);
  
  displayDOMString(test->data(),1);

  test->insert(10, *wStr1);

  displayDOMString(test->data(),1);

  for(int a = 0; a < 20;a++)
  {
      test->insert(20, *wStr2);
      displayDOMString(test->data(),1);
  }

  for(int a = 0; a < 20;a++)
      test->insert(50, *wStr2);
  
  displayDOMString(test->data(),1);

  test->replace(15, 80, *wStr3);
  
  displayDOMString(test->data(),1);
  
  domStringFree(wStr1);
  domStringFree(wStr2);
  domStringFree(wStr3);
  
  delete test;

  domStringStatus();
  _iaStatus();
  
  exit(1);
}