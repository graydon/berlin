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

#ifndef __DOMSTRING__
#define __DOMSTRING__

#include <domDefs.hh>

//3 CORBA functions...
wstring *domStringAlloc(CORBA::ULong len);
wstring *domStringDup(const wstring *inStr);
void domStringFree(const wstring *wStr);

void domStringStatus(void);

wstring *cStringToDOMString(char *cStr);
void displayDOMString(const wstring * wStr,CORBA::Boolean freeIt);
CORBA::ULong domStringLength(const wstring *input);
CORBA::Boolean domStringEqual(const wstring *str1, const wstring *str2);
void domSringCopy(const wstring *dest, const wstring *src);

#define ASS(exp) if(!(exp)) printf("Assert error: %s!\n",#exp);
#define STABLE_MEMCOPY 1 //FROM GNUSTEP: How do we determine this?

#endif //__DOMSTRING__