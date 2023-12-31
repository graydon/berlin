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

#ifndef __DOMMISC__
#define __DOMMISC__

typedef unsigned short     wchar;
typedef wchar *            wstring;

//3 CORBA functions...
wstring wstring_alloc(unsigned long len);
wstring wstring_dup(wstring inStr);
void wstring_free(wstring wStr);

void wstring_status(void);

wstring cStringToWString(char *cStr);
void displayWString(wstring wStr);
unsigned long wstringLength(wstring input);
void wstringCopy(wstring dest, wstring src);

#define ASS(exp) if(!(exp)) printf("Assert error: %s!\n",#exp);
#define STABLE_MEMCOPY 1 //FROM GNUSTEP: How do we determine this?

#endif //__DOMMISC__