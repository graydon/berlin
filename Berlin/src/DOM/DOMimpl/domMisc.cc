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

#include "support/dom/impl/domMisc.hh"         
#include <stdio.h>
#include <malloc.h>

unsigned long wstring_alloced = 0;

wstring wstring_alloc(unsigned long len)
{
    wstring newStr = (wstring)malloc((len + 1) * sizeof(wchar));
    if(newStr)
          wstring_alloced++;
    return newStr;
}

wstring wstring_dup(wstring inStr)
{
    ASS(inStr);
    unsigned long len =  wstringLength(inStr);

    wstring newStr = wstring_alloc(len);
    if(!newStr)
        return (wstring)0;
    memcpy(newStr, inStr, (len + 1) * sizeof(wchar));
    
    return newStr;
}

void wstring_free(wstring wStr)
{
    if(wStr)
    {
        wstring_alloced --;
        free(wStr);
    }
}

void wstring_status(void)
{
    printf("Your total memory leaks are: %ld\n",wstring_alloced);
}

wstring cStringToWString(char *cStr)
{
    ASS(cStr);
    long len = strlen(cStr);
    wstring wStr = wstring_alloc(len);
    wstring tmpWStr = wStr;
    while(*tmpWStr++ = (wchar)*cStr++);
    return wStr;
}

void displayWString(wstring wStr)
{
    ASS(wStr);
    while(*wStr)
        printf("%c",(char)*wStr++);
    puts("");
}

unsigned long wstringLength(wstring input)
{
    ASS(input);
    unsigned long cnt = 0;
    while(*input++)
        cnt ++;
    return cnt;
}

void wstringCopy(wstring dest, wstring src)
{
  ASS(dest);
  ASS(src);

  while(*dest++ = *src++);
}
