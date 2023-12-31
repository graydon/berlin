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

#include "support/dom/impl/domString.hh"         
#include <stdio.h>
#include <malloc.h>

unsigned long DOMstring_alloced = 0;

wstring *domStringAlloc(CORBA::ULong len)
{
    wstring *newStr = new wstring(len + 1);
    if(!newStr)
        throw CORBA::NO_MEMORY();
    newStr->length(len + 1);
        
    DOMstring_alloced++;
    return newStr;
}

wstring *domStringDup(const wstring *inStr)
{
    ASS(inStr);
    unsigned long len =  domStringLength(inStr);

    wstring *newStr = domStringAlloc(len);
    if(!newStr)
        return (wstring *)0;
    memcpy(newStr->NP_data(), inStr->NP_data(), (len + 1) * sizeof(wchar));
    
    return newStr;
}

void domStringFree(const wstring *wStr)
{
    if(wStr)
    {
        DOMstring_alloced --;
        delete wStr;
    }
}

void domStringStatus(void)
{
    printf("Your total memory leaks (unfreed domStrings) are: %ld\n",DOMstring_alloced);
}

wstring *cStringToDOMString(char *cStr)
{
    ASS(cStr);
    long len = strlen(cStr);
    wstring *wStr = domStringAlloc(len);
    
    wchar *tmpWStr = wStr->NP_data();
    while( (*tmpWStr++ = (wchar)*cStr++) );
    return wStr;
}

void displayDOMString(const wstring * wStr, CORBA::Boolean freeIt)
{
    ASS(wStr);

    wchar *wPtr = wStr->NP_data();
    
    while(*wPtr)
        printf("%c",(char)*wPtr++);
    puts("");

    if(freeIt)
        domStringFree(wStr);
}

CORBA::ULong domStringLength(const wstring *input)
{
    //ASS(input);
    //unsigned long cnt = 0;
    //while(*input++)
    //    cnt ++;
    //return cnt;
    return input->length() - 1;//0-termination...
}

CORBA::Boolean domStringEqual(const wstring *str1, const wstring *str2)
{
    ASS(str1);
    ASS(str2);

    CORBA::ULong len = str1->length();
    
    if(len != str2->length())
        return 0;

    wchar *ptr1 = str1->NP_data();
    wchar *ptr2 = str2->NP_data();

    for(CORBA::ULong cnt = 0;cnt < len;cnt++)
    {
        if(*ptr1++ != *ptr2++)
            return 0;
    }
    return 1;
}


void domStringCopy(const wstring *dest, const wstring *src)
{
  ASS(dest);
  ASS(src);

  wchar *dPtr = dest->NP_data();
  wchar *sPtr = src->NP_data();
  
  while( (*dPtr++ = *sPtr++) );
}
