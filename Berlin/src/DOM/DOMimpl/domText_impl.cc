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

#include <stdio.h>
#include "support/dom/impl/domText_impl.hh"
#include <malloc.h>

#define _DOMTEXT_EXPAND 4

unsigned long iaAlloced = 0;

wchar *_iaAlloc(CORBA::ULong len);
void _iaFree(wchar *wStr);
CORBA::ULong _iaLength(wchar *input);
wchar *_iaDup(wchar *inStr);
void _iaCopy(wchar *dest, wchar *src);
void _domToIACopy(wchar *dest,const wstring * src);

wchar *_iaAlloc(CORBA::ULong len)
{
    wchar *newStr = (wchar *)malloc((len + 1) * sizeof(wchar));
    if(newStr)
          iaAlloced++;
    return newStr;
}

void _iaFree(wchar *wStr)
{
    if(wStr)
    {
        iaAlloced --;
        free(wStr);
    }
}

CORBA::ULong _iaLength(wchar *input)
{
    ASS(input);
    CORBA::ULong cnt = 0;
    while(*input++)
        cnt ++;
    return cnt;
}                                                                        

wchar *_iaDup(wchar *inStr)
{
    ASS(inStr);
    CORBA::ULong len =  _iaLength(inStr);

    wchar *newStr = _iaAlloc(len);
    if(!newStr)
        return (wchar *)0;
    memcpy(newStr, inStr, (len + 1) * sizeof(wchar));
    
    return newStr;
}

void _iaCopy(wchar *dest, wchar *src)
{
  ASS(dest);
  ASS(src);

  while( (*dest++ = *src++) );
}

void _domToIACopy(wchar *dest,const wstring * src)
{
  ASS(dest);
  ASS(src);

  CORBA::ULong sLen = domStringLength(src);
  wchar * srcPtr = src->NP_data();
  
  for(CORBA::ULong cnt = 0; cnt < sLen; cnt++)
      dest[cnt] = srcPtr[cnt];

  dest[sLen] = (wchar)'\0';//FIXME: Unichar terminate
}

void _iaStatus(void)
{
    printf("Your total memory leaks (unfreed internal strings in domText_impl) are: %ld\n",iaAlloced);
}
                                     
#define MALLOC_EXPAND(minCap, methodStr)                                 \
({CORBA::ULong newCapacity = minCap + _DOMTEXT_EXPAND;                           \
  wchar * newTextStorage = _iaAlloc(newCapacity - 1); \
  if(!newTextStorage)                                                    \
    printf("Not enough memory for document data! \
    This error occurred while trying to \
    execute an internal function called \
    domText_impl::%s\n",methodStr);                                      \
  /* raise ...FIXME! */                                                  \
  this->_textCapacity = newCapacity;                                     \
  newTextStorage;})

#define FREE_EXPAND(newTextStorage) \
    ({_iaFree(this->_textStorage);         \
  this->_textStorage = newTextStorage;})

//NOTE: The next 3 #defines are taken from GNUStep and modified

#define GAP_TO_BASIC(INDEX)              \
  ({ CORBA::ULong __idx = (INDEX);       \
       __idx >= this->_gapStart          \
         ? __idx+this->_gapSize : __idx; })

#define BASIC_TO_GAP(INDEX)              \
  ({ CORBA::ULong __idx = (INDEX);       \
       __idx < this->_gapStart           \
         ? __idx : __idx-this->_gapSize; })

#ifndef STABLE_MEMCPY
#define MOVE_GAP_TO(INDEX)               \
({ CORBA::ULong i; CORBA::ULong __idx = (INDEX); \
   ASS (__idx <= this->_textCapacity);   \
   if (__idx < this->_gapStart)          \
   {                                     \
      CORBA::ULong b = __idx + this->_gapSize;    \
      for (i = this->_gapStart + this->_gapSize - 1; i >= b; i--)       \
        this->_textStorage[i] = this->_textStorage[i - this->_gapSize]; \
   }                                                       \
   else                                                    \
   {                                                       \
      for(i = this->_gapStart; i < __idx; i++)             \
        this->_textStorage[i] = this->_textStorage[i + this->_gapSize]; \
   }                                     \
   this->_gapStart = __idx;              \
    })
#else
#define MOVE_GAP_TO(INDEX)               \
({ CORBA::ULong i; CORBA::ULong __idx = (INDEX);         \
   ASS (__idx <= this->_textCapacity);   \
   if (__idx < this->_gapStart)          \
   {                                     \
      memcpy (this->_textStorage + __idx + this->_gapSize, \
              this->_textStorage + __idx,                  \
              (this->_gapStart - __idx) * sizeof(wchar))   \
   }                                                       \
   else                                                    \
   {                                                       \
      memcpy (this->_textStorage + this->_gapStart,                  \
              this->_textStorage + this->_gapStart + this->_gapSize, \
              (__idx - this->_gapStart) * sizeof(wchar));            \
   }                                     \
   this->_gapStart = __idx;              \
})
#endif

domText_impl::domText_impl()
{
  puts("created!");
  _textCapacity = _DOMTEXT_EXPAND;
  _gapStart = 1;
  _gapSize = _DOMTEXT_EXPAND - 1;
  _textStorage = _iaAlloc(_DOMTEXT_EXPAND - 1);
  if(!_textStorage)
      puts("Not enough memory for document data! This error occurred while trying to create an internal object of type domText_impl.");
  //  raise ...//FIXME!
  *_textStorage = (wchar)'\0';//FIXME: Unicode terminate char
}

domText_impl::~domText_impl()
{
  _iaFree(_textStorage);
  puts("destroyed!");
}

Node::NodeType domText_impl::getNodeType(void)
{
    return Node::TEXT;
}

CORBA::Long domText_impl::length(void)
{
  //NOTE: Not defined in DOM IDL yet...
  return _textCapacity - _gapSize - 1;
}

Text_ptr domText_impl::splice ( CORBA::Long  offset, CORBA::Long  count )
{
    return (Text_ptr)0;//Not defined in DOM yet, wait for next DOM spec.
}

wstring * domText_impl::range ( CORBA::Long  offset, CORBA::Long  count )
{
    return (wstring *)0;//Not defined in DOM yet, wait for next DOM spec.
}

wstring *domText_impl::data(void)
{
  CORBA::ULong newLen = _textCapacity - _gapSize;
  wstring *newStr = domStringAlloc(newLen);

  wchar *ptr = newStr->NP_data();
  
  for(CORBA::ULong cnt = 0; cnt < _gapStart; cnt++)
      ptr[cnt] = _textStorage[cnt];

  for(CORBA::ULong cnt = _gapStart + _gapSize; cnt < _textCapacity; cnt++)
      ptr[cnt - _gapSize] = _textStorage[cnt];

  ptr[newLen] = (wchar)'\0';//FIXME: Unicode terminate
  
  return newStr;
}

void domText_impl::data (const wstring & _value)
{
    CORBA::ULong len = domStringLength(&_value) + 1;
  
    if (_textCapacity < len)
        FREE_EXPAND(MALLOC_EXPAND(len, "data"));
  
    _gapStart = len;
    _gapSize = _textCapacity - _gapStart;
    _domToIACopy(_textStorage, &_value);
}

void domText_impl::append ( const wstring & newData )
{
    CORBA::ULong oldEnd = _textCapacity - _gapSize;
    MOVE_GAP_TO(oldEnd);

    CORBA::ULong len = domStringLength(&newData);

    if(_textCapacity < len + oldEnd)
    {
        wchar * newTextStorage = MALLOC_EXPAND(len + oldEnd, "append");

        _iaCopy(newTextStorage, _textStorage);

        FREE_EXPAND(newTextStorage);

        _gapSize = _DOMTEXT_EXPAND;
        _gapStart = _textCapacity - _gapSize;
    }
    else
    {
        _gapSize -= len;
        _gapStart += len;
    }
    oldEnd--;//To overwrite '\0' in wstringCopy below
    _domToIACopy(_textStorage + oldEnd, &newData);
}

void domText_impl::insert ( CORBA::Long offset, const wstring & newData )
{
    CORBA::ULong oldSize = _textCapacity - _gapSize;
    MOVE_GAP_TO(offset);

    CORBA::Long len = domStringLength(&newData);

    if(_textCapacity < len + oldSize)
    {
        wchar * newTextStorage = MALLOC_EXPAND(len + oldSize,"insert");

        memcpy(newTextStorage, _textStorage, offset * sizeof(wchar));// data before insert pos
        memcpy(newTextStorage + _textCapacity + offset - oldSize,
               _textStorage + offset + _gapSize,
               (oldSize - offset) * sizeof(wchar));// data after gap

        FREE_EXPAND(newTextStorage);

        _gapSize = _DOMTEXT_EXPAND;
        _gapStart += len;
    }
    else
    {
        _gapSize -= len;
        _gapStart += len;
    }

    wchar *ndPtr = newData.NP_data();
    memcpy(_textStorage + offset, ndPtr, len * (sizeof(wchar)));// new data
}

void domText_impl::_delete ( CORBA::Long  offset, CORBA::Long  count )
{
    if( (((CORBA::ULong)offset + count) > _textCapacity - _gapSize - 1) )
        puts("out of range in domText_impl::_delete!\n");
    //    raise ...;//FIXME!
    MOVE_GAP_TO((CORBA::ULong)offset + count);
    _gapStart -= count;
    _gapSize += count;
}

void domText_impl::replace ( CORBA::Long offset, CORBA::Long count, const wstring & newData )
{
    _delete(offset, count);
    insert(offset, newData);
}
