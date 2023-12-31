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

#include "support/dom/impl/domAttributeList_impl.hh"
#include <NoSuchAttributeException.hh>
#include "support/dom/impl/domString.hh"
#include <malloc.h>

#define _ATTRLIST_EXPAND 32

domAttributeList_impl::~domAttributeList_impl()
{
    if(_attrArray)
        free(_attrArray);

    //FIXME: Free attributes? Wait for next DOM spec...
}

Attrib_ptr domAttributeList_impl::getAttribute(const wstring & attrName)
{
    bool same;
    
    if(!_attrArray)
        throw NoSuchAttributeException();

    for(CORBA::ULong cnt = 0 ; cnt < _arrLength ; cnt++)
    {
        wstring *name = _attrArray[cnt]->getName();

        same = domStringEqual(name, &attrName);

        domStringFree(name);

        if(same)
            return _attrArray[cnt];
    }
    
    throw NoSuchAttributeException();
};

Attrib_ptr domAttributeList_impl::setAttribute(Attrib_ptr attr)
{
    //Check expand / alloc array
    if(_arrLength >= _arrSize)
    {
        CORBA::ULong newSize = _arrSize + _ATTRLIST_EXPAND;
        Attrib_ptr *newArr = (Attrib_ptr *)malloc(newSize * sizeof(Attrib_ptr));
        if(!newArr)
            throw CORBA::NO_MEMORY();
        
        if(_attrArray)
        {
            //Copy data from old array to new array
            for(CORBA::ULong cnt = 0; cnt < _arrSize; cnt++)
                newArr[cnt] = _attrArray[cnt];
            
            free(_attrArray);
        }
        _attrArray = newArr;
        _arrSize = newSize;
    }

    //Replace or add attribute
    bool same = 0;
    Attrib_ptr oldAttr = (Attrib_ptr)NULL;
    wstring *attrName = attr->getName();
    
    for(CORBA::ULong cnt = 0 ; cnt < _arrLength && !same ; cnt++)
    {
        wstring *name = _attrArray[cnt]->getName();

        same = domStringEqual(name, attrName);

        domStringFree(name);

        if(same)
        {
            oldAttr = _attrArray[cnt];
            _attrArray[cnt] = attr;
        }
    }
    delete attrName;

    if(!same)
    {
        //Add attribute
        _attrArray[_arrLength] = attr;
        _arrLength++;
    }
    
    return oldAttr;
};

Attrib_ptr domAttributeList_impl::remove(const wstring & attrName)
{
    bool same;
    
    if( ! &attrName)
        throw NoSuchAttributeException();

    for(CORBA::ULong cnt = 0 ; cnt < _arrLength ; cnt++)
    {
        wstring *name = _attrArray[cnt]->getName();

        same = domStringEqual(name, &attrName);

        domStringFree(name);

        if(same)
        {
            Attrib_ptr oldAttr = _attrArray[cnt];

            //Concat array elements
            for(CORBA::ULong cnt2 = cnt + 1; cnt2 < _arrLength ; cnt2++)
                _attrArray[cnt2 - 1] = _attrArray[cnt2];

            _arrLength--;
            
            return oldAttr;
        }
    }
    
    throw NoSuchAttributeException();
};

Attrib_ptr domAttributeList_impl::item(CORBA::ULong index)
{
    if(index >= _arrLength)
        throw NoSuchAttributeException();

    return _attrArray[index];
};

CORBA::ULong domAttributeList_impl::getLength( void)
{
    return _arrLength;
};

