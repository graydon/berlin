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

#ifndef __DOMTEXT_IMPL__
#define __DOMTEXT_IMPL__

#include <Text.hh>
#include "support/dom/impl/domNode_impl.hh"
#include "support/dom/impl/domString.hh"

void _iaStatus(void);

class domText_impl : virtual public _sk_Text, domNode_impl
{
public:
  domText_impl();  
  virtual ~domText_impl();

  virtual NodeType getNodeType(void);
  virtual wstring * data (void);
  virtual void data (const wstring & _value);
  virtual void append ( const wstring & newData );
  virtual void insert ( CORBA::Long  offset, const wstring & newData );
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count );
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData );
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count );
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count );

  virtual long length(void);//NOTE: Not defined in DOM IDL yet
private:
  wchar * _textStorage;
  CORBA::ULong _textCapacity;
  CORBA::ULong _gapStart, _gapSize;
  // NOTE FYI: textLength = _textCapacity - _gapSize - 1;
};

#endif //__DOMTEXT_IMPL__