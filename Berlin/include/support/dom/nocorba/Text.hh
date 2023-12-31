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

// Note: This is just a stripped down version of omniORB's
//       .hh file generated by omniidl2

#ifndef __Text_hh__
#define __Text_hh__

#include "support/dom/nocorba/CORBA.h"

#include "support/dom/nocorba/Node.hh"
#include "support/dom/nocorba/domDefs.hh"
#include "support/dom/nocorba/NoSuchAttributeException.hh"
#include "support/dom/nocorba/NotMyChildException.hh"
#include "support/dom/nocorba/NodeIterator.hh"
#ifndef __Text__
#define __Text__
class   Text;
typedef Text* Text_ptr;
typedef Text_ptr TextRef;
#endif

class Text :  public virtual Node
{
public:

  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual void append ( const wstring & newData ) = 0;
  virtual void insert ( CORBA::Long  offset, const wstring & newData ) = 0;
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ) = 0;
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  //static Text_ptr _duplicate(Text_ptr);
  static Text_ptr _narrow(CORBA::Object_ptr)
    { return (Text_ptr)0; }
  //static Text_ptr _nil();

protected:

  Text()
  {}
  virtual ~Text() {}
};

class _sk_Text :  public virtual _sk_Node, public virtual Text
{};

#endif // __Text_hh__
