/*$Id: Unistring.hh,v 1.8 1999/11/30 20:34:56 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Tobias Hunger <Tobias_Hunger@gmx.de>
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
#ifndef _Unistring_hh
#define _Unistring_hh

#include <Prague/Unicode/Unicode.hh>

namespace Unicode {

class String : public basic_string<Unicode::Char> {
public:
  // CONSTRUCTORS:
  String();
  String(const Unicode::Char &);
  String(const Unicode::_Char &);
  String(const string &);
  String(const Unicode::String&);
  String(const Unicode::_String&);
  String(unsigned long len, Unicode::Char * data ) {
    this->assign(data, len);
  }

  // OPERATORS:

  // DESTRUCTOR:
  // ~String() {} // Noting special needed...
protected:
private:
}; // class String

} // namespace Unicode

#endif // _Unistring_hh
