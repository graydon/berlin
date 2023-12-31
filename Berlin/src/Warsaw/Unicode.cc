/*$Id: Unicode.cc,v 1.5 2001/04/09 16:05:48 tobias Exp $
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

// This headerfile is necessary since Prague does not know about
// the Unistring (defined in Warsaw) that CORBA needs.

#include <Warsaw/Unicode.hh>

using namespace Warsaw;

// This is ugly but necessary since Unistring does not
// know iterators! So I can´t do decent assigment
// operators :-(

Unistring Unicode::to_CORBA(const Babylon::String &s) {
  Babylon::UTF16_string tmp = s.utf16();
  Warsaw::Unistring res(tmp.length(),
			tmp.length(),
			const_cast<Babylon::UCS2 *>(tmp.data()));
  return res;
}

Warsaw::Unichar Unicode::to_CORBA(const Babylon::Char c) {
    return Warsaw::Unichar(c.value());
}

Babylon::String Unicode::to_internal(const Unistring & us) {
    Babylon::String res;
    res.utf16(Babylon::UTF16_string(static_cast<const Babylon::UCS2 *>(us.get_buffer()),
				    static_cast<size_t>(us.length())));
    return res;
}

Babylon::Char Unicode::to_internal(const Warsaw::Unichar uc) {
    return Babylon::Char(Babylon::UCS4(uc));
}
