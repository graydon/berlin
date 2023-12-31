/*$Id: Unicode.hh,v 1.3 2001/01/09 21:35:08 tobias Exp $
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

#ifndef _Berlin_Unicode_hh
#define _Berlin_Unicode_hh

#include <Babylon/Babylon.hh>
#include <Warsaw/Types.hh>

namespace Unicode {
    
    // This is ugly but necessary since Unistring does not
    // know iterators! So I can't do decent assigment
    // operators :-(
    Warsaw::Unistring to_CORBA(const Babylon::String &s);
    Warsaw::Unichar   to_CORBA(const Babylon::Char c);

    Babylon::String   to_internal(const Warsaw::Unistring & us);
    Babylon::Char     to_internal(const Warsaw::Unichar uc);

}; // namespace Unicode

#endif // _Berlin_Unicode_hh
