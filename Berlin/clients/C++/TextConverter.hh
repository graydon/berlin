/*$Id: TextConverter.hh,v 1.1 2001/04/10 16:17:17 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Tobias Hunger <Tobias@berlin-consortium.org>
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

#ifndef _Text_Converter_hh
#define _Text_Converter_hh

#include <string>
#include <vector>
#include <Prague/Sys/MMap.hh>
#include <Babylon/Babylon.hh>

class TextConverter {
public:
    TextConverter(const std::string &);
    ~TextConverter() { delete tree_map; }

    Babylon::String
    convert(const Babylon::String &) const;

private:
    size_t
    find_char(const char p, const size_t begin, const size_t end) const;

    struct node {
	char Char;
	unsigned short Next;
	Babylon::UCS4 Unicode;
    };

    Prague::MMap * tree_map;
    node * tree;
    size_t tree_size;
};

#endif // _TextConverter_hh
