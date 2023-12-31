/*$Id: TextConverter.cc,v 1.3 2001/04/26 01:29:39 stefan Exp $
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

#include "TextConverter.hh"

TextConverter::TextConverter(const std::string & file) {
    tree_map = new Prague::MMap(file,
				-1,
				Prague::MMap::read,
				Prague::MMap::shared,
				0,
				0);
    tree = (node *)tree_map->addr();
    tree_size = tree_map->size() / sizeof(node);
}

Babylon::String
TextConverter::convert(const Babylon::String & pinyin) const {
    Babylon::String result;

    if (pinyin.empty()) return result;

    size_t cur_start = 0;
    size_t cur_end = 0;

    Babylon::String::const_iterator i = pinyin.begin();
    while (i != pinyin.end()) {
	if (i->value() > 127) return result;
	
	if (cur_start != 0) cur_start = tree[cur_start].Next;

	cur_start = find_char(char(i->value()), cur_start, tree[cur_start].Next);
	if (cur_start == 0xFFFF) return result;

	++i;
    }

    cur_end = tree[cur_start + 1].Next;
    cur_start = tree[cur_start].Next;

    for (size_t i = cur_start; i <= cur_end; i++)
	if (tree[i].Unicode != 0)
	  result += tree[i].Unicode;
    return result;
}

size_t
TextConverter::find_char(const char p,
		  const size_t s,
		  const size_t e) const {
    size_t start = s;
    size_t end;
    if (e < tree_size) end = e - 1; // Next points to the Beginning of the next. 
                                    // Wow, what a useful comment!
    else end = tree_size;

    size_t pos = start;
    while ((start < end) && tree[pos].Char != p) {
	pos = (start + end) / 2;
	if (p < tree[pos].Char)
	    end = pos - 1;
	else if (p > tree[pos].Char)
	    start = pos + 1;
    }

    if (p == tree[pos].Char)
	return pos;
    
    return size_t(0xFFFF);
}
