/*$Id: Unistring.cc,v 1.9 1999/12/08 02:13:32 tobias Exp $
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

#include <Prague/Unicode/Unistring.hh>

using namespace Unicode;

// CONSTRUCTORS:
String::String() {
  resize(0);
}

String::String(const Char & uc) {
  resize(1);
  (*this)[0] = uc;
}

String::String(const _Char & _uc) {
  resize(1);
  (*this)[0] = _uc;
}

String::String(const string & s) {
  resize(s.length());
  
  string::const_iterator    s_it = s.begin();
  String::iterator       this_it = this->begin();
  
  do {
    *this_it = *s_it;
    this_it++; s_it++;
  } while (s_it != s.end() && this_it != this->end());
}

String::String(const String & us) {
  resize(us.length());
  
  String::const_iterator   us_it = us.begin();
  String::iterator       this_it = this->begin();
  
  do {
    *this_it = *us_it;
    this_it++; us_it++;
  } while (us_it != us.end() && this_it != this->end());
}

String::String(const _String & _us) {
  resize(_us.length());
  
  _String::const_iterator  _us_it = _us.begin();
  String::iterator       this_it = this->begin();
  
  do {
    *this_it = *_us_it;
    this_it++; _us_it++;
  } while (_us_it != _us.end() && this_it != this->end());
}
