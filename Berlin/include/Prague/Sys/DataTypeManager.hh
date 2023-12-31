/*$Id: DataTypeManager.hh,v 1.2 1999/05/04 21:00:45 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _DataTypeManager_hh
#define _DataTypeManager_hh

#include <vector>
#include <string>
#include <Prague/Sys/regex.hh>

namespace Prague
{

class DataTypeManager
{
  struct Type
  {
    struct Name
    {
      string::const_iterator parse(string::const_iterator, string::const_iterator);
      unsigned short match(const string &);
      unsigned short score;
      regex name;
    };
    struct Magic
    {
      struct Part
      {
	string::const_iterator parse(string::const_iterator, string::const_iterator);
	bool match(const unsigned char *, int);
	unsigned short offset, length;
 	vector<unsigned char> data, mask;
      };
      string::const_iterator parse(string::const_iterator, string::const_iterator);
      unsigned short match(const unsigned char *, int);
      unsigned short score;
      vector<Part> parts;
    };
    bool parse(const string &);
    unsigned short matchName(const string &);
    unsigned short matchMagic(const unsigned char *, int);
    string type;
    string mime;
    vector<Name> names;
    vector<Magic> magics;
  };
public:
  DataTypeManager(const string &file) { merge(file);}
  ~DataTypeManager() {}
  void merge(const string &);
  string match(const string &);
  string match(const string &, const unsigned char *, unsigned int);
  string TypeToMime(const string &);
  string MimeToType(const string &);
private:
  static short compare(unsigned short, unsigned short, unsigned short, unsigned short);
  vector<Type> types;
};

};

#endif /* _DataTypeManager_hh */
