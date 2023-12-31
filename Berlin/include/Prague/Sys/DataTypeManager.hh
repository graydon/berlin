/*$Id: DataTypeManager.hh,v 1.5 2001/03/21 06:28:22 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Prague_DataTypeManager_hh
#define _Prague_DataTypeManager_hh

#include <vector>
#include <string>
#include <Prague/Sys/regex.hh>

namespace Prague
{

//. a DataTypeManager maintains a data type repository to determine
//. the type of a given file, based on either a magic number or the file's extension
class DataTypeManager
{
  struct Type
  {
    struct Name
    {
      std::string::const_iterator parse(std::string::const_iterator, std::string::const_iterator);
      unsigned short match(const std::string &);
      unsigned short score;
      regex name;
    };
    struct Magic
    {
      struct Part
      {
	std::string::const_iterator parse(std::string::const_iterator, std::string::const_iterator);
	bool match(const unsigned char *, int);
	unsigned short offset, length;
 	std::vector<unsigned char> data, mask;
      };
      std::string::const_iterator parse(std::string::const_iterator, std::string::const_iterator);
      unsigned short match(const unsigned char *, int);
      unsigned short score;
      std::vector<Part> parts;
    };
    bool parse(const std::string &);
    unsigned short match_name(const std::string &);
    unsigned short match_magic(const unsigned char *, int);
    std::string type;
    std::string mime;
    std::vector<Name> names;
    std::vector<Magic> magics;
  };
public:
  DataTypeManager(const std::string &file) { merge(file);}
  ~DataTypeManager() {}
  //. merges in another type repository
  void merge(const std::string &);
  //. return the type, given the head of a file
  std::string match(const std::string &);
  //. return the type for a given file
  std::string match(const std::string &, const unsigned char *, unsigned int);
  std::string type_to_mime(const std::string &);
  std::string mime_to_type(const std::string &);
private:
  static short compare(unsigned short, unsigned short, unsigned short, unsigned short);
  std::vector<Type> _types;
};

};

#endif
