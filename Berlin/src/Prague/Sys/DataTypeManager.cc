/*$Id: DataTypeManager.cc,v 1.5 1999/10/15 17:59:25 gray Exp $
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
#include "Prague/Sys/DataTypeManager.hh"
#include "Prague/Sys/File.hh"
#include "Prague/Sys/Memory.hh"
#include <fstream>
#include <strstream>
#include <algorithm>
#include <cstdio>

using namespace Prague;

void stripcomment(string &line)
{
  bool quote = false;
  for (string::iterator i = line.begin(); i != line.end(); i++)
    {
      if (*i == '\'') quote = !quote;
      else if (!quote && *i == '#')
	{
	  line.erase(i, line.end());
	  return;
	}
    }
}

vector<unsigned char> getbytes(istream &is, unsigned short n)
{
  vector<unsigned char> bytes;
  unsigned short i = 0;
  do
    {
      string token;
      while (isspace(is.peek())) is.ignore();
      /*
       * if it's a string, read all what is between the quote marks
       */
      if (is.peek() == '\'')
	{
	  is.ignore();
	  getline(is, token, '\'');
	  bytes.insert(bytes.end(), token.begin(), token.end());
	  i += token.length();
	}
      /*
       * else if it's a byte, read all till the next white space
       */
      else
	{
	  int value;
	  is >> token;
	  sscanf(token.c_str(), "%i", &value);
	  bytes.push_back(value);
	  i++;
	}
    }
  while (i < n);
  return bytes.size() == n ? bytes : vector<unsigned char>();
}

string::const_iterator DataTypeManager::Type::Name::parse(string::const_iterator begin, string::const_iterator end)
{
  istrstream iss(begin, end - begin);
  iss >> score;
  iss.ignore(end - begin, '\'');
  string tmp;
  getline(iss, tmp, '\'');
  name = tmp;
  return iss && tmp.length() ? end : begin;
}

string::const_iterator DataTypeManager::Type::Magic::Part::parse(string::const_iterator begin, string::const_iterator end)
{
  istrstream iss(begin, end - begin);
  iss >> offset;
  iss.ignore(end - begin, '[');
  iss >> length;
  iss.ignore(end - begin, ']');
  if (!iss || length > 4096) return begin;
  data.resize(length);
  mask.resize(length, 0xff);
  string token;
  iss >> token;
  if (token == "&")
    {
      mask = getbytes(iss, length);
      iss >> token;
    }
  bool approx = false;
  if (token == "~=") approx = true;
  if (!approx && token != "==") return begin;
  data = getbytes(iss, length);
  return iss  ? end : begin;
}

string::const_iterator DataTypeManager::Type::Magic::parse(string::const_iterator begin, string::const_iterator end)
{
  string::const_iterator i = begin;
  while (isspace(*i)) i++;
  istrstream iss(i, end - i);
  iss >> score;
  while (!isspace(*i)) i++;
  while (1)
    {
      Part part;
      string::const_iterator j = part.parse(i, end);
      if (i == j) return begin;
      begin = i = j;
      parts.push_back(part);
    }
  return begin;
}

bool DataTypeManager::Type::parse(const string &line)
{
  if (line.substr(0, 5) == "type:")
    {
      istrstream iss(line.begin() + 5, line.length() - 5);
      iss >> type;
    }
  else if (line.substr(0, 5) == "mime:")
    {
      istrstream iss(line.begin() + 5, line.length() - 5);
      iss >> mime;
    }
  else if (line.substr(0, 5) == "name:")
    {
      Name name;
      if (name.parse(line.begin() + 5, line.end()) != line.begin() + 5)
	names.push_back(name);
      else return false;
    }
  else if (line.substr(0, 6) == "magic:")
    {
      Magic magic;
      if (magic.parse(line.begin() + 6, line.end()) != line.begin() + 6)
	magics.push_back(magic);
      else return false;
    }
  else return false;
  return true;
}

void DataTypeManager::merge(const string &file)
{
  ifstream ifs(file.c_str());
  unsigned int lineno = 0;
  Type *type = 0;
  while (ifs && ++lineno)
    {
      string line;
      getline(ifs, line);
      stripcomment(line);
      if (line.empty()) continue;
      if (line.substr(0, 5) == "type:")
	{
	  if (type) { types.push_back(*type); delete type;}
	  type = new Type;
	}
      if (!type || !type->parse(line))
	cerr << "DataTypeManager::merge: error in line " << lineno << " of file " << file << endl;
    }
  if (type) { types.push_back(*type); delete type;}
}

short DataTypeManager::compare(unsigned short name1, unsigned short magic1, unsigned short name2, unsigned short magic2)
{
  /*
   * Returns: 
   * 	  1 if type1 > type2,
   *     -1 if type1 < type2,
   *	  0 if type1 = type2.   (conflict)
   */
  if ((magic1 > magic2 && name1 >= name2) || (name1 > name2 && magic1 >= magic2)) return 1;
  if ((magic2 > magic1 && name2 >= name1) || (name2 > name1 && magic2 >= magic1)) return -1;
  
  if (max(name1, magic1) > max(name2, magic2)) return  1;
  if (max(name2, magic2) > max(name1, magic1)) return -1;
  
  if (min(name1, magic1) > min(name2, magic2)) return  1;
  if (min(name2, magic2) > min(name1, magic1)) return -1;
  
  if (magic1 > magic2) return  1;
  if (magic2 > magic1) return -1;
  
  return 0;
}

unsigned short DataTypeManager::Type::Name::match(const string &file)
{
  return name.match(file) > 0 ? score : 0;
}

unsigned short DataTypeManager::Type::matchName(const string &file)
{
  unsigned short best = 0;
  for (vector<Name>::iterator i = names.begin(); i != names.end(); i++)
    {
      best = max(best, (*i).match(file));
    }
  return best;
}

bool DataTypeManager::Type::Magic::Part::match(const unsigned char *d, int l)
{
  if (offset + length > l) return false;
  for (unsigned short i = 0, o = offset; i != length; i++, o++)
    if ((d[o] & mask[i]) != data[i]) return false;
  return true;
}

unsigned short DataTypeManager::Type::Magic::match(const unsigned char *data, int length)
{
  for (vector<Part>::iterator i = parts.begin(); i != parts.end(); i++)
    if (!(*i).match(data, length)) return 0;
  return score;
}

unsigned short DataTypeManager::Type::matchMagic(const unsigned char *data, int length)
{
  unsigned short best = 0;
  for (vector<Magic>::iterator i = magics.begin(); i != magics.end(); i++)
    best = max(best, (*i).match(data, length));
  return best;
}

string DataTypeManager::match(const string &file, const unsigned char *data, unsigned int length)
{
  vector<Type>::iterator best = types.end();
  unsigned short bestName = 0;
  unsigned short bestMagic = 0;
  for (vector<Type>::iterator i = types.begin(); i != types.end(); i++)
    {
      unsigned short name  = 0;
      unsigned short magic = 0;
      if (file.length()) name = (*i).matchName(file);
      if (data && length) magic = (*i).matchMagic(data, length);
      if (name == 0 && magic == 0) continue;
      if (best == types.end())
	{
	  best = i;
	  bestName  = name;
	  bestMagic = magic;
	  continue;
	}
      if (compare(bestName, bestMagic, name, magic) < 0)
	{
	  best = i;
	  bestName  = name;
	  bestMagic = magic;
	  continue;
	}
    }
  if (best == types.end()) return "binary";
  else return (*best).type;
}

string DataTypeManager::match(const string &file)
{
  ifstream ifs(file.c_str());
  string name = File::base(file);
  unsigned char data[4096];
  ifs.read(data, 4096);
  return match(name, data, ifs.gcount());
}

string DataTypeManager::TypeToMime(const string &type)
{
  for (vector<Type>::iterator i = types.begin(); i != types.end(); i++)
    if ((*i).type == type) return (*i).mime;
  return string();
}

string DataTypeManager::MimeToType(const string &mime)
{
  for (vector<Type>::iterator i = types.begin(); i != types.end(); i++)
    if ((*i).mime == mime) return (*i).type;
  return string();
}
