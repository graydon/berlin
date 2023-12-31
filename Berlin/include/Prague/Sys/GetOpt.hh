/*$Id: GetOpt.hh,v 1.7 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _Prague_GetOpt_hh
#define _Prague_GetOpt_hh

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

namespace Prague
{

//. GetOpt manages an command line option data base. It deals with short and long options,
//. and reorders the given option vector such that you can run different GetOpt instances over
//. the argv vector one after another.
class GetOpt
{
public:
  //. the option type, i.e. whether an argument is required or not.
  enum type { novalue, optional, mandatory};
  enum order { inorder, require, permute};
private:
  struct cell
  {
    cell(char oo, const std::string &opt, type tt, const std::string &des)
      : o(oo), option(opt), t(tt), description(des) {}
    char        o;            // option char
    std::string option;       // option name
    type        t;	      // option type
    std::string description;  // a description of option
    std::string value;	      // value of option (string)    
  };
  typedef std::vector<cell> table_t;	      // option table
public:
  GetOpt(const char *, const char * = 0);
  ~GetOpt();
  //. print out usage information. This function uses help data provided with each
  //. registered option, i.e. it is suitable to be used as a help message.
  void usage() const;
  //. parse the given argv
  int parse(int argc, char **argv);
  //. parse the given args
  int parse(char *args);
  //. register an option
  void add(char o, const std::string &option, type, const std::string & = "no description available");
  //. get a value
  void get(char o, std::string *) const;
  //. get a value
  void get(const std::string &option, std::string *) const;
private:
  table_t table;
  const char *p;	      // program basename
  const char *u;	      // usage message
  order ordering;
  size_t getlongopt(int, char **);
  size_t getopt(int, char **);
  table_t::iterator find(const std::string &option)
    { return find_if(table.begin(), table.end(), comp_string(option));}
  table_t::const_iterator find(const std::string &option) const
    { return find_if(table.begin(), table.end(), comp_string(option));}
  table_t::iterator find(char o)
    { return find_if(table.begin(), table.end(), comp_char(o));}
  table_t::const_iterator find(char o) const
    { return find_if(table.begin(), table.end(), comp_char(o));}
  void exchange(char **, size_t, char **);
  struct comp_string
  {
    comp_string(const std::string &n) : name(n) {}
    bool operator () (const cell &c) { return name == c.option;}
    std::string name;
  };
  struct comp_char
  {
    comp_char(char n) : name(n) {}
    bool operator () (const cell &c) { return name == c.o;}
    char name;
  };
};

};

#endif
