/*$Id: GetOpt.hh,v 1.3 1999/09/30 17:23:33 gray Exp $
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
#ifndef _GetOpt_hh
#define _GetOpt_hh

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

namespace Prague
{

/* @Class{GetOpt}
 *
 * @Description{replacement for GNU GetOpt, adds handling of long options: proposed interface:
 @itemize @bullet
 @item try complience to standard getopt functions
 @item work as a filter, enabling subsequent option extraction by different GetOpt instances
 @item use '-' for short and '--' for long option styles
 @end itemize
 }
 */
class GetOpt
{
public:
  enum type { novalue, optional, mandatory};
  enum order { inorder, require, permute};
private:
  struct cell
  {
    cell(char oo, const string &opt, type tt, const string &des)
      : o(oo), option(opt), t(tt), description(des) {}
    char        o;            // option char
    string      option;       // option name
    type        t;	      // option type
    string      description;  // a description of option
    string      value;	      // value of option (string)    
  };
  typedef vector<cell> table_t;	      // option table
public:
  GetOpt(const char *, const char * = 0);
  ~GetOpt();
  void usage() const;
  int parse(int, char **);
  int parse(char *);
  void add(char, const string &, type, const string & = "no description available");
  void get(char, string *) const;
  void get(const string &, string *) const;
private:
  table_t table;
  const char *p;	      // program basename
  const char *u;	      // usage message
  order ordering;
  unsigned int getlongopt(int, char **);
  unsigned int getopt(int, char **);
  table_t::iterator find(const string &option)
    { return find_if(table.begin(), table.end(), comp_string(option));}
  table_t::const_iterator find(const string &option) const
    { return find_if(table.begin(), table.end(), comp_string(option));}
  table_t::iterator find(char o)
    { return find_if(table.begin(), table.end(), comp_char(o));}
  table_t::const_iterator find(char o) const
    { return find_if(table.begin(), table.end(), comp_char(o));}
  void exchange(char **, char **, char **);
  struct comp_string
  {
    comp_string(const string &n) : name(n) {}
    bool operator () (const cell &c) { return name == c.option;}
    string name;
  };
  struct comp_char
  {
    comp_char(char n) : name(n) {}
    bool operator () (const cell &c) { return name == c.o;}
    char name;
  };
};

};

#endif /* _GetOpt_hh */
