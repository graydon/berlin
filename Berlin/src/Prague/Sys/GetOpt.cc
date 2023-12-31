/*$Id: GetOpt.cc,v 1.3 1999/09/30 17:23:34 gray Exp $
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

#include "Prague/Sys/GetOpt.hh"
#include "Prague/Sys/Memory.hh"

using namespace Prague;

GetOpt::GetOpt(const char *program, const char *use) : p(program), u(use) {}
GetOpt::~GetOpt() {}

/* @Method{int GetOpt::add(char o, const string &option, type t, const string &description)}
 *
 * @Description{adds option specifications to the options data base.
 @table @var
 @item o
 is the option character
 @item option
 is the option string (for long option)
 @item t
 is the type of the option, one of @code{novalue}, @code{mandatory}, @code{optional}
 @item description
 is a small, descriptive text for that option
 @item default
 is the default value, which is returned if the option is not specified
 @end table
 */
void GetOpt::add(char o, const string &opt, type t, const string &desc)
{
   table.push_back(cell(o, opt, t, desc));
}

/* @Method{void GetOpt::get(char o, string *v) const}
 *
 * @Description{returns the value found for option @var{-o}}
 */
void GetOpt::get(char o, string *v) const
{
  table_t::const_iterator i = find(o);
  if (i == table.end()) cerr << "GetOpt::get unknown option -" << o << endl;
  else if ((*i).value.length()) *v = (*i).value;
}

/* @Method{void GetOpt::get(const string &option, string *v) const}
 *
 * @Description{returns the value found for @var{option}}
 */
void GetOpt::get(const string &option, string *v) const
{
  table_t::const_iterator i = find(option);
  if (i == table.end()) cerr << "GetOpt::get unknown option --" << option << endl;
  else if ((*i).value.length()) *v = (*i).value;
}

/* @Method{int GetOpt::parse(int argc, const char **argv)}
 *
 * @Description{parses @var{argc} tokens starting at @var{argv}. the return value is the index of the first non optional argument.}
 */
int GetOpt::parse(int argc, char **argv)
{
  int begin_unknown = 0, end_unknown = 0, optind = 0;
  while (optind < argc)
     {
       int consumed = 0;
       if (argv[0][0] == '-') // option
	 {
	   if (argv[0][1] == '-') // long option
	     {
	       if (argv[0][2] == '\0') // premature end
		 {
		   optind++;
		   return optind;
		 }
	       else consumed = getlongopt(argc, argv);
	     }
	   else consumed = getopt(argc, argv);
	 }
       if (!consumed) argc--, argv++, end_unknown++, optind++;
       else
	 {
	   optind += consumed;
	   if (begin_unknown != end_unknown && end_unknown != optind)
	     {
	       exchange(argv + begin_unknown, argv + end_unknown, argv + optind);
	       begin_unknown = end_unknown, end_unknown = optind;
	     }
	   argc -= consumed, argv += consumed;
	 }
     }
  return optind;
}

unsigned int GetOpt::getlongopt(int argc, char **argv)
{
  char *token = *argv + 2;
  while (*token != '\0' && *token != '=') token++;
  string name (*argv + 2, token - *argv - 2);
  table_t::iterator i = find(name);
  if (i == table.end()) return 0;
  if ((*i).t == novalue) (*i).value = "true";
  else if (*token == '=') (*i).value = token + 1;
  else if ((*i).t == mandatory) cerr << "GetOpt: option '--" << (*i).option << "' requires a value" << endl;
  return 1;
}

unsigned int GetOpt::getopt(int argc, char **argv)
{
  char *option = *argv + 1;
  table_t::iterator i;
  if (option[1] == '\0')
    {
      i = find(*option);
      if (i == table.end()) return 0;
      else if ((*i).t == novalue)
	{
	  (*i).value = "true";
	  return 1;
	}
      else if (argc > 1)
	{
	  (*i).value = argv[1];
	  return 2;
	}
      else if ((*i).t == mandatory)
	{
	  cerr << "GetOpt: option '-" << (*i).o << "' requires a value" << endl;
	  return 1;
	}
    }
//   do
//     {
//       option++;
//       struct
//       {
// 	compName(char &cc) : c(cc) {}
// 	bool operator () (Cell *c) { return c == c->o;}
// 	char c;
//       } comp(option);
//       i = find(*option);
//       if (i == table.end()) return 0;
//     }
//   while (
//   if ((*i)->t == novalue) (*i)->value = ~0; return 1;
//   else if (*token == '=') (*i)->value = token + 1;
//   else if ((*i)->t == mandatory) cerr << "GetOpt: option " << (*i)->name << " requires a value" << endl;
  return 0;
}

/* @Method{void GetOpt::usage() const}
 *
 * @Description{ prints the usage information to stdandard output}
 */
void GetOpt::usage() const
{
  cout << "Usage: " << p << " " << u << "\n";
  for (table_t::const_iterator i = table.begin(); i != table.end(); i++)
    {
      cout << '\t';
      if ((*i).o && (*i).option.length()) cout << '-' << (*i).o << ", --" << (*i).option;
      else if ((*i).o) cout << '-' << (*i).o << '\t';
      else if ((*i).option.length()) cout << ", --" << (*i).option;
      if ((*i).t == mandatory) cout << " <$val>";
      else if ((*i).t == optional) cout << " [$val]";
      cout << " (" << (*i).description << ")\n";
    }
  cout.flush();
}

void GetOpt::exchange (char **a, char **b, char **c)
{
  char **tmp = new char *[b - a];
  Memory::copy (a, tmp, (b - a)*sizeof(char **));
  Memory::copy (b, a, (c - b)*sizeof(char **));
  Memory::copy (tmp, b, (b - a)*sizeof(char **));
  delete [] tmp;
}
