/*$Id: GetOpt.cc,v 1.7 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/Sys/GetOpt.hh"
#include "Prague/Sys/Memory.hh"

using namespace Prague;

GetOpt::GetOpt(const char *program, const char *use) : p(program), u(use) {}
GetOpt::~GetOpt() {}

void GetOpt::add(char o, const std::string &opt, type t, const std::string &desc)
{
   table.push_back(cell(o, opt, t, desc));
}

void GetOpt::get(char o, std::string *v) const
{
  table_t::const_iterator i = find(o);
  if (i == table.end()) std::cerr << "GetOpt::get unknown option -" << o << std::endl;
  else if ((*i).value.length()) *v = (*i).value;
}

void GetOpt::get(const std::string &option, std::string *v) const
{
  table_t::const_iterator i = find(option);
  if (i == table.end()) std::cerr << "GetOpt::get unknown option --" << option << std::endl;
  else if ((*i).value.length()) *v = (*i).value;
}

int GetOpt::parse(int argc, char **argv)
{
  int unknown = 0;
  int cursor = 0;
  while (cursor < argc)
     {
       size_t consumed = 0;
       if (argv[cursor][0] == '-') // option
	 if (argv[cursor][1] == '-') // long option
	   if (argv[cursor][2] == '\0') // premature end
	     return ++cursor;
	   else consumed = getlongopt(argc - cursor, argv + cursor);
	 else consumed = getopt(argc - cursor, argv + cursor);
       if (consumed && cursor)
	 {
	   exchange(argv + cursor, consumed, argv + unknown);
	   unknown += consumed;
	   cursor += consumed;
	 }
       else cursor += 1;
     }
  return unknown;
}

size_t GetOpt::getlongopt(int argc, char **argv)
{
  char *token = *argv + 2;
  while (*token != '\0' && *token != '=') token++;
  std::string name (*argv + 2, token - *argv - 2);
  table_t::iterator i = find(name);
  if (i == table.end()) return 0;
  if ((*i).t == novalue) (*i).value = "true";
  else if (*token == '=') (*i).value = token + 1;
  else if ((*i).t == mandatory) std::cerr << p << ": option '--" << (*i).option << "' requires a value" << std::endl;
  return 1;
}

size_t GetOpt::getopt(int argc, char **argv)
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
	  std::cerr << p << ": option '-" << (*i).o << "' requires a value" << std::endl;
	  return 1;
	}
    }
  return 0;
}

void GetOpt::usage() const
{
  std::cout << "Usage: " << p << " " << u << "\n";
  for (table_t::const_iterator i = table.begin(); i != table.end(); i++)
    {
      std::cout << '\t';
      if ((*i).o && (*i).option.length()) std::cout << '-' << (*i).o << ", --" << (*i).option;
      else if ((*i).o) std::cout << '-' << (*i).o << '\t';
      else if ((*i).option.length()) std::cout << ", --" << (*i).option;
      if ((*i).t == mandatory) std::cout << " <value>";
      else if ((*i).t == optional) std::cout << " [value]";
      std::cout << " (" << (*i).description << ")\n";
    }
  std::cout.flush();
}

void GetOpt::exchange (char **a, size_t size, char **b)
{
  char **tmp = new char *[size];
  Memory::copy (a, tmp, size * sizeof(char **));
  Memory::copy (b, b + size, (a - b)*sizeof(char **));
  Memory::copy (tmp, b, size * sizeof(char **));
  delete [] tmp;
}
