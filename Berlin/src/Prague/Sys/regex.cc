/*$Id: regex.cc,v 1.5 2001/03/21 06:28:55 stefan Exp $
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

#include "Prague/Sys/regex.hh"

using namespace Prague;

std::string regex::error(int errcode)
{
  if (errcode == 0) return std::string();
  size_t length = regerror(errcode, _info->rx, 0, 0);
  char *buffer = new char[length];
  regerror(errcode, _info->rx, buffer, length);
  std::string text = buffer;
  delete[] buffer;
  return text;
}

regex::regex(const std::string &p, int flags)
  : _info(new rx_t(flags & extended))
{
  _info->rx = new regex_t;
  int errcode = regcomp(_info->rx, p.c_str(), _info->extended ? extended : 0);
  if (errcode) { delete _info->rx; _info->rx = 0;}
}

regex::regex(const regex &r) : _info(r._info)
{
  _info->count++;
}

regex &regex::operator = (const regex &r)
{
  if (!--_info->count) delete _info;
  _info = r._info;
  _info->count++;
  return *this;
}

regex &regex::operator = (const std::string &p)
{
  if (_info->count != 1)
    {
      _info->count--;
      _info = new rx_t(_info->extended);
    }
  _info->rx = new regex_t;
  int errcode = regcomp(_info->rx, p.c_str(), extended);
  if (errcode) { delete _info->rx; _info->rx = 0;}
  return *this;
}

regex::~regex()
{
  if (!--_info->count) delete _info;
}

rxmatch regex::search(const std::string &s, int i) const
{
  int n = _info->rx->re_nsub + 1;
  regmatch_t *rm = new regmatch_t [n];
  if (i >= 0)
    while (s[i] != '\0')
      {
 	if (regexec(_info->rx, s.c_str() + i, n, rm, i ? REG_NOTBOL : 0) == 0) return rxmatch(s, i, n, rm);
	i++;
      }
  else
    {
      i = s.length() - 1;
      do
        {
	  if (regexec(_info->rx, s.c_str() + i, n, rm, i ? REG_NOTBOL : 0) == 0) return rxmatch(s, i, n, rm);
          i--;
        }
      while (i >= 0);
    }
  return rxmatch(s, -1, n, rm);
}

std::string::size_type regex::match(const std::string &s, int i) const
{
  std::string substr;
  if (i < 0) i += s.length();
  if (i > (int) s.length()) return std::string::npos;
  regmatch_t rm;
  int errcode = regexec(_info->rx, s.c_str() + i, 1, &rm, 0);
  if (errcode == 0 && rm.rm_so >= 0) return rm.rm_eo - rm.rm_so;
  return std::string::npos;
}

const regex rxwhite("[ \n\t\r\v\f]+");
const regex rxint("-?[0-9]+");
const regex rxdouble("-?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))"
		     "([eE][---+]?[0-9]+)?");
const regex rxalpha("[A-Za-z]+");
const regex rxlowercase("[a-z]+");
const regex rxuppercase("[A-Z]+");
const regex rxalphanum("[0-9A-Za-z]+");
const regex rxidentifier("[A-Za-z_$][A-Za-z0-9_$]*");
