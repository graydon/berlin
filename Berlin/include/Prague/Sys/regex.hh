/*$Id: regex.hh,v 1.6 2001/03/21 06:28:23 stefan Exp $
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
#ifndef _regex_hh
#define _regex_hh

#include <string>
#include <sys/types.h>
#include <regex.h>

namespace Prague
{

//. rxmatch represents matches of a regular expression search
class rxmatch
{
public:
  typedef regmatch_t *iterator;
  rxmatch() : _n(0), _r(0) {}
  ~rxmatch() { delete [] _r;}
  size_t size() const { return _n;}
  iterator begin() const { return _r;}
  iterator end() const { return _r + _n;}
  std::string substr(iterator i) const { return _s.substr(i->rm_so, i->rm_eo - i->rm_so);}
  operator bool () const { return _n;}
  friend class regex;
private:
  std::string _s;
  size_t      _n;
  regmatch_t *_r;
  rxmatch(const std::string &s, int p, int n, regmatch_t *r) : _s(s), _n(n), _r(r)
    {
      if (p == -1) _n = 0;
      for (size_t i = 0; i != n; ++i)
	if (r[i].rm_so == -1) { _n = i; break;}
	else r[i].rm_so += p, r[i].rm_eo += p;
    }
};

class regex
{
  struct rx_t
  {
    rx_t(bool e) : rx(0), count(1), extended(e) {}
    ~rx_t() { if (rx) regfree(rx); delete rx;}
    regex_t *rx;
    short count;
    bool extended;
  };
public:
  enum syntax { extended = REG_EXTENDED, basic};
  regex(int e = extended) : _info(new rx_t(e & extended)) {}
  regex(const std::string &, int = extended);
  regex(const regex &);
  regex &operator = (const regex &);
  regex &operator = (const std::string &);
  ~regex();
  std::string::size_type match(const std::string &, int = 0) const;
  rxmatch search(const std::string &, int = 0) const;
  bool OK() const { return _info && _info->rx;}
private:
  std::string error(int);
  rx_t *_info;
};

extern const regex rxwhite;          // = "[ \n\t\r\v\f]+"
extern const regex rxint;            // = "-?[0-9]+"
extern const regex rxdouble;         // = "-?(([0-9]+.[0-9]*)|
                                     //    ([0-9]+)|(.[0-9]+))
                                     //    ([eE][---+]?[0-9]+)?"
extern const regex rxalpha;          // = "[A-Za-z]+"
extern const regex rxlowercase;      // = "[a-z]+"
extern const regex rxuppercase;      // = "[A-Z]+"
extern const regex rxalphanum;       // = "[0-9A-Za-z]+"
extern const regex rxidentifier;     // = "[A-Za-z_][A-Za-z0-9_]*"

};

#endif /* _regex_hh */
