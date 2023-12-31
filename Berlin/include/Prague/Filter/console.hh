/*+P
 * This file is part of OffiX,
 * a C++ API for the X Window System and Unix
 * Copyright (C) 1995-98  Stefan Seefeld
 *
 * this code is based on binio from Dietmar Kuehl:
 *
 * Copyright (C) 1996 Dietmar Kuehl
 * Universitaet Konstanz, Lehrstuhl fuer praktische Informatik I
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
 -P*/
/* $Id: console.hh,v 1.1 1999/04/22 14:44:58 gray Exp $ */
#ifndef _console_h
#define _console_h

#include <fstream.h>

/* @Class{console : public ofstream}
 *
 * @Description{console redirection}
 */
class console : public ofstream
{
public:
  enum type {cout = 0x1, cerr = 0x2};
  console(unsigned short t, const string &filename)
    {
      ofstream out(filename);
      if (!out) cerr << "console::console: couldn't open " << filename << " for write" << endl;
      else
	{
	  if (t & console::cerr) ::cerr.rdbuf(out.rdbuf());
	  if (t & console::cout) ::cout.rdbuf(out.rdbuf());
	}
    };
};

#endif /* _console_h */
