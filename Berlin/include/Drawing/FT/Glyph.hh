/*$Id: Glyph.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
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
#ifndef _FT_Glyph_hh
#define _FT_Glyph_hh

#include <Drawing/FT/Instance.hh>

namespace FT
{

class Glyph : public TT_Glyph
{
public:
  typedef TT_Glyph_Metrics Metrics;
  Glyph(const Instance &, unsigned short) throw (exception);
  ~Glyph() { TT_Done_Glyph(*this);}
  unsigned short code() const { return c;}
  const Instance &instance() { return i;}
  const Metrics &metrics() const { return m;}
protected:
  const Instance &i;
  unsigned short  c;
  Metrics         m;
};

};

#endif /* _FT_Glyph_hh */
