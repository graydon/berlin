/*$Id: Face.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
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
#ifndef _FT_Face_hh
#define _FT_Face_hh

#include <Drawing/FT/Engine.hh>

namespace FT
{

class Face : public TT_Face
{
public:
  class Properties;
  Face(const char *);
  ~Face() { TT_Close_Face(*this);}
  Properties properties() const;
  TT_UShort glyphIndex(TT_UShort i) const { return TT_Char_Index(table, i);}
private:
  TT_CharMap table;
};

class Face::Properties : public TT_Face_Properties
{
  friend class Face;
public:
  unsigned short glyphs() const { return num_Glyphs;}
  unsigned short maxPoints() const { return max_Points;}
  unsigned short maxContours() const { return max_Contours;}
  unsigned short charMaps() const { return num_CharMaps;}
  unsigned short names() const { return num_Names;}
  unsigned short faces() const { return num_Faces;}
private:
  Properties() {}
};

};

#endif /* _FT_Face_hh */
