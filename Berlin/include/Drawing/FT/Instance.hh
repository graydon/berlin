/*$Id: Instance.hh,v 1.1 1999/12/13 21:12:55 stefan Exp $
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
#ifndef _FT_Instance_hh
#define _FT_Instance_hh

#include "Drawing/FT/Face.hh"

namespace FT
{

class Instance : public TT_Instance
{
public:
  typedef TT_Instance_Metrics Metrics;
  Instance(const Face &ff) : f(ff) { if(TT_New_Instance(f, this)) throw exception();}
  ~Instance() { TT_Done_Instance(*this);}
  const Face &face() const { return f;}
  bool setResolutions(int x, int y) { return TT_Set_Instance_Resolutions(*this, x, y) == 0;}
  bool setPointSize(int s) { return TT_Set_Instance_PointSize(*this, s) == 0;}
  Metrics metrics() const { TT_Instance_Metrics m; TT_Get_Instance_Metrics(*this, &m); return m;}
  int height() const;
  int descender() const;
private:
  const Face &f;
};

};

#endif /* _FT_Instance_hh */
