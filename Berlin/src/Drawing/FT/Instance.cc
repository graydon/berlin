/*$Id: Instance.cc,v 1.1 1999/12/13 21:14:34 stefan Exp $
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

#include "Drawing/FT/Instance.hh"

using namespace FT;

int Instance::height() const
{
  TT_Instance_Metrics m;
  TT_Error err = TT_Get_Instance_Metrics(*this, &m);
  if(err) return 0;
  return (m.pointSize * m.y_resolution) / (72*64);
}

// Contributed by Karl Anders Oygard <Karl.Oygard@fou.telenor.no>
int Instance::descender() const
{
  TT_Instance_Metrics metrics;
  TT_Face_Properties properties;
  TT_Error err;

  err= TT_Get_Face_Properties(f, &properties);
  if(err) return 0;
  err= TT_Get_Instance_Metrics(*this, &metrics);
  if(err) return 0;
  return (properties.horizontal->Descender * metrics.y_ppem) / properties.header->Units_Per_EM;
}
