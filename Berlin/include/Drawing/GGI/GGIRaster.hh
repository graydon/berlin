/*$Id: GGIRaster.hh,v 1.2 2000/01/16 07:58:36 graydon Exp $
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
#ifndef _GGIRaster_hh
#define _GGIRaster_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <Warsaw/Transform.hh>
#include <vector>
extern "C" {
#include <ggi/ggi.h>
}


/** !!FIXME!! jan 7 2000 this needs finishing off by someone who knows PNG
    -graydon */

class GGIRaster
{
public:
  GGIRaster(Raster_var);
  ~GGIRaster();
  void draw() {};
};

#endif /* _GGIRaster_hh */
