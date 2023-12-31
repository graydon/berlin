/*$Id: ImageImpl.hh,v 1.1 1999/07/15 14:03:02 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999 Stefan Seefeld <seefelds@MAGELLAN.UMontreal.CA>
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

#ifndef _ImageImpl_hh
#define _ImageImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Image.hh"
#include "Warsaw/Raster.hh"
#include "Berlin/GraphicImpl.hh"

class ImageImpl : implements(Image), public virtual GraphicImpl
{
public:
  ImageImpl(Raster_ptr);
  ~ImageImpl();
  
  virtual Raster_ptr data() { return Raster::_duplicate(raster);}
  virtual void data(Raster_ptr r) { raster = r;}

  virtual void request(Requisition &);
  void draw(DrawTraversal_ptr); 
private:
  Raster_var raster;
  Coord width, height;
};

#endif
