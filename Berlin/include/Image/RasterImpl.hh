/*$Id: RasterImpl.hh,v 1.14 1999/10/19 21:07:52 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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

#ifndef _RasterImpl_hh
#define _RasterImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <Image/PNG.hh>

class RasterImpl : implements(Raster)
{  	
public:
  RasterImpl();
  RasterImpl(const char* file);
  virtual ~RasterImpl();
  virtual Info header();
  virtual void clear();
  virtual void loadData(const Raster::Data &);
  virtual void storeData(Raster::Data *&);
  virtual void loadPixel(const Index &, const Color &);
  virtual void storePixel(const Index &, Color &);
  virtual void loadPixels(const Index &, const Index &, const Raster::ColorSeq &);
  virtual void storePixels(const Index &, const Index &, Raster::ColorSeq *&);
  void write(const char *);
 private:
  PNG png;
  unsigned char **rows;
};

#endif
