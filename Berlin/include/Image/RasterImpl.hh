/*$Id: RasterImpl.hh,v 1.18 2000/09/19 21:11:04 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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

#ifndef _RasterImpl_hh
#define _RasterImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <Berlin/SubjectImpl.hh>
#include <Image/PNG.hh>

class RasterImpl : public virtual POA_Warsaw::Raster,
		   public SubjectImpl
						  
{  	
public:
  RasterImpl();
  RasterImpl(const char* file);
  virtual ~RasterImpl();
  virtual Warsaw::Raster::Info header();
  virtual void clear();
  virtual void load_data(const Warsaw::Raster::Data &);
  virtual void store_data(Warsaw::Raster::Data_out);
  virtual void load_pixel(const Warsaw::Raster::Index &, const Warsaw::Color &);
  virtual void store_pixel(const Warsaw::Raster::Index &, Warsaw::Color &);
  virtual void load_pixels(const Warsaw::Raster::Index &, const Warsaw::Raster::Index &, const Warsaw::Raster::ColorSeq &);
  virtual void store_pixels(const Warsaw::Raster::Index &, const Warsaw::Raster::Index &, Warsaw::Raster::ColorSeq_out);
  void write(const char *);
 private:
  PNG _png;
  unsigned char **_rows;
};

#endif
